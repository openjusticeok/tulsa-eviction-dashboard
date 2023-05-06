
#' @title Format County Names
#'
#' @description Formats county names for use in get_acs() and other functions.
#'
#' @param x A character vector of county names.
#'
#' @return A character vector of formatted county names.
#'
#' @importFrom stringr str_to_title str_detect
#' @importFrom purrr map_chr
#'
#' @export
#'
#' @examples
#' format_county_names(c("tulsa", "OKLAHOMA County"))
#'
format_county_names <- function(x) {
  x <- x |>
    stringr::str_to_title() |>
    purrr::map_chr(function(x) {
      if (!stringr::str_detect(x, "County$")) {
        x <- paste(x, "County")
      }
      return(x)
    })

  return(x)
}

#' @title Get ACS Variables
#'
#' @description Gets Census ACS Variables and Construct Output Variables by Geography.
#'
#' @param geography A character vector of Census geographies.
#' @param state A character vector of state FIPS codes.
#' @param counties A character vector of county names.
#' @param variables A character vector of Census variables.
#' @param year A character vector of Census years.
#' @param survey A character vector of Census surveys.
#' @param output A character vector of Census output types.
#' @param geometry A character vector of Census geometry types.
#' @param ... Placeholder for future arguments.
#' @param .keep A numeric value of the simplification tolerance.
#'
#' @return A tibble of Census ACS variables.
#'
#' @importFrom dplyr filter mutate
#'
get_acs_vars <- function(
    geography,
    state,
    counties,
    variables,
    year,
    survey,
    output,
    geometry,
    ...,
    .keep = 0.2
) {
  if (geography %in% c("place", "zcta")) {
    counties_geo <- tigris::counties(state) |>
      dplyr::filter(NAMELSAD %in% counties)

    counties <- NULL
  }

  census_data <- tidycensus::get_acs(
    geography = geography,
    state = state,
    county = counties,
    variables = variables,
    year = year,
    survey = survey,
    output = output,
    geometry = geometry
  )

  if (geography %in% c("place", "zcta")) {
    census_data <- census_data %>%
      .[counties_geo, ]
  }

  census_data <- census_data |>
    dplyr::mutate(
      id = GEOID,
      name = NAME,
      pop = tot_popE, # Total Population
      pvr = pop_bpE / tot_popE, # % in Poverty
      cpr = bp_u18E / pop_u18E, # % are Children
      prh = rohhE / thhE, # % Renter Occupied Households
      rhh = rohhE, # Number Renter Occupied Households
      mgr = med_rentE, # Median Rent
      mpv = med_valE, # Median value of owner-occupied housing units
      mhi = med_incE, # Median Income
      rb = (rcb1E + rcb2E + rcb3E + rcb4E + rcb5E) / rohhE, # % of Renters that are Rent Burdened
      pca = as_popE / tot_popE, # % Asian
      pcb = bl_popE / tot_popE, # % Black
      pcw = wh_popE / tot_popE, # % White
      pch = his_popE / tot_popE, # % Hispanic
      geometry,
      .keep = "none"
    ) |>
    ms_simplify(keep = .keep)

  return(census_data)
}

#' @title Custom Geography Interpolate Area-Weighted
#' 
#' @description Area-weighted areal interpolation for a custom geogpraphy (from a user-provided sf)
#' of a list of variables from another sf.
#' 
#' @param source An sf object of the source geography.
#' @param target An sf object of the target geography.
#' @param ... Placeholder for future arguments.
#' @param .keep A numeric value of the simplification tolerance.
#' 
#' @return An sf object of the interpolated variables.
#' 
#' @importFrom dplyr filter mutate rename select
#' @importFrom sf st_area st_intersection st_make_valid st_set_crs st_set_geometry
#' @importFrom units set_units
#' @importFrom purrr map
#' 
#' @export
#'
custom_geo_interpolate_aw <- function(source, target, ..., .keep = 0.2) {
  geometries_to_exclude <- c(
    "GEOMETRYCOLLECTION",
    "LINESTRING",
    "MULTILINESTRING",
    "POINT"
  )

  custom_geo <- source |>
    st_intersection(target) |>
    st_make_valid() |>
    filter(!st_is_empty(geometry)) |>
    mutate(
      new_geometry = purrr::map(
        geometry,
        function(x) {
          if (st_geometry_type(x) == "GEOMETRYCOLLECTION") {
            poly <- st_collection_extract(x, "POLYGON")
            if (!is_empty(poly)) {
              return(poly)
            }
          }
          return(x)
        }
      )
    ) |>
    st_set_geometry("new_geometry") |>
    select(-geometry) |>
    rename(geometry = new_geometry) |>
    filter(!st_geometry_type(geometry) %in% geometries_to_exclude) |>
    st_set_crs(4269) |>
    st_make_valid() %>%
    mutate(
      AreaIntersect = as.numeric(st_area(.)),
      PerIntersect = AreaIntersect / AreaTract,
      pop_intersect = round(PerIntersect * tot_popE, digits = 4),
      popbp_intersect = round(PerIntersect * pop_bpE, digits = 4),
      popu18_intersect = round(PerIntersect * pop_u18E, digits = 4),
      bpu18_intersect = round(PerIntersect * bp_u18E, digits = 4),
      rohh_intersect = round(PerIntersect * rohhE, digits = 4),
      thh_intersect = round(PerIntersect * thhE, digits = 4),
      rcb_intersect = round(PerIntersect * rcbE, digits = 4),
      as_intersect = round(PerIntersect * as_popE, digits = 4),
      bl_intersect = round(PerIntersect * bl_popE, digits = 4),
      wh_intersect = round(PerIntersect * wh_popE, digits = 4),
      his_intersect = round(PerIntersect * his_popE, digits = 4)
    ) |>
    group_by(id, name) |>
    summarise(
      pop = sum(pop_intersect),
      pvr = sum(popbp_intersect) / pop,
      cpr = sum(bpu18_intersect) / sum(popu18_intersect),
      prh = sum(rohh_intersect) / sum(thh_intersect),
      rhh = sum(rohh_intersect),
      mgr = mean(med_rentE, na.rm = TRUE),
      mpv = mean(med_valE, na.rm = TRUE),
      mhi = mean(med_incE, na.rm = TRUE),
      rb = sum(as_intersect) / 100,
      pca = sum(as_popE) / pop,
      pcb = sum(bl_intersect) / pop,
      pcw = sum(wh_intersect) / pop,
      pch = sum(his_intersect) / pop
    ) |>
    select(id, name, pop:pch) |>
    ms_simplify(keep = .keep)

  return(custom_geo)
}

#' @title Write GeoJSON
#' 
#' @description Writes an sf object to a .geojson in a specified file path.
#' 
#' @param data An sf object.
#' @param folder A string of the folder name.
#' @param filename A string of the file name.
#' @param ... Placeholder for future arguments.
#' @param .prefix A string of the file name prefix.
#' @param .delete_dsn A logical value of whether to delete the dsn.
#' 
#' @return A .geojson file.
#' 
#' @importFrom here here
#' @importFrom sf st_write
#' 
#' @export
#' 
write_geojson <- function(
  data,
  folder,
  filename,
  ...,
  .prefix = NA,
  .delete_dsn = TRUE
) {
  "Writes an sf object to a .geojson in a specified file path."

  if (is.na(.prefix)) {
    sf::st_write(
      data,
      here(folder, paste0(filename, ".geojson")),
      delete_dsn = .delete_dsn
    )
  } else {
    sf::st_write(
      data,
      here(folder, paste0(.prefix, filename, ".geojson")),
      delete_dsn = .delete_dsn
    )
  }
}

#' @title Generate Bubbles
#' 
#' @description Generate bubbles from sf object for with info on the population and
#' number of renter occupied households.
#' 
#' @param data An sf object.
#' @param crs A coordinate reference system.
#' 
#' @return An sf object of the bubbles.
#' 
#' @importFrom dplyr select
#' @importFrom sf st_point_on_surface st_transform
#' 
#' @export
#'
generate_bubbles <- function(data, crs = NA) {
  if (is.na(crs)) {
    bubble <- data |>
      select(id, name, pop, rhh) |>
      st_point_on_surface()
  } else {
    bubble <- data |>
      select(id, name, pop, rhh) |>
      st_transform(crs) |>
      st_point_on_surface()
  }

  return(bubble)
}

#' @title Geographies to Demographics and Bubbles
#' 
#' @description Generate demographics and bubbles from geographies.
#' 
#' @param state A string of the state name.
#' @param counties A vector of county names.
#' @param census_data_year A string of the census data year.
#' @param census_survey A string of the census survey.
#' @param custom_geographies A list of custom geographies.
#' @param ... Placeholder for future arguments.
#' @param .prefix A string of the file name prefix.
#' 
#' @return A list of sf objects.
#' 
#' @importFrom dplyr filter mutate rename select
#' @importFrom here here
#' @importFrom sf st_geometry_type st_make_valid st_set_crs st_set_geometry
#' @importFrom tidyr pivot_longer
#' 
#' @export
#'
geographies_to_demographics_and_bubbles <- function(
  state,
  counties,
  census_data_year,
  census_survey,
  custom_geographies,
  ...,
  .prefix
) {
  counties <- format_county_names(counties)

  # Census Geographies
  geographies <- c("county", "tract", "zcta", "place")

  # Census ACS Variables
  acs_vars <- c(
    tot_pop = "B01003_001", # total population
    pop_u18 = "B17006_001", # population under 18
    med_inc = "B19013_001", # median household income
    med_rent = "B25031_001", # median monthly housing costs
    his_pop = "B03002_012", # hispanic population
    wh_pop = "B03002_003", # white population
    bl_pop = "B03002_004", # black population
    as_pop = "B03002_006", # asian population
    rohh = "B25106_024", # renter-occupied households
    thh = "B25106_001", # total households
    pop_bp = "B17020_002", # population below poverty
    bp_u18 = "B17006_002", # population under 18 below poverty
    med_val = "B25097_001", # median value of owner-occupied housing units
    rcb1 = "B25106_028", # gross rent as a percentage of income 30% or more,
    rcb2 = "B25106_032",
    rcb3 = "B25106_036",
    rcb4 = "B25106_040",
    rcb5 = "B25106_044"
  )

  geographies_census_data <<- map(
    .x = geographies, # Into .geography
    .f = get_acs_vars,
    state = state,
    counties = counties,
    variables = acs_vars,
    year = census_data_year,
    survey = census_survey,
    output = "wide",
    geometry = TRUE
  ) |>
    set_names(geographies)

  census_tract_raw_data <<- get_acs(
    geography = "tract",
    state = state,
    county = counties,
    variables = acs_vars,
    year = census_data_year,
    survey = census_survey,
    output = "wide",
    geometry = TRUE
  ) %>%
  mutate(
    rcbE = rcb1E + rcb2E + rcb3E + rcb4E + rcb5E,
    AreaTract = as.numeric(st_area(.))
  )

  custom_geographies <<- map(
    .x = custom_geographies,
    .f = ~ sym(.x) |> eval()
  ) |>
    set_names(custom_geographies)

  #### area-weighted interpolation of census tract data to custom geographies
  custom_geographies_census_data <- map(
    .x = custom_geographies,
    .f = custom_geo_interpolate_aw,
    source = census_tract_raw_data,
    .keep = 0.2
  )

  all_geographies <<- append(
    geographies_census_data,
    custom_geographies_census_data
  )

  #### Export demographic data as geojson #####
  filenames <- names(all_geographies) |>
    str_replace("zcta", "zip")

  map2(
    .x = all_geographies, # into data
    .y = filenames, # into filenames
    .f = write_geojson,
    folder = "demo",
    .prefix = paste0(.filename_prefix, "_demographics_"),
    .delete_dsn = TRUE
  )

  #### Generate points with population data #####
  bubbles <<- map(
    .x = all_geographies,
    .f = generate_bubbles
  )

  #### Export population points to geojson #####
  map2(
    .x = bubbles, # into data
    .y = filenames, # into filenames
    .f = write_geojson,
    folder = "bubble",
    .prefix = paste0(.prefix, "_bubble_"),
    .delete_dsn = TRUE
  )
}
