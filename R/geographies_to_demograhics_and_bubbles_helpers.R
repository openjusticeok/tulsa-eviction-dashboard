#### Helper Functions:
format_county_names <- function(x) {
  "Formats county names for use in get_acs() and other functions.
  E.g., c('tulsa', 'OKLAHOMA County') -> c('Tulsa County', 'Oklahoma County')"

  x <- stringr::str_to_title(x)

  x <-
    x |>
    purrr::map_chr(function(x) {
      if (!stringr::str_detect(x, "County$")) {
        x <- paste(x, "County")
      }
      return(x)
    })

  return(x)
}

get_acs_vars <- function(.geography, .state, .counties,
                         .variables, .year, .survey,
                         .output, .geometry, .keep = 0.2) {

  "Gets Census ACS variables and Construct Output Variables by geography."

  if (.geography %in% c("place", "zcta")) {
    counties_geo <-
      tigris::counties(.state) %>%
      filter(NAMELSAD %in% .counties)

    .counties <- NULL
  }

  census_data <-
    get_acs(
      geography = .geography,
      state = .state,
      county = .counties,
      variables = .variables,
      year = .year,
      survey = .survey,
      output = .output,
      geometry = .geometry
    )

  if (.geography %in% c("place", "zcta")) {
    census_data <-
      census_data %>%
      .[counties_geo, ]
  }


  census_data <-
    census_data |>
    mutate(
      id = GEOID,
      name = NAME,
      pop = tot_popE, #Total Population
      pvr = pop_bpE/tot_popE, # % in Poverty
      cpr = bp_u18E/pop_u18E, # % are Children
      prh = rohhE/thhE, # % Renter Occupied Households
      rhh = rohhE, # Number Renter Occupied Households
      mgr = med_rentE, # Median Rent
      mpv = med_valE, # Median value of owner-occupied housing units
      mhi = med_incE, # Median Income
      rb = (rcb1E+rcb2E+rcb3E+rcb4E+rcb5E)/rohhE, # % of Renters that are Rent Burdened
      pca = as_popE/tot_popE, # % Asian
      pcb = bl_popE/tot_popE, # % Black
      pcw = wh_popE/tot_popE, # % White
      pch = his_popE/tot_popE, # % Hispanic
      geometry,
      .keep = "none"
    ) %>%
    ms_simplify(keep = .keep)

  return(census_data)

}

custom_geo_interpolate_aw <- function(source, target, .keep = 0.2) {
  "Area-weighted areal interpolation for a custom geogpraphy (from a user-provided sf)
  of a list of variables from another sf."

  geometries_to_exclude <-
    c("GEOMETRYCOLLECTION", "LINESTRING", "MULTILINESTRING", "POINT")

  custom_geo <-
    source |>
    st_intersection(target) |>
    st_make_valid() |>
    filter(!st_is_empty(geometry)) %>%
    mutate(new_geometry = purrr::map(geometry, ~ {
      if (st_geometry_type(.x) == "GEOMETRYCOLLECTION") {
        poly <- st_collection_extract(.x, "POLYGON")
        if (!is_empty(poly)) {
          return(poly)
        }
      }
      return(.x)
    })) %>%
    st_set_geometry("new_geometry") %>%
    select(-geometry) %>%
    rename(geometry = new_geometry) |>
    filter(!(st_geometry_type(geometry) %in% geometries_to_exclude)) %>%
    st_set_crs(4269) |>
    st_make_valid() %>%
    mutate(
      AreaIntersect = as.numeric(st_area(.)),
      PerIntersect = AreaIntersect/AreaTract,
      pop_intersect = round(PerIntersect*tot_popE, digits = 4),
      popbp_intersect = round(PerIntersect*pop_bpE, digits = 4),
      popu18_intersect = round(PerIntersect*pop_u18E, digits = 4),
      bpu18_intersect = round(PerIntersect*bp_u18E, digits = 4),
      rohh_intersect = round(PerIntersect*rohhE, digits = 4),
      thh_intersect = round(PerIntersect*thhE, digits = 4),
      rcb_intersect = round(PerIntersect*rcbE, digits = 4),
      as_intersect = round(PerIntersect*as_popE, digits = 4),
      bl_intersect = round(PerIntersect*bl_popE, digits = 4),
      wh_intersect = round(PerIntersect*wh_popE, digits = 4),
      his_intersect = round(PerIntersect*his_popE, digits = 4)
    ) |>
    group_by(id, name) |>
    summarise(
      pop = sum(pop_intersect),
      pvr = sum(popbp_intersect)/pop,
      cpr = sum(bpu18_intersect)/sum(popu18_intersect),
      prh = sum(rohh_intersect)/sum(thh_intersect),
      rhh = sum(rohh_intersect),
      mgr = mean(med_rentE, na.rm = TRUE),
      mpv = mean(med_valE, na.rm = TRUE),
      mhi = mean(med_incE, na.rm = TRUE),
      rb = sum(as_intersect)/100,
      pca = sum(as_popE)/pop,
      pcb = sum(bl_intersect)/pop,
      pcw = sum(wh_intersect)/pop,
      pch = sum(his_intersect)/pop
    ) |>
    select(id, name, pop:pch) |>
    ms_simplify(keep = .keep)

  return(custom_geo)
}

write_geojson <- function(data, folder, filename,
                          filename_prefix = NA, .delete_dsn = TRUE) {
  "Writes an sf object to a .geojson in a specified file path."

  if(is.na(filename_prefix)) {
    sf::st_write(
      data,
      here(folder, paste0(filename, ".geojson")),
      delete_dsn = .delete_dsn
    )
  } else {
    sf::st_write(
      data,
      here(folder, paste0(filename_prefix, filename, ".geojson")),
      delete_dsn = .delete_dsn
    )
  }
}

generate_bubbles <- function(data, crs = NA) {
  "Generate bubbles from sf object for with info on the population and
  number of renter occupied households."

  if (is.na(crs)) {
    bubble <-
      data |>
      select(id, name, pop, rhh) |>
      st_point_on_surface()
  } else {
    bubble <-
      data |>
      select(id, name, pop, rhh) |>
      st_transform(crs) |>
      st_point_on_surface()
  }

  return(bubble)
}


## Main Execution Function
geographies_to_demographics_and_bubbles <- function(.state,
                                                    .counties,
                                                    .census_data_year,
                                                    .census_survey,
                                                    .custom_geographies,
                                                    .filename_prefix) {

  .counties <- format_county_names(.counties)

  # Census Geographies
  geographies <- c("county", "tract", "zcta", "place")

  # Census ACS Variables
  acs_vars <- c(
    tot_pop = "B01003_001", #total population
    pop_u18 = "B17006_001", #population under 18
    med_inc = "B19013_001", #median household income
    med_rent = "B25031_001", #median monthly housing costs
    his_pop = "B03002_012", #hispanic population
    wh_pop = "B03002_003", #white population
    bl_pop = "B03002_004", #black population
    as_pop = "B03002_006", #asian population
    rohh = "B25106_024", #renter-occupied households
    thh = "B25106_001", #total households
    pop_bp = "B17020_002", #population below poverty
    bp_u18 = "B17006_002", #population under 18 below poverty
    med_val = "B25097_001", #median value of owner-occupied housing units
    rcb1 = "B25106_028", #gross rent as a percentage of income 30% or more,
    rcb2 = "B25106_032",
    rcb3 = "B25106_036",
    rcb4 = "B25106_040",
    rcb5 = "B25106_044"
  )

  geographies_census_data <<-
    map(
      .x = geographies, # Into .geography
      .f = get_acs_vars,
      .state = .state,
      .counties = .counties,
      .variables = acs_vars,
      .year = .census_data_year,
      .survey = .census_survey,
      .output = "wide",
      .geometry = TRUE
    ) |>
    set_names(geographies)

  census_tract_raw_data <<-
      get_acs(
        geography = "tract",
        state = .state,
        county = .counties,
        variables = acs_vars,
        year = .census_data_year,
        survey = .census_survey,
        output = "wide",
        geometry = TRUE
      ) %>%
      mutate(
        rcbE = rcb1E+rcb2E+rcb3E+rcb4E+rcb5E,
        AreaTract = as.numeric(st_area(.))
      )

  custom_geographies <<-
    map(.x = .custom_geographies, .f =  ~ sym(.x) |> eval()) |>
    set_names(.custom_geographies)

  #### area-weighted interpolation of census tract data to custom geographies
  custom_geographies_census_data <-
    map(
      .x = custom_geographies,
      .f = custom_geo_interpolate_aw,
      source = census_tract_raw_data,
      .keep = 0.2
    )

  all_geographies <<- append(geographies_census_data, custom_geographies_census_data)

  #### Export demographic data as geojson #####
  filenames <- names(all_geographies) |> str_replace("zcta", "zip")
  map2(
    .x = all_geographies, # into data
    .y = filenames, # into filenames
    .f = write_geojson,
    folder = "demo",
    filename_prefix = paste0(.filename_prefix, "_demographics_"),
    .delete_dsn = TRUE
  )

  #### Generate points with population data #####
  bubbles <<- map(.x = all_geographies, .f = generate_bubbles)

  #### Export population points to geojson #####
  map2(
    .x = bubbles, # into data
    .y = filenames, # into filenames
    .f = write_geojson,
    folder = "bubble",
    filename_prefix = paste0(.filename_prefix, "_bubble_"),
    .delete_dsn = TRUE
  )
}
