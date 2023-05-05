geographies_census_data <-
  map(
    .x = geographies, # Into .geography
    .f = get_acs_vars,
    .state = state,
    .county = counties,
    .variables = acs_vars,
    .year = census_data_year,
    .survey = census_survey,
    .output = "wide",
    .geometry = TRUE
  ) |>
  set_names(geographies)

census_tract_raw_data <-
    get_acs(
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
      rcbE = rcb1E+rcb2E+rcb3E+rcb4E+rcb5E,
      AreaTract = as.numeric(st_area(.))
    )

custom_geographies <-
  map(.x = custom_geographies, .f =  ~ sym(.x) |> eval()) |>
  set_names(custom_geographies)

#### area-weighted interpolation of census tract data to custom geographies
custom_geographies_census_data <-
  map(
    .x = custom_geographies,
    .f = custom_geo_interpolate_aw,
    source = census_tract_raw_data,
    .keep = 0.2
  )

all_geographies <- append(geographies_census_data, custom_geographies_census_data)

#### Export demographic data as geojson #####
filenames <- names(all_geographies) |> str_replace("zcta", "zip")
map2(
  .x = all_geographies, # into data
  .y = filenames, # into filenames
  .f = write_geojson,
  folder = "demo",
  filename_prefix = "tulsa_demographics_",
  .delete_dsn = TRUE
)

#### Generate points with population data #####
bubbles <- map(.x = all_geographies, .f = generate_bubbles)

#### Export population points to geojson #####
map2(
  .x = bubbles, # into data
  .y = filenames, # into filenames
  .f = write_geojson,
  folder = "bubble",
  filename_prefix = "tulsa_bubble_",
  .delete_dsn = TRUE
)
