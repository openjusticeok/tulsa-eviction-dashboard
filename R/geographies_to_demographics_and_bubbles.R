#### Load needed libraries #####
library(tidyverse)
library(tidycensus)
library(tigris)
library(purrr)
library(here)
library(sf)
library(rmapshaper)

## Load Helper Functions
source(here("R/geographies_to_demograhics_and_bubbles_helpers.R"))

### Set Cache Option
options(tigris_use_cache = TRUE)

###### NOTE: USER IS EXPECTED TO PRE-PROCESS SHAPEFILES
  # Including:
    # Check whether new custom shapefile is geometrically valid: st_make_valid()
    # Including only variables for 'id', 'name', and geometry.
    # Setting crs to 4269 with st_transform().

#### Requested Custom Geographies
## City Council Districts
city_council_districts <-
  st_read(here("data/shapefiles/council_districts/Council_Districts.shp")) |>
  select(id = DISTRICTID, geometry) |>
  mutate(name = paste("Council District", id)) |>
  st_transform(crs = 4269) |>
  st_make_valid()
## Oklahoma Public School Districts Shapefiles
school_districts <-
  st_read(here("data/shapefiles/school_districts/School_Districts.shp")) |>
  select(id = SD_CODE, name = SD_NAME, geometry) |>
  st_transform(crs = 4269) |>
  st_make_valid()
## Judicial Districts
judicial_districts <-
  st_read(here("data/shapefiles/judicial_districts/Judicial_Districts.shp")) |>
  mutate(id = DISTRICT, name = paste("Judicial District", DISTRICT)) |>
  select(id, name) |>
  st_make_valid() |>
  st_transform(crs = 4269)

#### Not-requested Custom Geographies (just in case)
## Federal Legislative Districts
federal_house_districts <-
  congressional_districts(state = "OK") |>
  select(id = GEOID, name = NAMELSAD)
## State Legislative Districts
state_senate_districts <-
  state_legislative_districts(state = "OK", house = "upper") |>
  select(id = GEOID, name = NAMELSAD)
state_house_districts <-
  state_legislative_districts(state = "OK", house = "lower") |>
  select(id = GEOID, name = NAMELSAD)
## Voting Precincts
voting_precincts <-
  voting_districts(state = "OK") |>
  mutate(id = GEOID20, name = paste("Voting Precinct", NAMELSAD20)) |>
  select(id, name)
## Tribal Lands
tribalLands <-
  st_read(here("data/shapefiles/tribal_boundaries/Tribal_Boundaries.shp")) |>
  st_transform(crs = 4269) |>
  st_make_valid() |>
  filter(!is.na(TRIBAL_NAM)) |> # Filter for only federally recognized tribal areas.
  mutate(tribal = TRIBAL_ARE |> str_to_title() |>  tools::toTitleCase()) |>
  mutate(tribal = gsub("Indain", "Indian", tribal)) |>
  select(name = tribal, id = TRIBAL_UTM, tribal_nation = TRIBAL_NAM)

custom_geographies <- c(
  "city_council_districts", "school_districts", "judicial_districts",
  "federal_house_districts", "state_senate_districts", "state_house_districts",
  "voting_precincts"
  )

#### Main Function
geographies_to_demographics_and_bubbles(
  .state = "OK",
  .counties = "tulsa",
  .census_data_year = 2019,
  .census_survey = "acs5",
  .custom_geographies = custom_geographies,
  .filename_prefix = "tulsa"
)
