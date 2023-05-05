## Load Packages
library(dplyr) # Data wrangling/cleaning
library(tidyr) # Data wrangling
library(stringr) # String manipulation
library(readr) # For reading/writing data
library(ggplot2) # Data visualization
library(forcats) # Working with factors
library(lubridate) # Dealing with dates
library(here) # For readable file paths
library(tidycensus) # Loading Census data
library(sf) # Spatial Manipulation
library(tigris) # Load Census TIGER/Line Shapefiles
library(sp)
library(mapview)
library(leaflet)
library(ojodb) # Working with OJO data


## Load locationData
locationData <- ojo_tbl("address", schema = "eviction_addresses") |>
  select(case, lat, lon, geo_accuracy) |>
  collect()

## Check histogram of geo_accuracy to determine reasonable cutoff
locationData |>
  ggplot(aes(x = geo_accuracy)) +
    geom_histogram() +
    geom_vline(xintercept = 0.85)

## Load Tulsa County Eviction Case Data
caseData <- read_csv(here("data/tulsaEvictionData.csv"))

## Left join locationData to caseData
data <- caseData |>
  left_join(locationData, by = c("id" = "case"))

## Get Number and Percent for cases that have don't have an address or have a bad accuracy score
# 4/17/2023; 23% of cases either have missing addresses or bad location accuracy.
data |>
  summarise(
    count_NA = sum(is.na(lat)),
    percent_NA = sum(is.na(lat)) / n() * 100,
    count_badAccuracy = sum(geo_accuracy < 0.85, na.rm = TRUE),
    percent_badAccuracy = sum(geo_accuracy < 0.85, na.rm = TRUE) / n() * 100,
  ) |>
  mutate(
    count_combined = count_NA + count_badAccuracy,
    percent_combined = percent_NA + percent_badAccuracy
  )

## Filter data for those not missing location data and for accuracy >=.85
data <- data |>
  filter(
    !is.na(lat),
    geo_accuracy >= 0.85
  )

#### Load and Manipulate Shapefiles
## Tulsa County
county <-
  counties(state = "OK") |>
  filter(GEOID == "40143")

## Cities/Towns
citiesAndTowns <- places(state = "OK")

## ZIP Codes
zipCodes <- zctas(state = "OK", year = 2010) # Not the latest 2020 ZCTAs from what I've seen (https://www.census.gov/geographies/mapping-files/2020/geo/tiger-line-file.html).

## Census Tracts
censusTracts <- tracts(state = "OK")

## Federal Legislative Districts
federalHouse <- congressional_districts(state = "OK")

## State Legislative Districts
stateSenate <- state_legislative_districts(state = "OK", house = "upper")
stateHouse <- state_legislative_districts(state = "OK", house = "lower")

## Voting Precincts
votingPrecincts <- voting_districts(state = "OK")

##City Council Districts
cityCouncilDistricts <- st_read(here("data/shapefiles/council_districts/Council_Districts.shp")) |>
  st_transform(crs = 4269) |>
  st_make_valid()

## Judicial Districts
judicialDistricts <- st_read(here("data/shapefiles/judicial_districts/Judicial_Districts.shp")) |>
  st_transform(crs = 4269) |>
  st_make_valid()

## Oklahoma Public School Districts Shapefiles
# schoolDistricts <- school_districts(state = "OK", year = 2018) # This is missing 1 county we need :(
schoolDistricts <- st_read(here("data/shapefiles/school_districts/School_Districts.shp")) |>
  st_transform(crs = 4269) |>
  st_make_valid()

## Tribal Lands
tribalLands <- st_read(here("data/shapefiles/tribal_boundaries/Tribal_Boundaries.shp")) |>
  st_transform(crs = 4269) |>
  st_make_valid()
# Filter for only federally recognized tribal areas.
tribalLands <- tribalLands |>
  filter(!is.na(TRIBAL_NAM))

# Filter to the 14 districts that at least partially fall into Tulsa County
schoolDistricts <- schoolDistricts |>
  filter(COUNTY == "Tulsa")

# Convert eviction data to sf object
data_sf <- st_as_sf(
    data,
    coords = c("lon", "lat"),
    crs = 4269,
    agr = "constant",
    remove = FALSE # Keep lat and lon
  )

# Provide additional geographic filter to ensure data are within Tulsa County
# Number of cases to be excluded with filter
st_disjoint(data_sf, county, sparse = FALSE) |>
  sum()

# Filter
data_sf <-
  data_sf |>
  filter(st_contains(county, data_sf, sparse = FALSE) |> as.logical())


## Create variables containing each cases location within every geographic level
data_sf <-
  data_sf |>
  select(case_number = id, date = date_filed, lat, lon) |>
  ## Tulsa County
  st_join(
    county |>
      select(county = NAMELSAD, county_id = GEOID)
  ) |>
  # Voting Precincts
  st_join(
    votingPrecincts |>
      select(
        precinct = NAMELSAD20, precinct_id = GEOID20
      )
  ) |>
  ## Census Tracts
  st_join(
    censusTracts |>
      select(
        tract = NAMELSAD, tract_id = GEOID
      )
  ) |>
  ## ZIP Codes
  st_join(
    zipCodes |>
      select(
        zip_id = ZCTA5CE10
      )
  ) |>
  ## Cities/Towns
  st_join(
    citiesAndTowns |>
      mutate(municipal_designation = str_extract(NAMELSAD, "city|town")) |>
      select(
        city = NAME, municipal_designation, city_id = GEOID
      )
  ) |>
  # City Council District
  st_join(
    cityCouncilDistricts |>
      mutate(council = paste("Tulsa City Council", NAME)) |>
      select(
        council, council_id = DISTRICTID
      )
  ) |>
  # Tulsa County Public School Districts
  st_join(
    schoolDistricts |>
      select(school = SD_NAME, school_id = SD_CODE)
  ) |>
  # Federal Legislative District
  st_join(
    federalHouse |>
      select(
        federal_house = NAMELSAD, federal_house_id = GEOID
      )
  ) |>
  # State Legislative Districts (House and Senate)
  st_join(
    stateHouse |>
      select(
        state_house = NAMELSAD, state_house_id = GEOID
      )
  ) |>
  st_join(
    stateSenate |>
      select(
        state_senate = NAMELSAD, state_senate_id = GEOID
      )
  ) |>
  # Judicial Precincts
  st_join(
    judicialDistricts |>
      mutate(judicial =  paste("Judicial District", DISTRICT)) |>
      select(judicial, judicial_id = DISTRICT)
  ) |>
  # Tribal Lands
  st_join(
    tribalLands |>
      mutate(tribal = TRIBAL_ARE |> str_to_title() |>  tools::toTitleCase()) |>
      mutate(tribal = gsub("Indain", "Indian", tribal)) |>
      select(tribal_nation = TRIBAL_NAM, tribal,
             tribal_id = TRIBAL_UTM)
  )

### Mapping Example of 5000 Addresses
# Coordinates to sf
mapview(
  votingPrecincts |> filter(COUNTYFP20 == "143"),
  color = "purple4", lwd = 2,
  alpha = 1, alpha.regions = 0.4,
  highlight = leaflet::highlightOptions(
    color = "red", fill = "red", fillOpacity = .6, opacity = .6,
    weight = 5, bringToFront = FALSE
  )
) +
mapview(
  county,
  color = "black", col.regions = "#000000",
  alpha = 0.8, alpha.regions = 0.05
) +
mapview(
  data_sf[1:5000,], col.regions = "cyan"
)

write_csv(data_sf, here("data/tulsa_eviction_cases.csv"))

data_sf <-
  data_sf |>
  mutate(year = year(date), month = month(date))

id_columns <-
  data_sf |>
  as_tibble() |>
  select(-geometry) |>
  select(ends_with("id")) |>
  names() |>
  gsub(pattern = "city_id", replacement = "city")


pivot_geography_longer <- function(data, id_column) {

  geography <-
    gsub("(.*)_id", "\\1", id_column) |>
    gsub(pattern = "_", replacement = " ") |>
    str_to_title()

  data |>
    filter(!is.na({{id_column}})) |>
    summarise(tot_evic = n(), .by = c({{id_column}}, year, month)) |>
    rename(NAME = !!enquo(id_column)) |>
    mutate(NAME = as.character(NAME)) |>
    mutate(Geography = geography) |>
    arrange(NAME, year, month) |>
    relocate(NAME, Geography)
}

data_sf_longer <-
  map(.x = id_columns, .f = pivot_geography_longer, data = data_sf) |>
  bind_rows() |>
  as_tibble() |>
  select(-geometry)

write_csv(data_sf_longer, here("data/tulsa_datadownload.csv"))
