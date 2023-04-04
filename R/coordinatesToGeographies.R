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
library(ojodb) # Working with OJO data


## Load LocationData
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
# 3/23/2023; 9.6% of cases either have missing addresses or bad location accuracy.
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

## Oklahoma Public School Shapefiles
# schoolDistricts <- school_districts(state = "OK", year = 2018) # This is missing 1 county we need :(
schoolDistricts <- st_read(here("data/shapefiles/school_districts/School_Districts.shp")) |>
  st_transform(crs = 4269) |>
  st_make_valid()

## Tribal Lands
tribalLands <- st_read(here("data/shapefiles/tribal_boundaries/tribal_boundaries.shp")) |>
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
    agr = "constant"
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
  ## Tulsa County
  st_join(
    county |>
      select(county = NAMELSAD, county_id = GEOID)
  ) |>
  ## Cities/Towns
  st_join(
    citiesAndTowns |>
      mutate(municipal_designation = str_extract(NAMELSAD, "city|town")) |>
      select(
        cities_and_towns = NAME, municipal_designation,
        cities_and_towns_id = GEOID
      )
  ) |>
  ## ZIP Codes
  st_join(
    zipCodes |>
      select(
        zip_code_id = ZCTA5CE10
      )
  ) |>
  ## Census Tracts
  st_join(
    censusTracts |>
      select(
        census_tract = NAMELSAD, census_tract_id = GEOID
      )
  ) |>
  # Federal Legislative District
  st_join(
    federalHouse |>
      select(
        federal_house = NAMELSAD, federal_house_id = GEOID
      )
  ) |>
  # State Legislative District
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
  # City Council District
  st_join(
    cityCouncilDistricts |>
      select(
        city_council_districts = NAME, city_council_districts_id = DISTRICTID
      )
  ) |>
  # Judicial Precincts
  st_join(
    judicialDistricts |>
      mutate(judicial_district =  paste("Judicial District", DISTRICT)) |>
      select(judicial_district, judicial_district_id = DISTRICT)
  ) |>
  # Voting Precincts
  st_join(
    votingPrecincts |>
      select(
        voting_precinct = NAMELSAD20, voting_precinct_id = GEOID20
      )
  ) |>
  # Tulsa County Public School Districts
  st_join(
    schoolDistricts |>
      select(school_district = SD_NAME, school_district_id = SD_CODE)
  ) |>
  # Tribal Lands
  st_join(
    tribalLands |>
      select(tribal_nation = TRIBAL_NAM, tribal_land = TRIBAL_ARE,
             tribal_land_id = TRIBAL_UTM)
  )

### Mapping Example
# Coordinates to sf
mapview(data_sf, col.regions = "cyan", map.types = "Stamen.Toner") +
mapview(cityCouncilDistricts, color = "purple4", lwd = 2,
        alpha = 1, alpha.regions = 0.4) +
mapview(county, color = "black", col.regions = "#000000",
        alpha = 0.8, alpha.regions = 0.05)

write_csv(data_sf, here("data/tulsaEvictionGeographies.csv"))
