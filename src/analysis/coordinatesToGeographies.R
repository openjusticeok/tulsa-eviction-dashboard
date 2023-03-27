## Load Packages
if (!"pacman" %in% installed.packages()) {
    install.packages("pacman")
}
library(pacman)
p_load(
    dplyr, # Data wrangling/cleaning
    tidyr, # Data wrangling
    stringr, # String manipulation
    readr, # For reading/writing data
    ggplot2, # Data visualization
    forcats, # Working with factors
    lubridate, # Dealing with dates
    here, # For readable file paths
    tidycensus, # Loading Census data
    sf, # Spatial Manipulation
    tigris, # Load Census TIGER/Line Shapefiles
    sp,
    mapview,
    ojodb # Working with OJO data
)

## Load LocationData
locationData <-
  ojo_tbl("address", schema = "eviction_addresses") |>
  select(case, lat, lon, geo_accuracy) |>
  collect()

## Check histogram of geo_accuracy to determine reasonable cutoff
# locationData |>
#   ggplot(aes(x = geo_accuracy)) +
#   geom_histogram() +
#   geom_vline(xintercept = .85)

## Load Tulsa County Eviction Case Data
caseData <- read_csv(here("out/data-management/tulsaEvictionData.csv"))

## Left join locationData to caseData
data <-
  caseData |>
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
data <-
  data |>
  filter(
    !is.na(lat),
    geo_accuracy >= .85
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
## TODO: City Council Districts
# Boilerplate:
# cityCouncilDistricts <-
#   st_read(here("src/data/cityCouncil_Districts/cityCouncil_Districts.shp")) |>
#   st_transform(crs = 4269) |>
#   st_make_valid()
## Judicial Districts
judicialDistricts <-
  st_read(here("src/data/Judicial_Districts/Judicial_Districts.shp")) |>
  st_transform(crs = 4269) |>
  st_make_valid()


## Oklahoma Public School Shapefiles
# schoolDistricts <- school_districts(state = "OK", year = 2018) # This is missing 1 county we need :(
schoolDistricts <-
  st_read(here("src/data/School_Districts/School_Districts.shp")) |>
  st_transform(crs = 4269) |>
  st_make_valid()
## Tribal Lands
tribalLands <-
  st_read(here("src/data/Tribal_Boundaries/Tribal_Boundaries.shp")) |>
  st_transform(crs = 4269) |>
  st_make_valid()
# Filter for only federally recognized tribal areas.
tribalLands <-
  tribalLands |>
  filter(!is.na(TRIBAL_NAM))

# Filter to the 14 districts that at least partially fall into Tulsa County
schoolDistricts <-
  schoolDistricts |>
  filter(COUNTY == "Tulsa")

# Convert eviction data to sf object
data_sf <-
  st_as_sf(
    data, coords = c("lon", "lat"),
    crs = 4269, agr = "constant"
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
    select(name_county = NAMELSAD, GEOID_county = GEOID, geometry_county = geometry)
  ) |>
  ## Cities/Towns
  st_join(
    citiesAndTowns |>
    mutate(municipalDesignation = str_extract(NAMELSAD, "city|town")) |>
    select(
      name_citiesAndTowns = NAME, municipalDesignation,
      GEOID_citiesAndTowns = GEOID, geometry_citiesAndTowns = geometry
    )
  ) |>
  ## ZIP Codes
  st_join(
    zipCodes |>
    select(
      GEOID_zipCodes = ZCTA5CE10, geometry_zipCodes = geometry
    )
  ) |>
  ## Census Tracts
  st_join(
    censusTracts |>
    select(
      name_censusTracts = NAMELSAD, GEOID_censusTracts = GEOID, geometry_censusTracts = geometry
    )
  ) |>
  # City/Town
  st_join(
    schoolDistricts |>
    select(schoolDistrict = SD_NAME, GEOID_schoolDistrict = SD_CODE, geometry)
  ) |>
  # Federal Legislative District
  st_join(
    federalHouse |>
    select(
      name_federalHouse = NAMELSAD, GEOID_federalHouse = GEOID, geometry_federalHouse = geometry
    )
  ) |>
  # State Legislative District
  st_join(
    stateHouse |>
    select(
      name_stateHouse = NAMELSAD, GEOID_stateHouse = GEOID, geometry_stateHouse = geometry
    )
  ) |>
  st_join(
    stateSenate |>
    select(
      name_stateSenate = NAMELSAD, GEOID_stateSenate = GEOID, geometry_stateSenate = geometry
    )
  ) |>
  # TODO: City Council District
  ## Boilerplate:
#  st_join(
#    cityCouncilDistricts |>
#    select(
#      name_cityCouncilDistricts = NAMELSAD, GEOID_cityCouncilDistricts = GEOID,
#      geometry_cityCouncilDistricts = geometry
#    )
#  ) |>
  # Judicial Precincts
  st_join(
    judicialDistricts |>
    mutate(name_judicialDistricts =  paste("Judicial District", DISTRICT)) |>
    select(name_judicialDistricts, GEOID_judicialDistricts = DISTRICT,
           geometry_judicialDistricts = geometry)
  ) |>
  # Voting Precincts
  st_join(
    votingPrecincts |>
    select(
      name_votingPrecincts = NAMELSAD20, GEOID_votingPrecincts = GEOID20,
      geometry_votingPrecincts = geometry
    )
  ) |>
  # Tulsa County Public School Districts
  st_join(
    schoolDistricts |>
    select(name_schoolDistrict = SD_NAME, GEOID_schoolDistrict = SD_CODE,
           geometry_schoolDistricts = geometry)
  ) |>
  # Tribal Lands
  st_join(
    tribalLands |>
    select(name_tribe = TRIBAL_NAM, name_tribalLand = TRIBAL_ARE,
           GEOID_tribalLands = TRIBAL_UTM, geometry_tribalLandss = geometry)
  )

### Mapping Example
# Coordinates to sf
mapview(data_sf, map.types = "Stamen.Toner") +
mapview(schoolDistricts, color = "red", alpha = 1) +
mapview(county, color = "cyan", col.regions = "#00000000")

write_csv(data_sf, here("out/analysis/tulsaEvictionGeographies.csv"))
