library(here)

source(here("R", "get_ojodb_tulsa_eviction_data.R"))
source(here("R", "coordinates_to_geographies.R"))
source(here("R", "geographies_to_demographics_and_bubbles_helpers.R"))
source(here("R", "tulsa_geographies_to_demographics_and_bubbles.R"))

get_ojodb_tulsa_eviction_data() # Get Eviction Data from Jan. 2018 to Last Month
coordinates_to_geographies() # Place Lat/Lon Case Data to Various Geographies
tulsa_geographies_to_demographics_and_bubbles() # Apply Census Data to Each Geography
