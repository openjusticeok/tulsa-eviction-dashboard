##### Setup

## Load Packages
if (!"pacman" %in% installed.packages()) {
    install.packages("pacman")
}
library(pacman)
# Install ojodb
renv::install("openjusticeok/ojodb")
#remotes::install_github("openjusticeok/ojodb")
p_load(
    here, # For readable file paths
    dplyr, # Data wrangling/cleaning
    readr, # For reading/writing data
    ojodb # Working with OJO data
)

## Setup Database Access
# Setup OJO Auth
ojo_auth(username = Sys.getenv("OJO_DEFAULT_USER"),
         password = Sys.getenv("OJO_DEFAULT_PASS"),
         host = "34.122.10.67",
         port = "5432")
readRenviron("~/.Renviron")

## Setup tidycensus

##### Data Wrangling / Cleaning

## Construct Data
data <-
    # Tulsa Small Claims Data from Feb. 2022 to Feb. 2023
    ojo_tbl("case") |>
    filter(
        district == "TULSA",
        case_type == "SC",
        date_filed >= "2022-02-01",
        date_filed < "2023-03-01"
    ) |>
    select(id, district, date_filed, date_closed, status) |>
    left_join(
        ojo_tbl("issue") |>
            select(id, case_id, description, disposition),
        by = c("id" = "case_id"),
        suffix = c(".case", ".issue")
    )

## Keep Only Eviction Cases
#   We are using OJO's standard "strict" definition since it is a more research oriented application.
data <-
    data |>
    filter(
        str_detect(
            description,
            "RENT|FORCI|EVICT|DETAIN"
            )
    )

## Save Data
data |>  show_query()
data <- data |> collect()

write_csv(data, file = here("out/data-management/tulsaEvictionData.csv"))
