##### Setup
library(pacman)

p_load(
    here, # For readable file paths
    dplyr, # Data wrangling/cleaning
    readr, # For reading/writing data
    ojodb # Working with OJO data
)

## Setup tidycensus

##### Data Wrangling / Cleaning

## Construct Data
# Tulsa Small Claims Data from Feb. 2022 to Feb. 2023
data <- ojo_tbl("case") |>
    filter(
        district == "TULSA",
        case_type == "SC",
        date_filed >= "2018-01-01",
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
data <- data |>
    filter(
        str_detect(
            description,
            "RENT|FORCI|EVICT|DETAIN"
        )
    )

## Save Data
data |>  show_query()
data <- data |> collect()

write_csv(data, file = here("data/tulsaEvictionData.csv"))
