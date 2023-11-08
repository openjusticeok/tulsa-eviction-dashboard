##### Setup
library(here) # For readable file paths
library(dplyr) # Data wrangling/cleaning
library(readr) # For reading/writing data
library(ojodb) # Working with OJO data
library(lubridate) # Dealing with dates
library(logger) # Logging

get_ojodb_tulsa_eviction_data <- function() {
  ##### Data Wrangling / Cleaning

  ## Construct Data
  # Tulsa Small Claims Data from Jan. 2018 to Last Day of Previous Month
  last_day_of_previous_month <- rollback(today())
  log_info("Using court data from 2018-01-01 to {last_day_of_previous_month}")


  data <- ojo_tbl("case") |>
      filter(
          district == "TULSA",
          case_type == "SC",
          date_filed >= "2018-01-01",
          date_filed <= last_day_of_previous_month
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

  data <- data |>
      ojo_collect()

  write_csv(data, file = here("data/tulsa_eviction_data.csv"))
}

