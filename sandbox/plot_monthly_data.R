library(here)
library(tidyverse)

data <- read_csv(here("data/tulsaEvictionData.csv"))
# This are the cases with location data ONLY. Not all of them
data_with_locations <- read_csv(here("data/tulsa_eviction_cases.csv"))

# Plots for all cases
case_day_data <-
    data |>
    summarise(n = n(), .by = date)

case_month_data <-
    data |>
    mutate(month_filed = rollback(date, roll_to_first = TRUE)) |>
    summarise(n = n(), .by = month_filed)

case_day_data |>
    ggplot(aes(x = date, y = n)) +
    geom_line()

case_month_data |>
    ggplot(aes(x = month_filed, y = n)) +
    geom_line()

# Plots for all cases with location data
case_day_data_with_locations <-
    data_with_locations |>
    summarise(n = n(), .by = date)

case_month_data_with_locations <-
    data_with_locations |>
    mutate(month_filed = rollback(date, roll_to_first = TRUE)) |>
    summarise(n = n(), .by = month_filed)

case_day_data_with_locations |>
    ggplot(aes(x = date, y = n)) +
    geom_line()

case_month_data_with_locations |>
    ggplot(aes(x = month_filed, y = n)) +
    geom_line()

# Plot for when data still has date_filed instead of date
data <- data |>
  filter(
    geo_accuracy >= 0.85,
    !is.na(lat)
  )

case_day_data <-
    data |>
    summarise(n = n(), .by = date_filed)

case_month_data <-
    data |>
    mutate(month_filed = rollback(date_filed, roll_to_first = TRUE)) |>
    summarise(n = n(), .by = month_filed)

case_day_data |>
    ggplot(aes(x = date_filed, y = n)) +
    geom_line()

case_month_data |>
    ggplot(aes(x = month_filed, y = n)) +
    geom_line()

