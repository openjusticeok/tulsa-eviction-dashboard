data <-
    read_csv(file = here("data/tulsaEvictionData.csv"))
case_monthData <-
    data |>
    ojo_add_population_data() |>
    ojo_aggregate_cases(frequency = "month")

case_monthData |>
    select(-c(population, n_per1k)) |>
    ojo_lines(x = month_filed, y  = n)
