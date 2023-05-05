judicial_districts %>%
  .[counties_geo, ] %>%
  ggplot(aes(fill = name)) +
  ojodb::ojo_fill() +
  geom_sf() +
  geom_sf(
    data =
      census_tract_raw_data |>
      filter(NAME == "Census Tract 74.10, Tulsa County, Oklahoma"),
    aes(fill = NAME)
  )

  st_touches(
    judicial_districts,
    census_tract_raw_data |>
      filter(NAME == "Census Tract 74.10, Tulsa County, Oklahoma"),
    sparse = FALSE
  )

  touches <-
    st_touches(
      judicial_districts,
      census_tract_raw_data |>
        filter(NAME == "Census Tract 74.10, Tulsa County, Oklahoma"),
      sparse = FALSE
    )
  intersects <-
    st_intersects(
      judicial_districts,
      census_tract_raw_data |>
        filter(NAME == "Census Tract 74.10, Tulsa County, Oklahoma"),
      sparse = FALSE
    )

plot(all_geographies[['voting_precincts']]['name'])
plot(bubbles[['voting_precincts']])

## Invisibly return plots
walk(geographies_census_data, plot)
