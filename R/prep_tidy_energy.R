prep_tidy_energy <- function(energy) {
  energy %>%
    filter(!is.na(electricity)) %>%
    filter(!is.na(gas)) %>%
    impute_lm(gas ~ date) %>%
    impute_lm(electricity ~ date) %>%
    arrange(date) %>%
    rename(gas_kwh = gas, electricity_kwh = electricity) %>%
    select(-gas_reading_time, -electricity_reading_time) %>%
    mutate(
      gas_cost = gas_kwh * gas_rate + gas_standing,
      electricity_cost = electricity_kwh * electricity_rate + electricity_standing
    ) %>%
    mutate(
      gas_total = cumsum(gas_cost),
      electricity_total = cumsum(electricity_cost),
    ) %>%
    pivot_longer(
      !c(date, at_home, had_bath, chimken, laundry, fish, parents, supplier, tarrif_name),
      names_to = c("fuel", "var"),
      names_sep = "_"
    ) %>%
    mutate(
      year = lubridate::year(date),
      month = lubridate::month(date)
    )
}
