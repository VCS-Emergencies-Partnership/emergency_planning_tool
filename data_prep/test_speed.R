# Test speed - join vs. filter 

charities_ltla_lookup <- read_rds("data/charities_ltla_lookup.rds")
charities_data <- read_rds("data/charities_list_latlong.rds")

ltlas_test <- lookup_ltla21_utla21 |>
  sample_n(3) |>
  select(ltla21_code)

system.time({ 
  charities_ltla_lookup |>
  filter(ltla21_code %in% ltlas_test$ltla21_code)
})
# 0.454

system.time({ 
charities_ltla_lookup |>
 inner_join(ltlas_test, by = "ltla21_code")
})
#0.678

system.time({ 
ltlas_test |>
  inner_join(charities_ltla_lookup, by = "ltla21_code")
  })
#0.371
