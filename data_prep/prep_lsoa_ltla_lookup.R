library(geographr)
library(stringr)

lookup_lsoa11_ltla21 |>
  filter(str_detect(ltla21_code, "^E")) |>
  write_rds("data/lsoa_ltla_lookup.rds")