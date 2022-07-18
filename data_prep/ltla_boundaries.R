library(geographr)
library(stringr)

boundaries_ltla21 |>
  filter(str_detect(ltla21_code, "^E")) |>
  write_rds("data/boundaries_ltlas.rds")
