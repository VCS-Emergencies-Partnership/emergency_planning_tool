library(geographr)
library(stringr)

boundaries_ltlas <- boundaries_ltla21 |>
  filter(str_detect(ltla21_code, "^E"))

# Save
usethis::use_data(boundaries_ltlas, overwrite = TRUE)
