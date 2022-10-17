library(tidyverse)

load("data/charities_lat_long.rda")

# No contact LTLA (based on postcode to LSOA to LTLA lookup table 'lookup_postcode_oa11_lsoa11_msoa11_ltla20' in geographr)
charities_lat_long |>
  filter(!is.na(charity_contact_postcode)) |>
  summarise(count_no_contact_ltla_postcode_match = sum(is.na(charity_contact_ltla_name)),
            prop_no_contact_ltla_postcode_match = round(sum(is.na(charity_contact_ltla_name)) / n(), 3))
# Check if more up to date postcode to LSOA to LTLA table and add to geographr and use if so
# 0.6% no match of postcode to LSOA (then LTLA)

charities_lat_long |>
  summarise(count_no_contact_postcode = sum(is.na(charity_contact_postcode)),
            prop_no_contact_postcode = round(sum(is.na(charity_contact_postcode)) / n(), 3))
# 35% have no contact postcode in the data

charities_lat_long |>
  filter(!is.na(charity_contact_postcode)) |>
  summarise(count_no_contact_lat_long_postcode_match = sum(is.na(lat)),
            prop_no_contact_lat_long_postcode_match = round(sum(is.na(lat)) / n(), 3))
# 0.7% of those which have a postcode have a lat/long returned
