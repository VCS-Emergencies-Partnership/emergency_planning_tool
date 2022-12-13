library(jsonlite)
library(geographr)
library(PostcodesioR)
library(compositr)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

# Load data ----
# Source: https://register-of-charities.charitycommission.gov.uk/register/full-register-download

# Had issues with the .txt files as not pulling through all the rows in the data
# missing about 50k entries in main table, so using JSON files

# Charity list
tf <- download_file("https://ccewuksprdoneregsadata1.blob.core.windows.net/data/json/publicextract.charity.zip", ".zip")

tf |>
  unzip(exdir = tempdir())

charities_list_raw <-
  fromJSON(
    list.files(
      tempdir(),
      pattern = "publicextract.charity.json",
      full.names = TRUE
    ),
    flatten = TRUE
  )


# Charity areas of operation
tf <- download_file("https://ccewuksprdoneregsadata1.blob.core.windows.net/data/json/publicextract.charity_area_of_operation.zip", ".zip")

tf |>
  unzip(exdir = tempdir())

charities_areas_raw <-
  fromJSON(
    list.files(
      tempdir(),
      pattern = "publicextract.charity_area_of_operation.json",
      full.names = TRUE
    ),
    flatten = TRUE
  )


# Identifiers in data -----
# organisation_number: The organisation number for the charity. This is the index value for the charity (used for joining).
# registered_charity_number: The registration number of the registered organisation allocated by the Commission. Note that a main charity and all its linked charities will share the same registered_charity_number.
# linked_charity_number: A number that uniquely identifies the subsidiary or group member associated with a registered charity. Used for user identification purposes where the subsidiary is known by the parent registration number and the subsidiary number. The main parent charity has a linked_charity_number of 0.

# Info on data -----
# In the Charity Commissioner data there is a table 'area of operation' that has
# either the UTLAs the charity operates in or 'Throughout London', 'Throughout England' or 'Throughout UK'
# (or what overseas countries operates) so think this would be the field to use over the
# postcode as in theory should more accurately represent the area the charity covers (rather than just where it's HQ is).

###############################
# Charity regional data ----
###############################

# All cases when type = 'Country' is not a UK country.
charities_areas_raw |>
  filter(geographic_area_type == "Country") |>
  distinct(geographic_area_description) |>
  arrange(geographic_area_description)

# All England charities ----
charities_areas_raw |>
  filter(geographic_area_type == "Region") |>
  distinct(geographic_area_description)

all_england_charities <- charities_areas_raw |>
  filter(geographic_area_type == "Region") |>
  filter(geographic_area_description %in% c("Throughout England And Wales", "Throughout England")) |>
  select(organisation_number)

# English UTLAs
utla_list_geogr_eng <- lookup_ltla21_utla21 |>
  filter(str_detect(utla21_code, "^E")) |>
  mutate(utla21_name = str_to_lower(utla21_name)) |>
  arrange(utla21_name)

# Merge the UTLA codes to the charity list
all_england_charities_codes <- all_england_charities |>
  merge(utla_list_geogr_eng)

# London charities ----
all_london_charities <- charities_areas_raw |>
  filter(geographic_area_type == "Region") |>
  filter(geographic_area_description %in% c("Throughout London"))

# UTLAs and LTLAs in London
london_utla_codes <- lookup_ltla21_region21 |>
  left_join(lookup_ltla21_utla21, by = "ltla21_code") |>
  filter(region21_name == "London") |>
  select(-c("region21_code", "region21_name", "ltla21_name.y")) |>
  rename(ltla21_name = ltla21_name.x)

# Merge the UTLA codes to the charity list
all_london_charities_codes <- all_london_charities |>
  select(organisation_number) |>
  merge(london_utla_codes)

# UTLA specific charities ----
utla_list_cc <- charities_areas_raw |>
  filter(geographic_area_type == "Local Authority") |>
  distinct(geographic_area_description)

# Create lists of UTLA's to get matches
# Welsh UTLAs (so can remove)
utla_list_geogr_wales <- lookup_ltla21_utla21 |>
  filter(str_detect(utla21_code, "^W")) |>
  mutate(utla21_name = str_to_lower(utla21_name)) |>
  arrange(utla21_name)

# Find Local Authority names not matched in UTLA list
utla_unmatched <- utla_list_cc |>
  mutate(geographic_area_description = str_to_lower(geographic_area_description)) |>
  select(utla21_name = geographic_area_description) |>
  anti_join(utla_list_geogr_wales, by = "utla21_name") |>
  anti_join(utla_list_geogr_eng, by = "utla21_name")

# Manually change some UTLA names to allow matching
charities_areas_updated <- charities_areas_raw |>
  mutate(
    geographic_area_description = str_to_lower(geographic_area_description)
  ) |>
  mutate(
    geographic_area_description = case_when(
      geographic_area_description == "bristol city" ~ "bristol, city of",
      geographic_area_description == "birmingham city" ~ "birmingham",
      geographic_area_description == "plymouth city" ~ "plymouth",
      geographic_area_description == "peterborough city" ~ "peterborough",
      geographic_area_description == "leeds city" ~ "leeds",
      geographic_area_description == "leicester city" ~ "leicester",
      geographic_area_description == "city of westminster" ~ "westminster",
      geographic_area_description == "herefordshire" ~ "herefordshire, county of",
      geographic_area_description == "liverpool city" ~ "liverpool",
      geographic_area_description == "nottingham city" ~ "nottingham",
      geographic_area_description == "city of york" ~ "york",
      geographic_area_description == "cheshire west & chester" ~ "cheshire west and chester",
      geographic_area_description == "sheffield city" ~ "sheffield",
      geographic_area_description == "salford city" ~ "salford",
      geographic_area_description == "durham" ~ "county durham",
      geographic_area_description == "city of wakefield" ~ "wakefield",
      geographic_area_description == "derby city" ~ "derby",
      geographic_area_description == "manchester city" ~ "manchester",
      geographic_area_description == "southampton city" ~ "southampton",
      geographic_area_description == "bradford city" ~ "bradford",
      geographic_area_description == "coventry city" ~ "coventry",
      geographic_area_description == "telford & wrekin" ~ "telford and wrekin",
      geographic_area_description == "st helens" ~ "st. helens",
      geographic_area_description == "kingston upon hull city" ~ "kingston upon hull, city of",
      geographic_area_description == "newcastle upon tyne city" ~ "newcastle upon tyne",
      geographic_area_description == "poole" ~ "bournemouth, christchurch and poole",
      geographic_area_description == "bournemouth" ~ "bournemouth, christchurch and poole",
      geographic_area_description == "stoke-on-trent city" ~ "stoke-on-trent",
      geographic_area_description == "portsmouth city" ~ "portsmouth",
      geographic_area_description == "city of swansea" ~ "swansea",
      geographic_area_description == "newport city" ~ "newport",
      geographic_area_description == "rhondda cynon taff" ~ "rhondda cynon taf",
      TRUE ~ geographic_area_description
    )
  )

# Check all matched ----
charities_areas_updated |>
  filter(geographic_area_type == "Local Authority") |>
  distinct(geographic_area_description) |>
  select(utla21_name = geographic_area_description) |>
  anti_join(utla_list_geogr_wales, by = "utla21_name") |>
  anti_join(utla_list_geogr_eng, by = "utla21_name")
# What UTLAs in northamptonshire? https://geoportal.statistics.gov.uk/documents/ons::local-authority-districts-counties-and-unitary-authorities-april-2021-map-in-united-kingdom-/explore
# Maybe North & West northamptonshire based on https://en.wikipedia.org/wiki/Northamptonshire

# Do manual matching for Northamptonshire ---
northamptonshire_ltla_codes <- utla_list_geogr_eng |>
  filter(str_detect(utla21_name, "northamptonshire")) |>
  select(ltla21_code, ltla21_name)

charities_areas_updated |>
  filter(
    geographic_area_type == "Local Authority",
    str_detect(geographic_area_description, "northamptonshire")
  ) |>
  distinct(geographic_area_description)

all_northamptonshire_charities <- charities_areas_raw |>
  filter(str_detect(geographic_area_description, "Northamptonshire"))

northamptonshire_ltla_codes <- all_northamptonshire_charities |>
  select(organisation_number) |>
  merge(northamptonshire_ltla_codes)

# Filter out Wales ----
utla_charities_eng <- charities_areas_updated |>
  filter(geographic_area_type == "Local Authority") |>
  anti_join(utla_list_geogr_wales, by = c("geographic_area_description" = "utla21_name"))

# Checks on regional data ----
reg_char <- charities_list_raw |>
  filter(charity_registration_status == "Registered") |>
  distinct(organisation_number)

reg_char_area <- charities_list_raw |>
  filter(charity_registration_status == "Registered") |>
  inner_join(charities_areas_raw, by = "organisation_number") |>
  distinct(organisation_number)

nrow(reg_char_area) / nrow(reg_char)

# There are ~184k active charities in the Charity Commissioner data. Of these:
# 8% (~15k) have no area of operation data (and only 28 of these have a postcode entry, as thought could use this in cases where this operation area field is blank)
# 5% (~9k) only have areas of operation outside of England
# 87% (~160k) have an area of operation within England (+ possibly out-with too)

# Create list of charities and the UTLAs they cover
utla_charities_eng_codes <- utla_charities_eng |>
  select(organisation_number, utla21_name = geographic_area_description) |>
  left_join(utla_list_geogr_eng, by = "utla21_name")

utla_charities_eng_codes |>
  filter(if_any(everything(), is.na))

# Combine England, London and UTLA charities
charities_ltla_lookup <- utla_charities_eng_codes |>
  select(organisation_number, ltla21_code, ltla21_name) |>
  mutate(area = "ltla_utla") |>
  bind_rows(all_london_charities_codes |>
    select(organisation_number, ltla21_code, ltla21_name) |>
    mutate(area = "london")) |>
  bind_rows(all_england_charities_codes |>
    select(organisation_number, ltla21_code, ltla21_name) |>
    mutate(area = "england")) |>
  bind_rows(northamptonshire_ltla_codes |>
    mutate(area = "ltla_utla")) |>
  distinct(organisation_number, ltla21_name, area)

charities_ltla_lookup |>
  nrow()

charities_ltla_lookup |>
  distinct(organisation_number, area) |>
  group_by(area) |>
  summarise(count = n()) |>
  ungroup() |>
  mutate(prop = count / sum(count))

# Save ----
usethis::use_data(charities_ltla_lookup, overwrite = TRUE)

############################################
# Charity  HQ/Contact locations ----
############################################

charities_subset <- charities_list_raw |>
  select(
    organisation_number,
    charity_name,
    charity_name,
    charity_activities,
    charity_contact_postcode,
    charity_contact_phone,
    charity_contact_email,
    charity_contact_web
  ) |>
  semi_join(charities_ltla_lookup, by = "organisation_number") |>
  mutate(charity_contact_postcode_join = str_to_upper(str_replace_all(charity_contact_postcode, " ", ""))) |>
  left_join(lookup_postcode_oa11_lsoa11_msoa11_ltla20, by = c("charity_contact_postcode_join" = "postcode")) |>
  left_join(lookup_lsoa11_ltla21, by = "lsoa11_code") |>
  select(-c("oa11_code", "lsoa11_code", "msoa11_code", "ltla20_code", "lsoa11_code", "lsoa11_code")) |>
  rename(
    charity_contact_ltla_name = ltla21_name,
    charity_contact_ltla_code = ltla21_code
  )

unique_postcodes <- charities_subset |>
  distinct(charity_contact_postcode_join) |>
  filter(!is.na(charity_contact_postcode_join)) |>
  pull()

# Loop to use ostcodesioR::bulk_postcode_lookup which takes a 1 element list of 100 postcodes
# Does take some time to run
postcode_lookup <- NULL
num_loops <- ceiling(length(unique_postcodes) / 100) + 1

for (i in 1:num_loops) {
  start <- 1 + 100 * (i - 1)
  end <- start + 99

  pc <- unique_postcodes[start:end]

  returned <- bulk_postcode_lookup(
    postcodes = list(postcodes = pc)
  )

  returned_results <- map(returned, "result")

  values <- tibble(
    postcodes = pc,
    long = map_chr(returned_results, "longitude", .default = NA_character_),
    lat = map_chr(returned_results, "latitude", .default = NA_character_),
    loop = i
  )

  postcode_lookup <- postcode_lookup |>
    bind_rows(values)
}

# Check all postcodes have been run
charities_subset |>
  distinct(charity_contact_postcode_join) |>
  filter(!is.na(charity_contact_postcode_join)) |>
  anti_join(postcode_lookup, by = c("charity_contact_postcode_join" = "postcodes"))

# Don't actually use this in the app but saving as code takes time to run
usethis::use_data(postcode_lookup, overwrite = TRUE)


# Join charity data to lat/long
charities_lat_long <- charities_subset |>
  left_join(postcode_lookup |>
              select(-loop) |>
              filter(!is.na(postcodes)), by = c("charity_contact_postcode_join" = "postcodes")) |>
  select(-c("charity_contact_postcode_join", "lsoa11_name", "charity_contact_ltla_code"))
# |>
#   mutate(long = as.numeric(long),
#          lat = as.numeric(lat)
#          )

# Save ----
usethis::use_data(charities_lat_long, overwrite = TRUE)
