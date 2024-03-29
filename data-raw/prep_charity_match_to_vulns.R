# Script aims to match social flooding vulnerabilities to charity activities to provide organisations to help with the vulnerabilities

library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)
library(readxl)
library(fuzzyjoin)
library(janitor)
library(compositr)

# Load Charity Commissioner categories data ----
# Source: https://register-of-charities.charitycommission.gov.uk/register/full-register-download

# Had issues with the .txt files as not pulling through all the rows in the data
# missing about 50k entries in main table, so using JSON files# Charity classification
tf <- download_file("https://ccewuksprdoneregsadata1.blob.core.windows.net/data/json/publicextract.charity_classification.zip", ".zip")

tf |>
  unzip(exdir = tempdir())

charities_classification_raw <-
  fromJSON(
    list.files(
      tempdir(),
      pattern = "publicextract.charity_classification.json",
      full.names = TRUE
    ),
    flatten = TRUE
  ) |>
  tibble::as_tibble()

# Charity activities
tf <- download_file("https://ccewuksprdoneregsadata1.blob.core.windows.net/data/json/publicextract.charity.zip", ".zip")

tf |>
  unzip(exdir = tempdir())

charities_activities_raw <-
  fromJSON(
    list.files(
      tempdir(),
      pattern = "publicextract.charity.json",
      full.names = TRUE
    ),
    flatten = TRUE
  )

# Clean activities data to have the same format as the classification data
charities_activities <- charities_activities_raw |>
  select(date_of_extract, organisation_number, registered_charity_number, linked_charity_number, charity_activities) |>
  filter(!is.na(charity_activities)) |>
  mutate(classification_code = as.numeric(1),
         classification_type = "Activity", .after=linked_charity_number) |>
  rename(classification_description = charity_activities)

# Bind the charity activities and classification data together
charities_classification <- rbind(charities_classification_raw, charities_activities)

# Loading in NVFI lookup ----
# Data downloaded https://www.climatejust.org.uk/map
# 'Download' button on left hand side of page
# 'Excel format' -> 'Neighbourhood Flood Vulnerability Index (NFVI) and Social Flood Risk Index (SFRI) data'
# 'Revised August 2018 for improved SFRI mapping (see Excel notes)'
tf <- download_file("http://maps.humanities.manchester.ac.uk/cj/2018/Climate_Just_2017_Master_Excel_Sheet_NFVI_and_SFRI_August2018.zip", ".zip")

tf |>
  unzip(exdir = tempdir())

raw_lookup <- read_excel(list.files(
  tempdir(),
  pattern = "Climate_Just_2017_Master_Excel_Sheet_NFVI_and_SFRI_August2018.xlsx",
  full.names = TRUE
),
sheet = "Look-up",
skip = 41
) |>
  clean_names() |>
  select(variable_id = column_name, variable_name = long_name) |>
  # 27 variables in NFVI calcs
  slice(1:27)

# Match organisation categories to NVFI social vulnerability variables -----

# Info on the matching logic: https://brcsbrms.sharepoint.com/sites/VCSEP/_layouts/15/doc.aspx?sourcedoc={78fe3e10-91a6-4589-946b-65358bed3b84}&action=edit
# TO DO: increase matching of vulnerabilities and charities

# Note: these should be used as detection terms, not exact matches so use fuzzyjoin package
lookup_vuln_id_match_term <- tribble(
  ~variable_id, ~charity_comm_classification_match_term ,
  "a1", "children/young people",
  "n3", "children/young people",
  "a2", "elderly/old people",
  "h1", "disabilities|disability",
  "h2", "the advancement of health or saving of lives",
  "m1", "disabilities|disability",
  "i1", "economic/community development/employment",
  "i2", "economic/community development/employment",
  "i3", "economic/community development/employment",
  "i4", "economic/community development/employment",
  "i5", "economic/community development/employment|prevention or relief of poverty",
  "t1", "accommodation/housing",
  "t2", "accommodation/housing",
  "l1", "accommodation/housing",
  "f1", "refugee|immigration",
  "f2", "language|education",
  "k1", "refugee|immigration|displacement|people of a particular ethnic or racial origin",
  "m2", "care home",
  "m3", "transport|car",
  "c1", "crime",
  "n2", "single parent|lone parent"
)

# Test of fuzzyjoin joining
# fuzzyjoin_test_data <-   tribble(
#   ~registered_charity_number, ~classification_description ,
#   200001,   "People With Disabilities",
#   200001,   "Disability",
#   200001,   "Economic/community Development/employment",
#   200001,  "The Prevention Or Relief Of Poverty"
# )
#
# fuzzyjoin_test_data |>
#   mutate(classification_description = str_to_lower(classification_description)) |>
#   fuzzy_left_join(lookup_vuln_id_match_term, by = c("classification_description" = "charity_comm_classification_match_term"), match_fun = str_detect) |>
#   View()

charities_vuln_drivers_flood_lookup <- charities_classification |>
  select(organisation_number, classification_type, classification_description) |>
  mutate(classification_description = str_to_lower(classification_description)) |>
  fuzzy_left_join(lookup_vuln_id_match_term, by = c("classification_description" = "charity_comm_classification_match_term"), match_fun = str_detect) |>
  distinct(organisation_number, variable_id) |>
  filter(!is.na(variable_id)) |>
  left_join(raw_lookup, by = "variable_id") |>
  select(-variable_id)

# Save ----
usethis::use_data(charities_vuln_drivers_flood_lookup, overwrite = TRUE)
