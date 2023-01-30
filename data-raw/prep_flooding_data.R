# *** IMPORTANT - READ BEFORE RUNNING *****
# This script is for both LSOA and LTLA level Neighbourhood Vulnerability Flood Index data
# The LSOA level data is pulled from ClimateJust & the LTLA data is created via 'data-raw/prep_ltla_nvfi.R' file and saved in the 'data' folder
# To run for LSOA level data comment out the LTLA code in lines 177-186
# To run for LTLA level data comment out the LSOA code in lines 58-147

library(readxl)
library(compositr)
library(janitor)
library(dplyr)
library(tidyr)
library(geographr)
library(stringr)
library(sf)
library(usethis)

# Loading in data - used in both LSOA & LTLA calcs ----
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

raw_nvfi_sayers_classification <- read_excel(list.files(
  tempdir(),
  pattern = "Climate_Just_2017_Master_Excel_Sheet_NFVI_and_SFRI_August2018.xlsx",
  full.names = TRUE
),
sheet = "Classification Scheme",
skip = 1
) |>
  clean_names() |>
  # only NVFI classification for now
  slice(1:7)

#############################################################################################################################

#####################################################
# LSOA calcs only
#####################################################

# Loading LSOA data (LSOA level data only) -----
raw_data <- read_excel(list.files(
  tempdir(),
  pattern = "Climate_Just_2017_Master_Excel_Sheet_NFVI_and_SFRI_August2018.xlsx",
  full.names = TRUE
),
sheet = "Data"
) |>
  clean_names()

# Subset so only for England
data_eng_lsoa <- raw_data |>
  filter(country == "England") |>
  rename(lsoa11_code = code,
         lsoa11_name = name)

# Check if all LSOAs in data
lookup_lsoa11_ltla21 |>
  filter(str_detect(lsoa11_code, "^E")) |>
  anti_join(data_eng_lsoa, by = "lsoa11_code")
# all 32,834 LSOAs

# Create data for LSOA flood exposure & LTLA lookup (LSOA level data only) ----
# https://www.climatejust.org.uk/sites/default/files/INFO_Sheet_SFRI.pdf
# ‘SFRIPFCI’ - SFRI individual for fluvial & costal flooding for present day
# ‘SFRIPSWI’ - SFRI individual for surface water flooding for present day

# TO DO: think about this section as only 5 LSOAs have SFRI of 0 for both fluvial & coastal (i.e. no exposure to fluvial and coastal) due to the 1:1000 year cut off of the EAI
# Q: should move back to using DEFRA flood risk/zone data where subsets at higher rate or use cut off category e.g. 'High' and above for SFRI?

flood_exposure_lsoas <- data_eng_lsoa |>
  # only neighbourhoods exposed to flooding
  filter(sfripfcg != 0 | sfripswg != 0) |>
  mutate(flood_risk = 1) |>
  select(lsoa11_code, flood_risk)

lsoa_flood_risk_ltla_lookup <- lookup_lsoa11_ltla21 |>
  filter(str_detect(ltla21_code, "^E")) |>
  left_join(flood_exposure_lsoas, by = "lsoa11_code") |>
  mutate(flood_risk = if_else(is.na(flood_risk), 0, flood_risk))

# Save ----
usethis::use_data(lsoa_flood_risk_ltla_lookup, overwrite = TRUE)

# ---- Mike Review ----
# - Why did you add deciles on top of the NFVI categories?
# - AM reply - thinking was the NFVI categories where UK wide but the quantiles are England only
#   but perhaps is better to use the NFVI categories e.g. 'Acute' etc. Checking with Paul
#   how these categories are calculated then can consider which is better for the user.

# Make data for NFVI quantiles/categories (LSOA level data only) -----
# Higher value = more vulnerable
lsoa_nvfi_quantiles <- data_eng_lsoa |>
  mutate(nvfi_quantiles_eng = quantise(nvfi, num_quantiles = 10)) |>
  select(lsoa11_code, nvfi, nvfi_quantiles_eng, sfripfcg, sfripfci) |>
  mutate(nvfi_top_20_percent_eng = if_else(nvfi_quantiles_eng %in% c(9, 10), 1, 0),
         eai = (sfripfci / nvfi) /1000) |>
  select(-sfripfci) |>
  left_join(boundaries_lsoa11, by =  "lsoa11_code") |>
  relocate(lsoa11_name, .after = lsoa11_code)

# Could clean up but changes direction (upper then lower vs. upper then lower half way through table)
# Inclusivity of upper or lower limit also seems to change at last and first class
raw_nvfi_sayers_classification

nvfi_sayers_classification <- tibble(
  lower = c(-Inf, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5),
  upper = c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5, Inf),
  label = rev(raw_nvfi_sayers_classification$label)
)

vuln_scores_flood_lsoa <- lsoa_nvfi_quantiles |>
  mutate(
    nvfi_sayers_class = case_when(
      nvfi <= -2.5 ~ "Slight",
      -2.5 < nvfi & nvfi <= -1.5 ~ "Extremely low",
      -1.5 < nvfi & nvfi <= -0.5 ~ "Relatively low",
      -0.5 < nvfi & nvfi <= 0.5 ~ "Average",
      0.5 < nvfi & nvfi <= 1.5 ~ "Relatively high",
      1.5 < nvfi & nvfi < 2.5 ~ "Extremely high",
      2.5 <= nvfi ~ "Acute"
    ), .after = nvfi_top_20_percent_eng
  ) |>
  relocate(sfripfcg, .after = nvfi_sayers_class) |>
  mutate(
    sfri_sayers_class = case_when(
      sfripfcg == 0 ~ "No exposed population",
      sfripfcg < 0 ~ "Exposed, NFVI below the UK mean",
      0 <  sfripfcg & sfripfcg < 5 ~ "Low",
      5 <= sfripfcg & sfripfcg < 12.5 ~ "Moderate",
      12.5 <= sfripfcg & sfripfcg < 25 ~ "High",
      25 <= sfripfcg & sfripfcg < 50 ~ "Very high",
      50 <= sfripfcg & sfripfcg < 100 ~ "Acute",
      100 <= sfripfcg ~ "Extreme"
    ),
    sfri_class_cleaned = case_when(
      sfripfcg <= 0 ~ 0,
      0 <  sfripfcg & sfripfcg < 5 ~ 1,
      5 <= sfripfcg & sfripfcg < 12.5 ~ 2,
      12.5 <= sfripfcg & sfripfcg < 25 ~ 3,
      25 <= sfripfcg & sfripfcg < 50 ~ 4,
      50 <= sfripfcg & sfripfcg < 100 ~ 5,
      100 <= sfripfcg ~ 6
    ), .after = sfripfcg,
  )

# Save ----
usethis::use_data(vuln_scores_flood_lsoa, overwrite = TRUE)

# More general naming of columns so code below can be used for both LSOA and LTLA level data ----
geog_col_name <- "lsoa11_name"
geog_col_code <- "lsoa11_code"
geog <- "lsoa"

data_eng <- data_eng_lsoa |>
  rename(geog_code = lsoa11_code, geog_name = lsoa11_name)

#####################################################
# LTLA calcs only
#####################################################

# Loading LTLA data -----
# Created from 'data-raw/prep_ltla_nvfi.R' file
# load("data/eng_nvfi_ltla.rda")
#
# data_eng <- eng_nvfi_ltla |>
#   rename(geog_code = ltla21_code, geog_name = ltla21_name)
#
# geog_col_name <- "ltla21_name"
# geog_col_code <- "ltla21_code"
# geog <- "ltla"

#############################################################################################################################

###################################################################################
# Create data for top drivers of vulnerability - LTLA and LSOA data can be used
###################################################################################

# Underlying variables - rank across LSOA/LTLA ----
# Data dictionary or NFVI variables: https://www.climatejust.org.uk/sites/default/files/Sayers_et_al_2017_indicator_list%20%28table%203-2%20p27%29-46789%2BMP.pdf
# High value = higher vulnerability, except e1 (direct flooding exposure) - negative as it acts to reduce the relative vulnerability of one neighbourhood compared to another.
data_eng_vars_rank_geog <- data_eng |>
  select(geog_code, a1:n3) |>
  mutate(across(where(is.numeric), standardise)) |>
  pivot_longer(-geog_code, names_to = "variable_id", values_to = "normalised_value") |>
  arrange(geog_code, desc(normalised_value)) |>
  group_by(geog_code) |>
  mutate(normalised_rank = rank(-normalised_value,
    na.last = TRUE,
    ties.method = "first"
  )) |>
  ungroup()


data_eng_vars_rank_names_geog <- data_eng_vars_rank_geog |>
  left_join(raw_lookup, by = "variable_id") |>
  relocate(variable_name, .after = geog_code) |>
  mutate(domain_variable = "variable")

# Check on how many rows expect
length(unique(data_eng$geog_code)) * nrow(raw_lookup) == nrow(data_eng_vars_rank_names_geog)

# Domains - rank across LSOA/LTLA ----
# No need to standarised as the values in the data for domains are already standardised
data_eng_domain_rank_geog <- data_eng |>
  select(geog_code, nvfi_sus:nvfi_com) |>
  pivot_longer(-geog_code, names_to = "domain_id", values_to = "normalised_value") |>
  arrange(geog_code, desc(normalised_value)) |>
  group_by(geog_code) |>
  mutate(normalised_rank = rank(-normalised_value,
    na.last = TRUE,
    ties.method = "first"
  )) |>
  ungroup()

data_eng_domain_rank_names_geog <- data_eng_domain_rank_geog |>
  mutate(domain_name = case_when(
    domain_id == "nvfi_prp" ~ "Ability to prepare",
    domain_id == "nvfi_sus" ~ "Susceptibility",
    domain_id == "nvfi_res" ~ "Ability to respond",
    domain_id == "nvfi_rec" ~ "Ability to recover",
    domain_id == "nvfi_com" ~ "Community support",
  )) |>
  mutate(domain_variable = "domain")

# Check on how many rows expect
length(unique(data_eng$geog_code)) * 5 == nrow(data_eng_domain_rank_names_geog)

# Combine domains and variables ----
# Only include top 5 variables for simplicity
data_eng_vars_domain_rank_geog <- data_eng_vars_rank_names_geog |>
  filter(normalised_rank <= 5) |>
  rename(
    domain_variable_name = variable_name,
    domain_variable_id = variable_id
  ) |>
  bind_rows(
    data_eng_domain_rank_names_geog |>
      rename(
        domain_variable_name = domain_name,
        domain_variable_id = domain_id
      )
  ) |>
  select(-normalised_value)

# Check on how many rows expect
nrow(data_eng_vars_domain_rank_geog) == 10 * length(unique(data_eng$geog_code))

# Underlying variables - quantise across variable nationally (across England, not UK) ----
data_eng_vars_rank_var <- data_eng |>
  select(geog_code, a1:n3) |>
  mutate(across(where(is.numeric), standardise)) |>
  pivot_longer(-geog_code, names_to = "domain_variable_id", values_to = "normalised_value") |>
  arrange(domain_variable_id, desc(normalised_value)) |>
  group_by(domain_variable_id) |>
  mutate(normalised_rank = rank(-normalised_value,
    na.last = TRUE,
    ties.method = "first"
  )) |>
  ungroup() |>
  group_by(domain_variable_id) |>
  mutate(quantiles_eng = quantise(normalised_rank, num_quantiles = 10)) |>
  ungroup() |>
  select(geog_code, domain_variable_id, quantiles_eng) |>
  mutate(domain_variable = "variable")

# Check on how many rows expect
nrow(data_eng_vars_rank_var) == nrow(raw_lookup) * length(unique(data_eng$geog_code))

# Domains - quantise across variable nationally (across England, not UK) ----
# No need to standarised as the values in the data for domains are already standardised
data_eng_domain_rank_var <- data_eng |>
  select(geog_code, nvfi_sus:nvfi_com) |>
  pivot_longer(-geog_code, names_to = "domain_variable_id", values_to = "normalised_value") |>
  arrange(domain_variable_id, desc(normalised_value)) |>
  group_by(domain_variable_id) |>
  mutate(normalised_rank = rank(-normalised_value,
    na.last = TRUE,
    ties.method = "first"
  )) |>
  ungroup() |>
  group_by(domain_variable_id) |>
  mutate(quantiles_eng = quantise(normalised_rank, num_quantiles = 10)) |>
  ungroup() |>
  select(geog_code, domain_variable_id, quantiles_eng) |>
  mutate(domain_variable = "domain")

# Check on how many rows expect
nrow(data_eng_domain_rank_var) == 5 * length(unique(data_eng$geog_code))

# Join on quantiles ----
# Only include top 5 variables for simplicity
vuln_drivers_flood <- data_eng_vars_domain_rank_geog |>
  left_join(
    data_eng_vars_rank_var |>
      bind_rows(data_eng_domain_rank_var),
    by = c("geog_code", "domain_variable", "domain_variable_id")
  ) |>
  select(-domain_variable_id) |>
  left_join(
    data_eng |>
      select(geog_name, geog_code),
    by = "geog_code"
  ) |>
  relocate(geog_name, .after = geog_code) |>
  rename(
    {{ geog_col_code }} := geog_code,
    {{ geog_col_name }} := geog_name
  )


# Save ----
obj_save_name <- paste0("vuln_drivers_flood_", geog)
assign(obj_save_name, vuln_drivers_flood)
do.call("use_data", list(as.name(obj_save_name), overwrite = TRUE))
