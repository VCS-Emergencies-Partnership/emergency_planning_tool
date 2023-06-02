# *** IMPORTANT - READ BEFORE RUNNING *****
# This script is for both LSOA and LTLA level Neighbourhood Vulnerability heat Index data
# The LSOA level data is pulled from ClimateJust & the LTLA data is created via 'data-raw/prep_ltla_sphvi.R' file and saved in the 'data' folder
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

# Load data ------
# Data provided by Sarah from Manchester uni - previous data available on Climate Just website
# Only contains data for England - using 2011 LSOA codes
# SPHVI = Socio Spatial Heat Vulnerability Index
# Loading LSOA data (LSOA level data only) -----
raw_data <- read_xlsx("data-raw/heat_index_2022.xlsx", sheet = "SPHVI Z Score Data") |>
  clean_names() |>
  rename(lsoa11_code = code, lsoa11_name = name) |>
  # Data error: EH6 and EX5 as well as EF3 and U2 contain the same data but have been given a different ID. Therefore, removing one of each of these variables from the list.
  select(-ex5, -u2)

# skim(raw_data)

raw_classification <- read_xlsx(
  path = "data-raw/heat_index_2022.xlsx",
  sheet = "Classification Scheme",
  skip = 1
) |>
  clean_names() |>
  slice(1:7)

raw_lookup <- read_excel(
  path = "data-raw/heat_index_2022.xlsx",
  sheet = "Indicator Metadata"
) |>
  clean_names() |>
  distinct(id, .keep_all = T) |>
  drop_na(id) |>
  mutate(
    id = str_to_lower(id),
    id = case_when(
      id == "m1 (h2)" ~ "m1_h2",
      id == "m2 (h4)" ~ "m2_h4",
      T ~ id
    )
  ) |>
  select(variable_id = id, variable_name = indicator_name) |>
  # Data error: EH6 and EX5 as well as EF3 and U2 contain the same data but have been given a different ID. Therefore, removing one of each of these variables from the list.
  filter(!variable_id %in% c("ex5", "u2"))

#############################################################################################################################

#####################################################
# LSOA calcs only
#####################################################

# Check if all LSOAs in data
lookup_lsoa11_ltla21 |>
  filter(str_detect(lsoa11_code, "^E")) |>
  anti_join(raw_data, by = "lsoa11_code")
# all 32,844 LSOAs

# Make data for SPHVI quantiles/categories (LSOA level data only) -----
# Higher value = more vulnerable
lsoa_sphvi_quantiles <- raw_data |>
  mutate(sphvi_quantiles_eng = quantise(sphvi, num_quantiles = 10)) |>
  select(lsoa11_code, sphvi, sphvi_quantiles_eng) |>
  mutate(sphvi_top_20_percent_eng = if_else(sphvi_quantiles_eng %in% c(9, 10), 1, 0)) |>
  left_join(boundaries_lsoa11, by = "lsoa11_code") |>
  relocate(lsoa11_name, .after = lsoa11_code)

# Could clean up but changes direction (upper then lower vs. upper then lower half way through table)
# Inclusivity of upper or lower limit also seems to change at last and first class
raw_classification

sphvi_climate_just_classification <- tibble(
  lower = c(-Inf, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5),
  upper = c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5, Inf),
  label = rev(raw_classification$label)
)

vuln_scores_heat_lsoa <- lsoa_sphvi_quantiles |>
  mutate(
    sphvi_climate_just_class = case_when(
      sphvi <= -2.5 ~ "Slight",
      -2.5 < sphvi & sphvi <= -1.5 ~ "Extremely low",
      -1.5 < sphvi & sphvi <= -0.5 ~ "Relatively low",
      -0.5 < sphvi & sphvi <= 0.5 ~ "Average",
      0.5 < sphvi & sphvi <= 1.5 ~ "Relatively high",
      1.5 < sphvi & sphvi < 2.5 ~ "Extremely high",
      2.5 <= sphvi ~ "Acute"
    ), .after = sphvi_top_20_percent_eng
  )

# Save ----
usethis::use_data(vuln_scores_heat_lsoa, overwrite = TRUE)

#####################################################
# LTLA calcs only
#####################################################

# More general naming of columns so code below can be used for both LSOA and LTLA level data ----
geog_col_name <- "lsoa11_name"
geog_col_code <- "lsoa11_code"
geog <- "lsoa"

data_eng <- raw_data |>
  rename(geog_code = lsoa11_code, geog_name = lsoa11_name)

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
# High value = higher vulnerability, except e1 (direct flooding exposure) - negative as it acts to reduce the relative vulnerability of one neighbourhood compared to another.
data_eng_vars_rank_geog <- data_eng |>
  select(geog_code, a1:x2) |>
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
  select(geog_code, sphvi_sus:sphvi_exp) |>
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
    domain_id == "sphvi_prp" ~ "Ability to prepare",
    domain_id == "sphvi_sus" ~ "Susceptibility",
    domain_id == "sphvi_res" ~ "Ability to respond",
    domain_id == "sphvi_rec" ~ "Ability to recover",
    domain_id == "sphvi_exp" ~ "Enhanced Exposure",
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
  select(geog_code, a1:x2) |>
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
  select(geog_code, sphvi_sus:sphvi_exp) |>
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
vuln_drivers_heat <- data_eng_vars_domain_rank_geog |>
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
obj_save_name <- paste0("vuln_drivers_heat_", geog)
assign(obj_save_name, vuln_drivers_heat)
do.call("use_data", list(as.name(obj_save_name), overwrite = TRUE))
