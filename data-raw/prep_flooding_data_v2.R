library(readxl)
library(compositr)
library(janitor)
library(dplyr)
library(tidyr)
library(geographr)
library(stringr)
library(sf)

#####################################################
# Loading in Flood vuln data
#####################################################

# Loading data -----
# Data downloaded https://www.climatejust.org.uk/map
# 'Download' button on left hand side of page
# 'Excel format' -> 'Neighbourhood Flood Vulnerability Index (NFVI) and Social Flood Risk Index (SFRI) data'
# 'Revised August 2018 for improved SFRI mapping (see Excel notes)'
tf <- download_file("http://maps.humanities.manchester.ac.uk/cj/2018/Climate_Just_2017_Master_Excel_Sheet_NFVI_and_SFRI_August2018.zip", ".zip")

tf |>
  unzip(exdir = tempdir())

raw_data <- read_excel(list.files(
  tempdir(),
  pattern = "Climate_Just_2017_Master_Excel_Sheet_NFVI_and_SFRI_August2018.xlsx",
  full.names = TRUE
),
sheet = "Data"
) |>
  clean_names()

# Subset so only for England
data_eng <- raw_data |>
  filter(country == "England") |>
  rename(lsoa11_code = code, lsoa11_name = name)


#####################################################
# Loading in helper data
#####################################################

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


#####################################################
# Step 1 - Make data for top drivers of vulnerability
#####################################################

# Underlying variables - rank across LSOA ----
# Data dictionary or NFVI variables: https://www.climatejust.org.uk/sites/default/files/Sayers_et_al_2017_indicator_list%20%28table%203-2%20p27%29-46789%2BMP.pdf
# High value = higher vulnerability, except e1 (direct flooding exposure) - negative as it acts to reduce the relative vulnerability of one neighbourhood compared to another.
# TO DO: come back to e1 - a larger value of e1 would increase vulnerability?
data_eng_vars_rank_lsoa <- data_eng |>
  select(lsoa11_code, a1:n3) |>
  mutate(across(where(is.numeric), standardise)) |>
  pivot_longer(-lsoa11_code, names_to = "variable_id", values_to = "normalised_value") |>
  arrange(lsoa11_code, desc(normalised_value)) |>
  group_by(lsoa11_code) |>
  mutate(normalised_rank = rank(-normalised_value,
    na.last = TRUE,
    ties.method = "first"
  )) |>
  ungroup()


data_eng_vars_rank_names_lsoa <- data_eng_vars_rank_lsoa |>
  left_join(raw_lookup, by = "variable_id") |>
  relocate(variable_name, .after = lsoa11_code) |>
  mutate(domain_variable = "variable")

# Check on how many rows expect
length(unique(data_eng$lsoa11_code)) * nrow(raw_lookup) == nrow(data_eng_vars_rank_names_lsoa)

# Domains - rank across LSOA ----
# No need to standarised as the values in the data for domains are already standardised
data_eng_domain_rank_lsoa <- data_eng |>
  select(lsoa11_code, nvfi_sus:nvfi_com) |>
  pivot_longer(-lsoa11_code, names_to = "domain_id", values_to = "normalised_value") |>
  arrange(lsoa11_code, desc(normalised_value)) |>
  group_by(lsoa11_code) |>
  mutate(normalised_rank = rank(-normalised_value,
    na.last = TRUE,
    ties.method = "first"
  )) |>
  ungroup()

data_eng_domain_rank_names_lsoa <- data_eng_domain_rank_lsoa |>
  mutate(domain_name = case_when(
    domain_id == "nvfi_prp" ~ "Ability to prepare",
    domain_id == "nvfi_sus" ~ "Susceptibility",
    domain_id == "nvfi_res" ~ "Ability to respond",
    domain_id == "nvfi_rec" ~ "Ability to recover",
    domain_id == "nvfi_com" ~ "Community support",
  )) |>
  mutate(domain_variable = "domain")

# Check on how many rows expect
length(unique(data_eng$lsoa11_code)) * 5 == nrow(data_eng_domain_rank_names_lsoa)

# Combine domains and variables ----
# Only include top 5 variables for simplicity
data_eng_vars_domain_rank_lsoa <- data_eng_vars_rank_names_lsoa |>
  filter(normalised_rank <= 5) |>
  rename(
    domain_variable_name = variable_name,
    domain_variable_id = variable_id
  ) |>
  bind_rows(
    data_eng_domain_rank_names_lsoa |>
      rename(
        domain_variable_name = domain_name,
        domain_variable_id = domain_id
      )
  ) |>
  select(-normalised_value)

# Check on how many rows expect
nrow(data_eng_vars_domain_rank_lsoa) == 10 * length(unique(data_eng$lsoa11_code))

# Underlying variables - quantise across variable nationally (across England, not UK) ----
data_eng_vars_rank_var <- data_eng |>
  select(lsoa11_code, a1:n3) |>
  mutate(across(where(is.numeric), standardise)) |>
  pivot_longer(-lsoa11_code, names_to = "domain_variable_id", values_to = "normalised_value") |>
  arrange(domain_variable_id, desc(normalised_value)) |>
  group_by(domain_variable_id) |>
  mutate(normalised_rank = rank(-normalised_value,
    na.last = TRUE,
    ties.method = "first"
  )) |>
  ungroup() |>
  mutate(quantiles_eng = quantise(normalised_rank, num_quantiles = 10)) |>
  select(lsoa11_code, domain_variable_id, quantiles_eng) |>
  mutate(domain_variable = "variable")

# Check on how many rows expect
nrow(data_eng_vars_rank_var) == nrow(raw_lookup) * length(unique(data_eng$lsoa11_code))

# Domains - quantise across variable nationally (across England, not UK) ----
# No need to standarised as the values in the data for domains are already standardised
data_eng_domain_rank_var <- data_eng |>
  select(lsoa11_code, nvfi_sus:nvfi_com) |>
  pivot_longer(-lsoa11_code, names_to = "domain_variable_id", values_to = "normalised_value") |>
  arrange(domain_variable_id, desc(normalised_value)) |>
  group_by(domain_variable_id) |>
  mutate(normalised_rank = rank(-normalised_value,
    na.last = TRUE,
    ties.method = "first"
  )) |>
  ungroup() |>
  mutate(quantiles_eng = quantise(normalised_rank, num_quantiles = 10)) |>
  select(lsoa11_code, domain_variable_id, quantiles_eng) |>
  mutate(domain_variable = "domain")

# Check on how many rows expect
nrow(data_eng_domain_rank_var) == 5 * length(unique(data_eng$lsoa11_code))

# Join on quantiles ----
# Only include top 5 variables for simplicity
vuln_drivers_flood <- data_eng_vars_domain_rank_lsoa |>
  left_join(
    data_eng_vars_rank_var |>
      bind_rows(data_eng_domain_rank_var),
    by = c("lsoa11_code", "domain_variable", "domain_variable_id")
  ) |>
  select(-domain_variable_id)

# Save ----
usethis::use_data(vuln_drivers_flood, overwrite = TRUE)

#####################################################
# Step 2 - Make data for NFVI quantiles/categories
#####################################################

# Higher value = more vulnerable
lsoa_nvfi_quantiles <- data_eng |>
  mutate(nvfi_quantiles_eng = quantise(nvfi, num_quantiles = 10)) |>
  select(lsoa11_code, nvfi, nvfi_quantiles_eng) |>
  mutate(top_20_eng = if_else(nvfi_quantiles_eng %in% c(9, 10), 1, 0)) |>
  left_join(boundaries_lsoa11, by = "lsoa11_code") |>
  relocate(lsoa11_name, .after = lsoa11_code)

# Could clean up but changes direction (upper then lower vs. upper then lower half way through table)
# Inclusivity of upper or lower limit also seems to change at last and first class
raw_nvfi_sayers_classification

nvfi_sayers_classification <- tibble(
  lower = c(-Inf, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5),
  upper = c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5, Inf),
  label = rev(raw_nvfi_sayers_classification$label)
)

vuln_scores_flood <- lsoa_nvfi_quantiles |>
  mutate(sayers_class = case_when(
    nvfi <= -2.5 ~ "Slight",
    -2.5 < nvfi & nvfi <= -1.5 ~ "Extremely low",
    -1.5 < nvfi & nvfi <= -0.5 ~ "Relatively low",
    -0.5 < nvfi & nvfi <= 0.5 ~ "Average",
    0.5 < nvfi & nvfi <= 1.5 ~ "Relatively high",
    1.5 < nvfi & nvfi < 2.5 ~ "Extremely high",
    2.5 <= nvfi ~ "Acute"
  ), .after = top_20_eng)

# Save ----
usethis::use_data(vuln_scores_flood, overwrite = TRUE)

########################################################
# Step 3 - Make data for LSOA flood exposure & LTLA lookup
########################################################

# https://www.climatejust.org.uk/sites/default/files/INFO_Sheet_SFRI.pdf
# ‘SFRIPFCI’ - SFRI individual for fluvial & costal flooding for present day
# ‘SFRIPSWI’ - SFRI individual for surface water flooding for present day
flood_exposure_lsoas <- data_eng |>
  # only neighbourhoods exposed to flooding
  filter(sfripfcg != 0 & sfripswg != 0) |>
  mutate(flood_risk = 1) |>
  select(lsoa11_code, flood_risk)

lsoa_flood_risk_ltla_lookup <- lookup_lsoa11_ltla21 |>
  filter(str_detect(ltla21_code, "^E")) |>
  left_join(flood_exposure_lsoas, by = "lsoa11_code") |>
  mutate(flood_risk = if_else(is.na(flood_risk), 0, flood_risk))

# Save ----
usethis::use_data(lsoa_flood_risk_ltla_lookup, overwrite = TRUE)

