library(dplyr)
library(tidyr)
library(geographr)
library(readr)
library(sf)
library(stringr)
library(httr)
---------------------------------------------
  # Ranking vulnerability drivers -------------
  ---------------------------------------------

    # Loading in vulnerability data from Resiliance Index Github repo
    # https://docs.github.com/en/rest/reference/repos#contents
    # https://stackoverflow.com/questions/25022016/get-all-file-names-from-a-github-repo-through-the-github-api
    # https://stackoverflow.com/questions/25485216/how-to-get-list-files-from-a-github-repository-folder-using-r

url <- "https://api.github.com/repos/britishredcrosssociety/resilience-index/git/trees/flooding?recursive=1"
req <- GET(url)

filepaths <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
# Only pull out the LSOA related datasets
filepaths_vuln_variables <- filepaths[str_detect(filepaths, "data/vulnerability/disasters-emergencies/flooding/2022-interim/england/[a-z\\-]+/[a-z\\-\\_]+lsoa.rds")]

for (i in 1:length(filepaths_vuln_variables)) {
  raw_data_url <- paste0(
    "https://github.com/britishredcrosssociety/resilience-index/blob/flooding/",
    filepaths_vuln_variables[i],
    "?raw=true"
  )

  vuln_data <- read_rds(raw_data_url)

  if (i == 1) {
    vuln_data_joined <- vuln_data
  } else {
    vuln_data_joined <- vuln_data_joined |>
      left_join(vuln_data, by = "lsoa_code") |>
      # Deals with any duplicated variables (in more than 1 domain)
      select(-ends_with(".y")) |>
      rename_with(~ str_remove(., ".x"), ends_with(".x"))
  }
}

# Remove unwanted columns & duplicated columns
# Based on https://github.com/britishredcrosssociety/resilience-index/blob/flooding/R/vulnerability/disasters-emergencies/flooding/2022-interim/england/build-index.R
vuln_data_cleaned <- vuln_data_joined |>
  select(-starts_with("num"), -starts_with("deciles"), -people_flooded, -total_population) |>
  mutate(proportion_people_flooded = replace_na(proportion_people_flooded, 0))

# Check of how many unique variables 
ncol(vuln_data_cleaned) - 1
# Should be 20? https://github.com/britishredcrosssociety/resilience-index/blob/flooding/R/vulnerability/disasters-emergencies/flooding/2022-interim/metadata.md
# TO DO: Check

# Aligned indicators - Higher value = worse support
# Based on https://github.com/britishredcrosssociety/resilience-index/blob/flooding/R/vulnerability/disasters-emergencies/flooding/2022-interim/england/build-index.R
vuln_data_aligned <- vuln_data_cleaned |> 
  mutate(
  proportion_people_flooded = proportion_people_flooded * -1,
  proportion_primary_school_age = proportion_primary_school_age * -1
)

# Data sourced from Resilience Index repo
source("https://github.com/britishredcrosssociety/resilience-index/blob/flooding/R/vulnerability/disasters-emergencies/flooding/2022-interim/england/build-index.R")

# high score = high vulnerability
all_variables_normalised <- susceptibility_normalised |>
  left_join(prepare_normalised, by = "lsoa_code") |>
  left_join(respond_normalised, by = "lsoa_code") |>
  left_join(support_normalised, by = "lsoa_code") |>
  # some of the variables are repeated in the domains
  select(-contains(".y")) |>
  rename_with(~ str_remove(., ".x"), contains(".x")) |>
  pivot_longer(-lsoa_code, names_to = "variable", values_to = "normalised_value")

all_variables_ranked <- all_variables_normalised |>
  arrange(lsoa_code, desc(normalised_value)) |>
  group_by(lsoa_code) |>
  mutate(normalised_rank = rank(-normalised_value, ties.method = "first")) |>
  ungroup()

# Calculating quantiles of the raw values split by variable
# Gives picture of how value compares on a national level
all_variables_values <- susceptibility_indicators |>
  left_join(prepare_indicators, by = "lsoa_code") |>
  left_join(respond_indicators, by = "lsoa_code") |>
  left_join(support_indicators, by = "lsoa_code") |>
  # some of the variables are repeated in the domains
  select(-contains(".y")) |>
  rename_with(~ str_remove(., ".x"), contains(".x")) |>
  pivot_longer(-lsoa_code, names_to = "variable", values_to = "value")

# Quantiles by each variable (not LSOA)
all_variables_quantised <- all_variables_values |>
  arrange(variable, desc(value)) |>
  group_by(variable) |>
  mutate(variable_rank = rank(-value, ties.method = "first")) |>
  group_by(variable) |>
  mutate(variable_quantiles = quantise(variable_rank, num_quantiles = 10)) |>
  ungroup()

# Combine the ranks and the national quantiles and add LSOA names
lsoa_names <- lookup_lsoa11_ltla21 |>
  select(lsoa11_name, lsoa11_code)

all_variables_combined <- all_variables_quantised |>
  left_join(all_variables_ranked, by = c("lsoa_code", "variable")) |>
  select(-c("variable_rank", "normalised_value")) |>
  arrange(lsoa_code, desc(normalised_rank)) |>
  left_join(lsoa_names, by = c("lsoa_code" = "lsoa11_code"))

# TO DO - deal with NAs for variables as they shouldn't be ranked but should be flagged as missing when see area
# Check which variables have missing and what prop.
all_variables_combined |>
  group_by(variable) |>
  summarise(prop_missing = sum(is.na(value)) / n())

# Save csv
all_variables_combined |>
  rename("lsoa11_code" = "lsoa_code") |>
  relocate("lsoa11_name", .after = "lsoa11_code") |>
  write_rds("data/flooding_drivers.rds")

--------------------------------------------------------------
  # Overall flooding vulnerability scores ----------------------
  --------------------------------------------------------------

    vuln_scores <- read_csv("https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/flooding/data/vulnerability/disasters-emergencies/flooding/2022-interim/england/flood-vulnerability-index-all-scores.csv")

vuln_scores_overall <- vuln_scores |>
  mutate(top_20_national = if_else(flood_vulnerability_composite_quantiles %in% c(9, 10), 1, 0)) |>
  select(lsoa_code, vulnerability_quantiles = flood_vulnerability_composite_quantiles, top_20_national)

vuln_scores_overall_sf <- boundaries_lsoa11 |>
  filter(str_detect(lsoa11_code, "^E")) |>
  left_join(vuln_scores_overall, by = c("lsoa11_code" = "lsoa_code"))

# Check if mismtaches of LSOAs in vuln data but not in geographr data
vuln_scores_overall |>
  anti_join(boundaries_lsoa11, by = c("lsoa_code" = "lsoa11_code"))
# 619 not in boundaries_lsoa11

# TO DO: Come back to this - using ms_simplify in geographr removes 677 lsoas

boundaries_lsoa11 |>
  filter(str_detect(lsoa11_code, "^E")) |>
  anti_join(vuln_scores_overall, by = c("lsoa11_code" = "lsoa_code"))

# Save csv
vuln_scores_overall_sf |>
  write_rds("data/flooding_vuln_scores_sf.rds")
