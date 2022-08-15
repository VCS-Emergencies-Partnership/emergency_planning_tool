library(dplyr)
library(tidyr)
library(geographr)
library(readr)
library(sf)
library(stringr)
library(httr)

source("https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/flooding/R/utils.R") # for normalise_indicators()
#---------------------------------------------
# Ranking vulnerability drivers -------------
#---------------------------------------------
# Loading in vulnerability data from Resilience Index Github repo
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

  vuln_variables <- read_rds(raw_data_url)

  if (i == 1) {
    vuln_variables_joined <- vuln_variables
  } else {
    vuln_variables_joined <- vuln_variables_joined |>
      left_join(vuln_variables, by = "lsoa_code") |>
      # Deals with any duplicated variables (in more than 1 domain)
      select(-ends_with(".y")) |>
      rename_with(~ str_remove(., ".x"), ends_with(".x"))
  }
}


# check <- read_rds("https://github.com/britishredcrosssociety/resilience-index/blob/flooding/data/vulnerability/disasters-emergencies/flooding/2022-interim/england/ability-to-recover/long-term-unemployed-lsoa.rds?raw=true")

# Remove unwanted columns & duplicated columns
# Based on https://github.com/britishredcrosssociety/resilience-index/blob/flooding/R/vulnerability/disasters-emergencies/flooding/2022-interim/england/build-index.R
vuln_variables_cleaned <- vuln_variables_joined |>
  select(-starts_with("num"), -starts_with("deciles"), -people_flooded, -total_population) |>
  mutate(proportion_people_flooded = replace_na(proportion_people_flooded, 0))

# Check of how many unique variables
ncol(vuln_variables_cleaned) - 1
# Should be 20? https://github.com/britishredcrosssociety/resilience-index/blob/flooding/R/vulnerability/disasters-emergencies/flooding/2022-interim/metadata.md
# TO DO: Check

# Aligned indicators - Higher value = worse support
# Based on https://github.com/britishredcrosssociety/resilience-index/blob/flooding/R/vulnerability/disasters-emergencies/flooding/2022-interim/england/build-index.R
vuln_variables_aligned <- vuln_variables_cleaned |>
  mutate(
    proportion_people_flooded = proportion_people_flooded * -1,
    proportion_primary_school_age = proportion_primary_school_age * -1
  )

# high score = high vulnerability
vuln_variables_normalised <- vuln_variables_aligned |>
  normalise_indicators() |>
  pivot_longer(-lsoa_code, names_to = "variable", values_to = "normalised_value")

vuln_variables_ranked <- vuln_variables_normalised |>
  arrange(lsoa_code, desc(normalised_value)) |>
  group_by(lsoa_code) |>
  mutate(normalised_rank = rank(-normalised_value, 
                                na.last = TRUE,
                                ties.method = "first")) |>
  ungroup() |>
  mutate(normalised_rank = if_else(is.na(normalised_value), NA_integer_, normalised_rank))

# Calculating quantiles of the raw values split by variable
# Gives picture of how value compares on a national level
vuln_variables_aligned_long <- vuln_variables_aligned |>
  pivot_longer(-lsoa_code, names_to = "variable", values_to = "value")

# Quantiles by each variable (not LSOA)
# TO DO: think about if makes sense for align variables (when multipied by -1)
vuln_variables_quantised <- vuln_variables_aligned_long |>
  arrange(variable, desc(value)) |>
  group_by(variable) |>
  mutate(variable_rank = rank(-value,
                              na.last = TRUE,
                              ties.method = "first")) |>
  group_by(variable) |>
  mutate(variable_quantiles = quantise(variable_rank, num_quantiles = 10)) |>
  ungroup() |>
  mutate(variable_quantiles = if_else(is.na(value), NA_integer_, variable_quantiles))

# Combine the ranks and the national quantiles and add LSOA names
lsoa_names <- lookup_lsoa11_ltla21 |>
  select(lsoa11_name, lsoa11_code)

vuln_variables_combined <- vuln_variables_quantised |>
  left_join(vuln_variables_ranked, by = c("lsoa_code", "variable")) |>
  select(-c("variable_rank", "normalised_value")) |>
  arrange(lsoa_code, desc(normalised_rank)) |>
  left_join(lsoa_names, by = c("lsoa_code" = "lsoa11_code"))

# TO DO - deal with NAs for variables as they shouldn't be ranked but should be flagged as missing when see area
# Check which variables have missing and what prop.
vuln_variables_combined |>
  group_by(variable) |>
  summarise(prop_missing = sum(is.na(value)) / n())

vuln_variables_combined_labels <- vuln_variables_combined |>
  mutate(variable_label = case_when(
    variable == "proportion_gp_surgeries_flood_risk" ~ "Proportion of GP surgeries within flood risk areas",
    variable == "proportion_care_homes_flood_risk" ~ "Proportion of care homes within flood risk areas",
    variable == "propotion_schools_flood_risk" ~ "Proportion of schools within flood risk areas",
    variable == "proportion_emergency_services_flood_risk" ~ "Proportion of emergency services locations within flood risk areas",
    variable == "proportion_social_renters" ~ "Proportion of households renting social housing",
    # TO DO: Think about explanation of this value
    variable == "crime_imd_score" ~ "Crime Index of Multiple Deprivation Score",
    variable == "proportion_low_income_occupations" ~ "Proportion of working age population in low income occupations",
    variable == "proportion_lone_parents_dependent_children" ~ "Proportion of households who are lone parent with dependent children",
    # TO DO: Think about explanation of this value
    variable == "illness_disability" ~ "Comparative illness and disability ratio",
    variable == "proportion_long_term_unemployed" ~ "Proportion of working age population who are long term unemployed",
    # TO DO: Think about explanation of this value
    variable == "proportion_residing_in_hospitals_or_care_homes_z" ~ "Proportion of people living in medical and care establishments (z-score)",
    variable == "proportion_under_5" ~ "Proportion of young children (under 5 years) in the population",
    variable == "proportion_caravans" ~ "Proportion of households which are caravan or other mobile or temporary structures",
    variable == "proportion_primary_school_age" ~ "Proportion of primary school age children (4‐11 years) in the population",
    variable == "proportion_people_flooded" ~ "Proportion of people living in a flood risk area",
    variable == "proportion_over_75" ~ "Proportion of older people (75 years and over) in the population",
    variable == "proportion_private_renters" ~ "Proportion of households privately renting",
    variable == "proportion_no_cars" ~ "Proportion of households with no car or van",
    variable == "proportion_unemployed" ~ "Proportion of working age population who are unemployed",
    variable == "proportion_income_deprived" ~ "Proportion of population experiencing deprivation relating to low income",
    TRUE ~ variable
  )) |>
  mutate(value_label = case_when(
    is.na(value) ~ "-",  
    variable == "proportion_gp_surgeries_flood_risk" ~ paste0(round(value * 100, 1), "%"),
    variable == "proportion_care_homes_flood_risk" ~ paste0(round(value * 100, 1), "%"),
    variable == "propotion_schools_flood_risk" ~ paste0(round(value * 100, 1), "%"),
    variable == "proportion_emergency_services_flood_risk" ~ paste0(round(value * 100, 1), "%"),
    variable == "proportion_social_renters" ~ paste0(round(value * 100, 1), "%"),
    variable == "crime_imd_score" ~ as.character(round(value, 1)),
    variable == "proportion_low_income_occupations" ~ paste0(round(value * 100, 1), "%"),
    variable == "proportion_lone_parents_dependent_children" ~ paste0(round(value * 100, 1), "%"),
    variable == "illness_disability" ~ as.character(round(value, 1)),
    variable == "proportion_long_term_unemployed" ~ paste0(round(value * 100, 1), "%"),
    variable == "proportion_residing_in_hospitals_or_care_homes_z" ~ as.character(round(value, 2)),
    variable == "proportion_under_5" ~ paste0(round(value * 100, 1), "%"),
    variable == "proportion_caravans" ~ paste0(round(value * 100, 1), "%"),
    variable == "proportion_primary_school_age" ~ paste0(round(value * 100, 1) * -1, "%"),
    variable == "proportion_people_flooded" ~ paste0(round(value * 100, 1) * -1, "%"),
    variable == "proportion_over_75" ~ paste0(round(value * 100, 1), "%"),
    variable == "proportion_private_renters" ~ paste0(round(value * 100, 1), "%"),
    variable == "proportion_no_cars" ~ paste0(round(value * 100, 1), "%"),
    variable == "proportion_unemployed" ~ paste0(round(value * 100, 1), "%"),
    variable == "proportion_income_deprived" ~ paste0(round(value * 100, 1), "%"),
    TRUE ~ as.character(value)
  ))

# TO DO: Change the aligned variables back (line 61) so make sense as raw values
# TO DO: check if quantising of these variables make sense

# Save csv
vuln_variables_combined_labels |>
  select(-c("variable", "value")) |>
  rename("lsoa11_code" = "lsoa_code",
         "value" = "value_label",
         "variable" = "variable_label") |>
  relocate("lsoa11_name", .after = "lsoa11_code") |>
  write_rds("data/flooding_drivers_v2.rds")

#--------------------------------------------------------------
# Overall flooding vulnerability scores ----------------------
#--------------------------------------------------------------

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

boundaries_lsoa11 |>
  filter(str_detect(lsoa11_code, "^E")) |>
  anti_join(vuln_scores_overall, by = c("lsoa11_code" = "lsoa_code"))

# Save csv
vuln_scores_overall_sf |>
  write_rds("data/flooding_vuln_scores_sf.rds")
