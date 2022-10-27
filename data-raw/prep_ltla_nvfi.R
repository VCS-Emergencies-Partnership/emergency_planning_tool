# Approach - take population weighted averages of the underlying supporting indicators at LSOA level to Local Authority level
# Then recalculate the NFVI at Local Authority Level using calculation steps for NVFI & domains p.15 & 16 in Appendix B http://www.sayersandpartners.co.uk/uploads/6/2/0/9/6209349/appendix_b_neighbourhood_flood_vulnerability_index_-_final_-_uploaded_4june2017_revised_and_upload_280617_-_minor_corrections.pdf

# Step 1 - Load LSOA level data
# Step 2 - Filter for England only
# Step 3 - Aggregate up to LTLA level via population weighted averages
# Step 4 - Calculate domains and NVFI with LTLA level calculated in Step 3 ----


library(compositr)
library(janitor)
library(dplyr)
library(readxl)
library(demographr)
library(stringr)
library(geographr)
library(skimr)

source("R/utils.R")

# Step 1 - Load data ------
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

lsoa11_2017pop <- population17_lsoa11 |>
  select(lsoa11_code, total_population) |>
  filter(str_detect(lsoa11_code, "^E"))

# Step 2 - Filter for England only ----
# Data dictionary or NFVI variables: https://www.climatejust.org.uk/sites/default/files/Sayers_et_al_2017_indicator_list%20%28table%203-2%20p27%29-46789%2BMP.pdf
eng_nvfi_vars_lsoa <- raw_data |>
  filter(country == "England") |>
  select(lsoa11_code = code, lsoa11_name = name, a1:n3, sfripfcg, sfripswg)

# eng_nvfi_vars_lsoa |>
#   skim()

# Step 3 - Aggregate up to LTLA level via population weighted averages ----
# Have used 2017 populations since when Sayers report from
# ---- Review ----
# - While this may align time period of data, would it not be better to use more
#   recent estimates which better reflect population distributions, even if
#   underlying indicators have changed? (I don't know the answer to this question)

# Both c1 and i5, which have come from IMD values
# page 15 http://www.sayersandpartners.co.uk/uploads/6/2/0/9/6209349/appendix_b_neighbourhood_flood_vulnerability_index_-_final_-_uploaded_4june2017_revised_and_upload_280617_-_minor_corrections.pdf
# 'If an indicator is already in the form of a rank (e.g., as is the Index of Multiple Deprivation, IMD), the equivalent z score is determined by assuming the rank is drawn from a normal distribution and calculating the number of standard deviations from the mean associated with that rank.'
# TO DO: Unsure if this step has already been done for the raw indicators - to investigate and check with Paul.

# Only include LSOAs which have flood exposure
# p.67 of full report http://www.sayersandpartners.co.uk/uploads/6/2/0/9/6209349/sayers_2017_-_present_and_future_flood_vulnerability_risk_and_disadvantage_-_final_report_-_uploaded_05june2017_printed_-_stan.pdf
# 'population weighted average across all neighbourhoods exposed to flooding'

# Check: do all LTLAs have at least one LSOA (neighbourhood) exposed to flooding?
eng_nvfi_vars_lsoa |>
  left_join(
    lookup_lsoa11_ltla21 |>
      select(-lsoa11_name)
    , by = "lsoa11_code") |>
  mutate(flood_exposed = if_else(sfripfcg != 0 | sfripswg != 0, 1, 0)) |>
  group_by(ltla21_name, ltla21_code) |>
  summarise(count_flood_exposed = sum(flood_exposed)) |>
  filter(count_flood_exposed == 0)
# Yes

# https://www.climatejust.org.uk/sites/default/files/INFO_Sheet_SFRI.pdf
# ‘SFRIPFCI’ - SFRI individual for fluvial & costal flooding for present day
# ‘SFRIPSWI’ - SFRI individual for surface water flooding for present day

eng_nvfi_vars_pop_weighted_ltla <- eng_nvfi_vars_lsoa |>
  left_join(lsoa11_2017pop, by = "lsoa11_code") |>
  # only neighbourhoods exposed to flooding
  filter(sfripfcg != 0 | sfripswg != 0) |>
  left_join(
    lookup_lsoa11_ltla21 |>
      select(-lsoa11_name)
    , by = "lsoa11_code") |>
  mutate(across(a1:n3, ~(.x * total_population))) |>
  group_by(ltla21_code, ltla21_name) |>
  summarise(across(a1:n3, ~(sum(.x) / sum(total_population)))) |>
  ungroup()


# eng_nvfi_vars_pop_weighted_ltla |>
#     skim()

# Step 4 - Calculate domains and NVFI with LTLA level calculated in Step 3 ----
# Calculation steps for NVFI & domains p.15 & 16 in Appendix B http://www.sayersandpartners.co.uk/uploads/6/2/0/9/6209349/appendix_b_neighbourhood_flood_vulnerability_index_-_final_-_uploaded_4june2017_revised_and_upload_280617_-_minor_corrections.pdf
# TO DO: get this calculation approach approved
geog_columns <- c("ltla21_code", "ltla21_name")
nvfi_sus_variables <- c("a1", "a2", "h1", "h2")
nvfi_prp_variables <- c("i1", "i2", "i3", "i4", "i5", "f1", "f2", "k1", "t1", "t2")
nvfi_res_variables <- c("i1", "i2", "i3", "i4", "i5", "f1", "f2", "k1",  "m1", "m2", "m3", "c1")
nvfi_rec_variables <- c("i1", "i2", "i3", "i4", "i5", "f1", "f2", "m1", "m2", "m3")
# In docs 'housing characteristics' is 'hc1' but in data is 'l1'
nvfi_com_variables <- c("l1", "e1", "s1", "s2", "s3", "s4", "n1", "n2", "n3")


# Standardise variables and then sum and then standardise to get domains values
# Sum and then standardise domains to get NVFI

# TO DO: think about comment about standardising the variables which are ranks i.e. c1
# p. 15 in Appendix B; http://www.sayersandpartners.co.uk/uploads/6/2/0/9/6209349/appendix_b_neighbourhood_flood_vulnerability_index_-_final_-_uploaded_4june2017_revised_and_upload_280617_-_minor_corrections.pdf
eng_nvfi_ltla_standardised_domains <- eng_nvfi_vars_pop_weighted_ltla |>
mutate(nvfi_sus = standarise_summed_standarise_cols(eng_nvfi_vars_pop_weighted_ltla, id_columns = geog_columns, calc_columns = nvfi_sus_variables),
       nvfi_prp = standarise_summed_standarise_cols(eng_nvfi_vars_pop_weighted_ltla, id_columns = geog_columns, calc_columns = nvfi_prp_variables),
       nvfi_res = standarise_summed_standarise_cols(eng_nvfi_vars_pop_weighted_ltla, id_columns = geog_columns, calc_columns = nvfi_res_variables),
       nvfi_rec = standarise_summed_standarise_cols(eng_nvfi_vars_pop_weighted_ltla, id_columns = geog_columns, calc_columns = nvfi_rec_variables),
       nvfi_com = standarise_summed_standarise_cols(eng_nvfi_vars_pop_weighted_ltla, id_columns = geog_columns, calc_columns = nvfi_com_variables)) |>
  select(all_of(geog_columns), contains("nvfi"))

eng_nvfi_ltla_standardised_domains_nvfi <- eng_nvfi_ltla_standardised_domains |>
  mutate(nvfi = standarise_summed_cols(eng_nvfi_ltla_standardised_domains, id_columns = geog_columns, calc_columns = c("nvfi_sus", "nvfi_prp", "nvfi_res", "nvfi_rec", "nvfi_com")),
         .after = ltla21_name)

# eng_nvfi_ltla_standardised_domains_nvfi |>
#   skim()
#
# raw_data |>
#   select(contains("nvfi")) |>
#   skim()

# ---- Review ----
# - Do you not want to include standarised indicators (in addition to domains and
#   nvfi overall score)? The indicators below are the population weighted, not
#   standardised scores
# - AM reply - Was looking to replicate format of the LSOA level dataset from ClimateJust where they are the raw indicator values

# LTLA level dataset to save ----
eng_nvfi_ltla <- eng_nvfi_ltla_standardised_domains_nvfi |>
  left_join(data_eng_nvfi_vars_ltla,
            by = c("ltla21_code", "ltla21_name"))


usethis::use_data(eng_nvfi_ltla, overwrite = TRUE)
