# Approach - take population weighted averages of the underlying supporting indicators at LSOA level to Local Authority level
# Then recalculate the NFVI at Local Authority Level

library(compositr)
library(janitor)
library(dplyr)
library(readxl)
library(demographr)
library(stringr)
library(geographr)

# Load data ------
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

lsoa11_2017pop <- demographr::population17_lsoa11 |>
  select(lsoa11_code, total_population) |>
  filter(str_detect(lsoa11_code, "^E"))

# Data dictionary or NFVI variables: https://www.climatejust.org.uk/sites/default/files/Sayers_et_al_2017_indicator_list%20%28table%203-2%20p27%29-46789%2BMP.pdf
data_eng_nvfi_vars_lsoa <- raw_data |>
  filter(country == "England") |>
  select(lsoa11_code = code, lsoa11_name = name, a1:n3)

data_eng_nvfi_vars_lsoa |>
  summary()

# Aggregate up to LA level via population weighted averages ----
# Have used 2017 populations since when Sayers report from

# TO DO: does population weighted crime IMD value (c1) make sense? All rest are %. Come back to once investigated variable in IMD docs.

data_eng_nvfi_vars_ltla <- data_eng_nvfi_vars_lsoa |>
  left_join(lsoa11_2017pop, by = "lsoa11_code") |>
  left_join(
    lookup_lsoa11_ltla21 |>
      select(-lsoa11_name)
    , by = "lsoa11_code") |>
  mutate(across(a1:n3, ~(.x * total_population))) |>
  group_by(ltla21_code, ltla21_name) |>
  summarise(across(a1:n3, ~(sum(.x) / sum(total_population)))) |>
  ungroup()

data_eng_nvfi_vars_ltla |>
  summary()


# Calculate domains and NVFI ----
# Calculation steps for NVFI & domains p.15 & 16 in Appendix B http://www.sayersandpartners.co.uk/uploads/6/2/0/9/6209349/appendix_b_neighbourhood_flood_vulnerability_index_-_final_-_uploaded_4june2017_revised_and_upload_280617_-_minor_corrections.pdf
# TO DO: get this calculation approach approved

nvfi_sus_variables <- c("a1", "a2", "h1", "h2")
nvfi_prp_variables <- c("i1", "i2", "i3", "i4", "i5", "f1", "f2", "k1", "t1", "t2")
nvfi_res_variables <- c("i1", "i2", "i3", "i4", "i5", "f1", "f2", "k1",  "m1", "m2", "m3", "c1")
nvfi_rec_variables <- c("i1", "i2", "i3", "i4", "i5", "f1", "f2", "m1", "m2", "m3")
# In docs 'housing characteristics' is 'hc1' but in data is 'l1'
nvfi_com_variables <- c("l1", "e1", "s1", "s2", "s3", "s4", "n1", "n2", "n3")


# Standardise variables and then sum and then standardise to get domains values
# Sum and then standarise domains to get NVFI
