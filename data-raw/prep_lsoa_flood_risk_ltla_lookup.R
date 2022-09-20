library(sf)
library(geographr)
library(compositr)
library(stringr)
library(dplyr)

# Flood risk areas ----

# Source: Environment Agency https://www.data.gov.uk/dataset/42c31542-228d-439b-8dbe-e72135dae71c/flood-risk-areas
# Flood Risk Areas identify locations where there is believed to be significant flood risk.
# Download flood risk map from https://environment.data.gov.uk/DefraDataDownload/?mapService=EA/FloodRiskAreas&Mode=spatial
# Note: You may need to generate a new download URL (below) from the link above
tf <- download_file("https://environment.data.gov.uk/UserDownloads/interactive/cfa8e2a3b35644718c03597c1dab868a161232/EA_FloodRiskAreas_GeoJSON_Full.zip", ".zip")

unzip(tf, exdir = tempdir())

flood_risk_areas <- read_sf(file.path(tempdir(), "data", "Flood_Risk_Areas.json"))

# Which Lower Super Output Areas intersect with flood risk areas
# boundaries_lsoa11 |>
#   ggplot() +
#   geom_sf() +
#   geom_sf(data = flood_risk_areas, fill = "red")

flood_risk_lsoa <-
  boundaries_lsoa11 |>
  filter(str_detect(lsoa11_code, "^E")) |>
  st_transform(crs = st_crs(flood_risk_areas)) |>
  st_join(flood_risk_areas, left = FALSE) # want inner join

# An LSOA can intersect with/contain more than 1 flood risk area so make distinct
# Assumption: not taking into account how much an LSOA instersect/contains a flood risk zone
# only interested if it intersects/contains at all
flood_risk_lsoa_distinct <- flood_risk_lsoa |>
  distinct(lsoa11_code) |>
  mutate(flood_risk = 1)

lsoa_flood_risk_ltla_lookup <- lookup_lsoa11_ltla21 |>
  filter(str_detect(ltla21_code, "^E")) |>
  left_join(flood_risk_lsoa_distinct, by = "lsoa11_code") |>
  mutate(flood_risk = if_else(is.na(flood_risk), 0, flood_risk))

# Save
usethis::use_data(lsoa_flood_risk_ltla_lookup, overwrite = TRUE)
