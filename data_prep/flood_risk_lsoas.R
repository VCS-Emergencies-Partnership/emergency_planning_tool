library(sf)
library(geographr)

source("https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/R/utils.R") # for download_file() function

# Flood risk areas ----

# Source: Environment Agency https://www.data.gov.uk/dataset/42c31542-228d-439b-8dbe-e72135dae71c/flood-risk-areas
# Flood Risk Areas identify locations where there is believed to be significant flood risk.
# Download flood risk map from https://environment.data.gov.uk/DefraDataDownload/?mapService=EA/FloodRiskAreas&Mode=spatial
# Note: You may need to generate a new download URL (below) from the link above
tf <- download_file("https://environment.data.gov.uk/UserDownloads/interactive/d1d0be51b49940c19c39e7627e5a057a111424/EA_FloodRiskAreas_GeoJSON_Full.zip", ".zip")

unzip(tf, exdir = tempdir())

flood_risk_areas <- read_sf(file.path(tempdir(), "data", "Flood_Risk_Areas.json"))

# Which Lower Super Output Areas intersect with flood risk areas 
flood_risk_lsoa <- 
  boundaries_lsoa11 |>
  filter(str_detect(lsoa11_code, "^E")) |>
  st_transform(crs = st_crs(flood_risk_areas)) |>
  st_join(flood_risk_areas)

# An LSOA can intersect with/contain more than 1 flood risk area so make distinct
# Assumption: not taking into account how much an LSOA insersect/contains a flood risk zone
# only interested if it intersects/contains at all
flood_risk_lsoa_distinct <- flood_risk_lsoa |>
  distinct(lsoa11_code)



