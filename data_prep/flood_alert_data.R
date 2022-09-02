library(httr2)
library(jsonlite)
library(dplyr)
library(magrittr)
library(sf)
library(stringr)

# Source: https://environment.data.gov.uk/flood-monitoring/doc/reference
# Updated every 15 minutes.
# Levels of alerts:
# 1	Severe Flood Warning (Severe Flooding, Danger to Life.)
# 2	Flood Warning	(Flooding is Expected, Immediate Action Required.)
# 3	Flood Alert	(Flooding is Possible, Be Prepared.)
# 4	Warning no Longer in Force	

query <- "http://environment.data.gov.uk/flood-monitoring/id/floods?min-severity=3"
req <- request(query) |>
  req_perform() |>
  resp_body_string()

flood_alerts <- fromJSON(req) |>
  extract2("items") |>
  as_tibble()

# More prep 
# use https://github.com/VCS-Emergencies-Partnership/r-shiny-web-apps/blob/main/packages/dashboard/preprocess_scripts/prep-ea-floodwarning-data.R
# use mock for now

flood_alerts <- readr::read_rds("data/flood_risk_test/live_warning_mock.rds")

# Read boundaries in directly from ONS as need more precise than in geographr package as these are simplified 
# Source: https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-boundaries-super-generalised-clipped-bsc-ew-v3/explore?location=52.837570%2C-2.489798%2C7.20",
boundaries_lsoa11 <- read_sf("https://opendata.arcgis.com/datasets/e9d10c36ebed4ff3865c4389c2c98827_0.geojson")
# When automating this process have this saved as an rds file so don't need to pull in 

boundaries_lsoa11_eng  <-
  boundaries_lsoa11  |>
  select(
    lsoa11_name = LSOA11NM,
    lsoa11_code = LSOA11CD,
    geometry
  ) |>
  st_make_valid() |>
  filter(str_detect(lsoa11_code, "^E")) |>
  st_transform(crs = st_crs(flood_alerts))

# set geometry to the polygon not the point so joins via polygon
st_geometry(flood_alerts) <- "geometry_poly"

flood_alert_lsoas <- flood_alerts |>
  st_join(boundaries_lsoa11_eng) 

# set geometry to the polygon not the point so joins via polygon
st_geometry(flood_alert_lsoas) <- "geometry_poly"

flood_alert_lsoas |>
  st_drop_geometry() |>
  select(-geometry_point)
