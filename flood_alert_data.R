library(httr2)
library(jsonlite)
library(dplyr)
library(magrittr)
library(geographr)
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

boundaries_lsoa11_eng <- boundaries_lsoa11 |>
  filter(str_detect(lsoa11_name, "^E")) |>
  st_transform(crs = st_crs(flood_alerts))

flood_alerts |>
  st_join(boundaries_lsoa11_eng) 