library(shiny) 
library(sf)
library(leaflet)
library(readr)
library(dplyr)

source("topVuln.R")
source("subsetData.R")
source("vulMap.R")
source("topDrivers.R")
source("emergency_planning_tool.R")

vuln_scores_flood <- read_rds("data/flooding_vuln_scores_sf.rds")
vuln_drivers_flood <- read_rds("data/flooding_drivers.rds")

emergency_planning_tool()
