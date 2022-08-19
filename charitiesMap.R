# UI -----
charitiesMapUI <- function(id) {
  leafletOutput(
    NS(id, "charities_map"),
    height = 630
  )
}

# Server -----
charitiesMapServer <- function(id,
                               charities_data_subset,
                               lsoa_vuln_scores_sf_subset) {

  # Checks to ensure the input is reactive
  stopifnot(is.reactive(charities_data_subset))
 # stopifnot(is.reactive(lsoa_vuln_scores_sf_subset))

  moduleServer(id, function(input, output, session) {
    output$charities_map <- renderLeaflet({

      # Catch errors if no area has been selected - blank message as not at top of the page
      validate(need(nrow(charities_data_subset()) != 0, ""))

      # only plot the charities where the contact info is within the chosen LTLA
      charities_within_area <- charities_data_subset() |>
        dplyr::filter(
          flag_contact_in_ltla == 1,
          !is.na(lat),
          !is.na(long)
        ) |>
        # avoided replace_na() as from tidyr (package not used elsewhere yet)
        mutate_at(
          c(
            "charity_contact_web",
            "charity_contact_email",
            "charity_contact_phone",
            "charity_activities"
          ),
          ~ replace(., is.na(.), "-")
        )

      pal <- colorBin("Reds",
        domain = lsoa_vuln_scores_sf_subset()$vulnerability_quantiles,
        bins = c(1:10)
      )

      charities_within_area |>
        leaflet() |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addCircleMarkers(~long,
          ~lat,
          label = ~charity_name,
          clusterOptions = markerClusterOptions(),
          popup = paste0(
            "<b> Name: </b>", charities_within_area$charity_name, "<br>",
            "<b> Web: </b>", charities_within_area$charity_contact_web, "<br>",
            "<b> Email: </b>", charities_within_area$charity_contact_email, "<br>",
            "<b> Phone: </b>", charities_within_area$charity_contact_phone, "<br>",
            "<b> Actvities: </b>", charities_within_area$charity_activities
          )
        ) |>
        addPolygons(
          data = lsoa_vuln_scores_sf_subset(),
          # outline of polygon
          weight = 0.7,
          opacity = 0.5,
          color = "#5C747A",
          dashArray = "0.1",
          # fill of polygon
          fillColor = ~ pal(vulnerability_quantiles),
          fillOpacity = 0.7
        ) |>
        addLegend(
          data = lsoa_vuln_scores_sf_subset(),
          pal = pal,
          values = ~vulnerability_quantiles,
          opacity = 0.7,
          title = "Flood vulnerability index",
          position = "bottomright"
        )
    })
  })
}

#--------------------------------------------------------------------------------
# Test -------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# charities_ltla_lookup <- read_rds("data/charities_ltla_lookup.rds")
# charities_data <- read_rds("data/charities_list_latlong.rds")
# 
# subset_lsoas <- geographr::lookup_lsoa11_ltla21 |>
#   filter(ltla21_code == "E06000001") |>
#   select(lsoa11_code)
# 
# lsoa_vuln_scores_flood <- read_rds("data/flooding_vuln_scores_sf.rds")
# 
# lsoa_vuln_scores_subset_flood <- lsoa_vuln_scores_flood |>
#   inner_join(subset_lsoas, by = "lsoa11_code")
# 
# source("subsetCharitiesData.R")
# 
# charitiesMapTest <- function() {
#   ui <- fluidPage(
#     charitiesMapUI("test"),
#   )
# 
#   server <- function(input, output, session) {
#     charities_subset_test <- subsetCharitiesDataServer(
#       "test",
#       charities_data = charities_data,
#       charities_ltla_lookup_data = charities_ltla_lookup_data,
#       ltlas_for_filtering = reactive(c("Hartlepool"))
#     )
# 
#     charitiesMapServer("test",
#       charities_data_subset = charities_subset_test,
#       lsoa_vuln_scores_sf_subset = reactive(lsoa_vuln_scores_subset_flood)
#     )
#   }
#   shinyApp(ui, server)
# }
# 
# charitiesMapTest()
