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

      # Once loaded .rda file no longer recognised as spatial object?
      lsoa_vuln_scores_sf_subset_clean <- reactive({
        lsoa_vuln_scores_sf_subset() |>
          st_as_sf(crs = 4326) |>
          rename(vulnerability_quantiles = nvfi_quantiles_eng)
      })


      # Catch errors if no area has been selected - blank message as not at top of the page
      validate(need(nrow(charities_data_subset()) != 0, ""))

      # only plot the charities where the contact info is within the chosen LTLA
      charities_within_area <- charities_data_subset() |>
        dplyr::filter(
          flag_contact_in_ltla == 1,
          !is.na(lat),
          !is.na(long)
        )

      pal <- colorBin("inferno",
        reverse = TRUE,
        domain = lsoa_vuln_scores_sf_subset_clean()$vulnerability_quantiles,
        bins = c(1:10)
      )

      legend_labels <- c("Least vulnerable", rep("", times = 7), "Most vulnerable")

      charities_within_area |>
        leaflet() |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addCircleMarkers(~long,
          ~lat,
          label = ~charity_name,
          clusterOptions = markerClusterOptions(),
          popup = paste0(
            "<b> Name: </b>", charities_within_area$charity_name, "<br>",
            "<b> Web: </b>", paste0("<a href='", charities_within_area$charity_contact_web, "' target='_blank'>", charities_within_area$charity_contact_web, "</a>"), "<br>",
            "<b> Email: </b>", charities_within_area$charity_contact_email, "<br>",
            "<b> Phone: </b>", charities_within_area$charity_contact_phone, "<br>",
            "<b> Actvities: </b>", charities_within_area$charity_activities
          )
        ) |>
        addPolygons(
          data = lsoa_vuln_scores_sf_subset_clean(),
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
          data = lsoa_vuln_scores_sf_subset_clean(),
          pal = pal,
          values = ~vulnerability_quantiles,
          opacity = 0.7,
          title = "Flood vulnerability",
          position = "bottomright",
          # To change from numeric values to 'most/least vulnerable labels'
          labFormat = function(type, cuts, p) {
            paste0(legend_labels)
          }
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
