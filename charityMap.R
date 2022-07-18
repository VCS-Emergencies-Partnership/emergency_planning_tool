# UI -----
charityMapUI <- function(id) {
  leafletOutput(
    NS(id, "charity_map"),
    height = 630
  )
}

# Server -----
charityMapServer <- function(id, charities_data_subset) {

  # Checks to ensure the input is reactive (data input not reactive)
  stopifnot(is.reactive(charities_data_subset))

  moduleServer(id, function(input, output, session) {
    output$charity_map <- renderLeaflet({

      # only plot the charities where the contact info is within the chosen LTLA
      charities_data_subset() |>
        dplyr::filter(flag_contact_in_ltla == TRUE) |>
        leaflet() |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addMarkers(~long,
          ~lat,
          popup = ~charity_name,
          clusterOptions = markerClusterOptions()
        )
    })
  })
}

#--------------------------------------------------------------------------------
# Test -------------------------------------------------------------------------
#--------------------------------------------------------------------------------

charities_ltla_lookup <- read_rds("data/charities_ltla_lookup.rds")
charities_data <- read_rds("data/charities_list_latlong.rds")

source("subsetCharityData.R")

charityMapTest <- function() {
  ui <- fluidPage(
    charityMapUI("test"),
  )

  server <- function(input, output, session) {
    charities_subset_test <- subsetCharityDataServer(
      "test",
      charities_data = charities_data,
      charities_ltla_lookup_data = charities_ltla_lookup,
      ltlas_for_filtering = reactive(c("E08000014"))
    )

    charityMapServer("test",
      charities_data_subset = charities_subset_test
    )
  }
  shinyApp(ui, server)
}

charityMapTest()
