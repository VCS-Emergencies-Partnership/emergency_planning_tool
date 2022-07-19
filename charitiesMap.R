# UI -----
charitiesMapUI <- function(id) {
  leafletOutput(
    NS(id, "charities_map"),
    height = 630
  )
}

# Server -----
charitiesMapServer <- function(id, charities_data_subset, filter_for_within_area) {

  # Checks to ensure the input is reactive
  stopifnot(is.reactive(charities_data_subset))
  stopifnot(is.reactive(filter_for_within_area))

  moduleServer(id, function(input, output, session) {
    output$charities_map <- renderLeaflet({

      # Catch errors if no area has been selected - blank message as not at top of the page
      validate(need(nrow(charities_data_subset()) != 0, ""))

      # only plot the charities where the contact info is within the chosen LTLA
      charities_within_area <- charities_data_subset() |>
        dplyr::filter(
          flag_contact_in_ltla == filter_for_within_area(),
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
#       charities_ltla_lookup_data = charities_ltla_lookup,
#       ltlas_for_filtering = reactive(c("Hartlepool"))
#     )
#
#     charitiesMapServer("test",
#       charities_data_subset = charities_subset_test,
#       filter_for_within_area = reactive("TRUE")
#     )
#   }
#   shinyApp(ui, server)
# }
#
# charitiesMapTest()
