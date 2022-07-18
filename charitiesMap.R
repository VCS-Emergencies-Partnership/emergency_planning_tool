# UI -----
charitiesMapUI <- function(id) {
  leafletOutput(
    NS(id, "charities_map"),
    height = 630
  )
}

# Server -----
charitiesMapServer <- function(id, charities_data_subset) {

  # Checks to ensure the input is reactive 
  stopifnot(is.reactive(charities_data_subset))

  moduleServer(id, function(input, output, session) {
    output$charities_map <- renderLeaflet({

      # only plot the charities where the contact info is within the chosen LTLA
      charities_within_area <- charities_data_subset() |>
        dplyr::filter(flag_contact_in_ltla == TRUE) |>
        # avoided replace_na() as from tidyr (package not used elsewhere yet)
        mutate_at(c("charity_contact_web", 
                    "charity_contact_email", 
                    "charity_contact_phone", 
                    "charity_activities"),  
                  ~replace(., is.na(.), "-")) 
      
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
#     charityMapUI("test"),
#   )
# 
#   server <- function(input, output, session) {
#     charities_subset_test <- subsetCharityDataServer(
#       "test",
#       charities_data = charities_data,
#       charities_ltla_lookup_data = charities_ltla_lookup,
#       ltlas_for_filtering = reactive(c("E08000014"))
#     )
# 
#     charitiesMapServer("test",
#       charities_data_subset = charities_subset_test
#     )
#   }
#   shinyApp(ui, server)
# }
# 
# charitiesMapTest()
