# Server ----
subsetCharitiesDataServer <- function(id, charities_data, charities_ltla_lookup_data, ltlas_for_filtering) {
  
  # Checks to ensure the inputs are reactive
  stopifnot(is.reactive(ltlas_for_filtering))
  
  moduleServer(id, function(input, output, session) {
    reactive({
      charities_ltla_lookup |>
        dplyr::filter(ltla21_name %in% ltlas_for_filtering()) |>
        inner_join(charities_data, by = "organisation_number") |>
        select(-ltla21_code) |>
        # create flag where charity has contact info is within the chosen LTLA
        mutate(flag_contact_in_ltla = charity_contact_ltla_name %in% ltlas_for_filtering()) 
      
    })
  })
}

#--------------------------------------------------------------------------------
# Test -------------------------------------------------------------------------
#--------------------------------------------------------------------------------
  
# charities_ltla_lookup <- read_rds("data/charities_ltla_lookup.rds")
# charities_data <- read_rds("data/charities_list_latlong.rds")
# 
# subsetCharitiesDataTest <- function() {
#   ui <- fluidPage(
# 
#   )
#   server <- function(input, output, session) {
#     charities_subset <-  subsetCharitiesDataServer(
#       "test",
#       charities_data = charities_data,
#       charities_ltla_lookup_data = charities_ltla_lookup,
#       ltlas_for_filtering = reactive(c("Central Bedfordshire", "Broxbourne"))
#     )
# 
#     observe(print(charities_subset()))
#   }
#   shinyApp(ui, server)
# }
# 
# # Run test
# subsetCharitiesDataTest()
