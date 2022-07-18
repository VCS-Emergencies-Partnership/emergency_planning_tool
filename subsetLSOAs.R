# Server ----
subsetLSOAServer <- function(id, lsoa_ltla_lookup, ltlas_for_filtering) {
  
  # Checks to ensure the inputs are reactive
  stopifnot(is.reactive(ltlas_for_filtering))
  
  moduleServer(id, function(input, output, session) {
    reactive({
      lsoa_ltla_lookup |>
      dplyr::filter(ltla21_code %in% ltlas_for_filtering()$ltlas) |>
        select(lsoa11_code) |>
        pull()
      
    })
  })
}

#--------------------------------------------------------------------------------
# Test -------------------------------------------------------------------------
#--------------------------------------------------------------------------------

lsoa_ltla_lookup <- read_rds("data/lsoa_ltla_lookup.rds")

subsetLSOATest <- function() {
  ui <- fluidPage(
  )
  
  server <- function(input, output, session) {
    
    selected_ltlas <- reactiveValues(ltlas =c("E06000001", "E06000002")))
    
    lsoas_selected <-  subsetLSOAServer(
      "test",
      lsoa_ltla_lookup = lsoa_ltla_lookup,
      ltlas_for_filtering = selected_ltlas)
    )

    observe(print(lsoas_selected()))
  }
  shinyApp(ui, server)
}

# Run test
subsetLSOATest()
