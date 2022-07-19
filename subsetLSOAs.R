# Server ----
subsetLSOAServer <- function(id, lsoa_ltla_lookup, ltlas_for_filtering) {

  # Checks to ensure the inputs are reactive
  stopifnot(is.reactive(ltlas_for_filtering))

  moduleServer(id, function(input, output, session) {
    reactive({
      
    })
  })
}

#--------------------------------------------------------------------------------
# Test -------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# lsoa_ltla_lookup <- read_rds("data/lsoa_ltla_lookup.rds")
#
# subsetLSOATest <- function() {
#   ui <- fluidPage()
#
#   server <- function(input, output, session) {
#     selected_ltlas <- reactiveVal(c("Northumberland", "Carlisle"))
#
#     lsoas_selected <- subsetLSOAServer(
#       "test",
#       lsoa_ltla_lookup = lsoa_ltla_lookup,
#       ltlas_for_filtering = selected_ltlas
#     )
#
#
#     observe(print(lsoas_selected()))
#   }
#   shinyApp(ui, server)
# }
#
# # Run test
# subsetLSOATest()
