# Server ----
subsetVulnDataServer <- function(id, lsoa_data, ltlas_for_filtering) {

  # Checks to ensure the inputs are reactive
  stopifnot(is.reactive(ltlas_for_filtering))

  moduleServer(id, function(input, output, session) {

    lsoas_selected <- reactive({
      lsoa_flood_risk_ltla_lookup |>
        dplyr::filter(ltla21_name %in% ltlas_for_filtering()) |>
        select(c("lsoa11_code")) |>
        pull()
    })

    reactive({
        lsoa_data |>
        dplyr::filter(lsoa11_code %in% lsoas_selected())

    })
  })
}

#--------------------------------------------------------------------------------
# Test -------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# load("data/vuln_scores_flood_lsoa.rda")
#
# subsetVulnDataTest <- function() {
#   ui <- fluidPage(
#
#   )
#   server <- function(input, output, session) {
#     lsoa_vuln_scores_subset_flood <-  subsetVulnDataServer(
#       "test",
#       lsoa_data = vuln_scores_flood_lsoa,
#       ltlas_for_filtering = reactive(c("Central Bedfordshire", "Broxbourne"))
#     )
#
#     observe(print(lsoa_vuln_scores_subset_flood()))
#   }
#   shinyApp(ui, server)
# }
#
# # Run test
# subsetVulnDataTest()
