# UI ----
topVulnUI <- function(id) {
  div(
    # TO DO: Decide if want border
    # "border-style: solid; border-color: black; border-width: thin;
    style = "text-align: center; font-size: 120%",
    textOutput(NS(id, "top_vuln_text"))
  )
}

# Server ----
topVulnServer <- function(id, lsoa_vuln_scores_sf_subset) {

  # Checks to ensure the inputs are reactive
  stopifnot(is.reactive(lsoa_vuln_scores_sf_subset))

  moduleServer(id, function(input, output, session) {
    output$top_vuln_text <- renderText({

      # Catch errors if no area has been selected - show message as at top of the page
      validate(need(nrow(lsoa_vuln_scores_sf_subset()) != 0, "Please select an area on the first tab."))

      prop_top_20 <- lsoa_vuln_scores_sf_subset() |>
        summarise(prop = sum(top_20_eng) / n())

      paste0(
        round(prop_top_20$prop, 2) * 100,
        "% of the neighbourhoods in your area are the most vulnerable to flooding nationally"
      )
    })
  })
}

#--------------------------------------------------------------------------------
# Test -------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# library(geographr)
#
# subset_lsoas <- lookup_lsoa11_ltla21 |>
#   filter(ltla21_code == "E06000001") |>
#   select(lsoa11_code)
#
# load("data/vuln_scores_flood.rda")
#
# lsoa_vuln_scores_subset_flood <- vuln_scores_flood |>
#   inner_join(subset_lsoas, by = "lsoa11_code")
#
# topVulnTest <- function() {
#   ui <- fluidPage(
#     topVulnUI("test")
#   )
#   server <- function(input, output, session) {
#     topVulnServer("test",
#       lsoa_vuln_scores_sf_subset = reactive(lsoa_vuln_scores_subset_flood)
#     )
#   }
#   shinyApp(ui, server)
# }
#
# # Run test
# topVulnTest()
