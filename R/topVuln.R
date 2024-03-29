# UI ----
topVulnUI <- function(id) {
  div(
    style = "text-align: left; font-size: 120%",
    textOutput(NS(id, "top_vuln_text")),
    textOutput(NS(id, "ltla_clicked_name"))
  )
}

# Server ----
topVulnServer <- function(id,
                          lsoa_vuln_scores_sf_subset,
                          selected_ltlas) {

  # Checks to ensure the inputs are reactive
  stopifnot(is.reactive(lsoa_vuln_scores_sf_subset))

  moduleServer(id, function(input, output, session) {
    output$top_vuln_text <- renderText({

      # Catch errors if no area has been selected - show message as at top of the page
      validate(need(nrow(lsoa_vuln_scores_sf_subset()) != 0, "Please select an area on the first tab."))

      prop_top_20 <- lsoa_vuln_scores_sf_subset() |>
        summarise(prop = sum(nvfi_top_20_percent_eng) / n())

      selected_ltlas()

      paste0(
        round(prop_top_20$prop, 2) * 100,
        "% of the neighbourhoods in ",
        selected_ltlas(),
        " local authority are the most socially vulnerable to flooding nationally"
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
# load("data/vuln_scores_flood_lsoa.rda")
#
# lsoa_vuln_scores_subset_flood <- vuln_scores_flood_lsoa |>
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
