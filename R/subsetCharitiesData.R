# UI ----
subsetCharitiesDataUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("top_vuln_drivers"))
}

# Server ----
subsetCharitiesDataServer <- function(id,
                                      charities_vuln_drivers_flood_lookup,
                                      charities_lat_long,
                                      charities_ltla_lookup,
                                      vuln_drivers_flood_ltla,
                                      ltlas_for_filtering) {

  # Checks to ensure the inputs are reactive
  stopifnot(is.reactive(ltlas_for_filtering))

  moduleServer(id, function(input, output, session) {
    top_flooding_drivers_ltla <- reactive({
      vuln_drivers_flood_ltla |>
        # Just look at top 3 drivers of social vulnerability to flooding
        dplyr::filter(
          ltla21_name == ltlas_for_filtering(),
          domain_variable == "variable",
          normalised_rank %in% c(1, 2, 3)
        ) |>
        select(-c(quantiles_eng, domain_variable))
    })


    charities_data_subset <- reactive({
      charities_in_area_codes <- charities_ltla_lookup |>
        dplyr::filter(ltla21_name %in% ltlas_for_filtering()) |>
        select(organisation_number) |>
        pull()

      charities_working_in_top_flooding_drivers_codes <- charities_vuln_drivers_flood_lookup |>
        dplyr::filter(variable_name %in% top_flooding_drivers_ltla()$domain_variable_name) |>
        select(organisation_number) |>
        pull()

      charities_area_and_vuln <- intersect(charities_in_area_codes, charities_working_in_top_flooding_drivers_codes)

      # Output table
      charities_lat_long |>
        dplyr::filter(organisation_number %in% charities_area_and_vuln) |>
        mutate(flag_contact_in_ltla = charity_contact_ltla_name %in% ltlas_for_filtering()) |>
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
    })


    # Dropdown for UI ----
    # Section 'Using renderUI within modules' from https://shiny.rstudio.com/articles/modules.html
    output$top_vuln_drivers <- renderUI({
      ns <- session$ns

      pickerInput(
        ns("charity_top_vuln_drivers_chosen"),
        label = "Select driver:",
        choices = top_flooding_drivers_ltla()$domain_variable_name,
        selected = top_flooding_drivers_ltla()$domain_variable_name,
        options = list(`actions-box` = TRUE),
        multiple = TRUE
      )
    })

    # Return 2 items from this module
    # https://mastering-shiny.org/scaling-modules.html?q=list#multiple-outputs
    list(
      data = reactive(charities_data_subset()),
      top_vulns = reactive(top_flooding_drivers_ltla()$domain_variable_name)
    )
  })
}

#--------------------------------------------------------------------------------
# Test -------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# load("data/charities_vuln_drivers_flood_lookup.rda")
# load("data/charities_lat_long.rda")
# load("data/charities_ltla_lookup.rda")
# load("data/vuln_drivers_flood_ltla.rda")
#
# subsetCharitiesDataTest <- function() {
#   ui <- fluidPage(
#     subsetCharitiesDataUI("test")
#   )
#
#   server <- function(input, output, session) {
#     charities_subset <- subsetCharitiesDataServer(
#       "test",
#       charities_vuln_drivers_flood_lookup = charities_vuln_drivers_flood_lookup,
#       charities_lat_long = charities_lat_long,
#       charities_ltla_lookup = charities_ltla_lookup,
#       vuln_drivers_flood_ltla = vuln_drivers_flood_ltla,
#       ltlas_for_filtering = reactive(c("Central Bedfordshire"))
#     )
#   }
#   shinyApp(ui, server)
# }
#
# # Run test
# subsetCharitiesDataTest()
