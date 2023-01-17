# UI ----
subsetCharitiesDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(NS(id, "selected_ltla_name_charity")),
    uiOutput(NS(id, "top_flooding_drivers_ltla_text")),
    uiOutput(NS(id, "top_drivers_ltla_text")),
    uiOutput(ns("top_vuln_drivers"))
  )
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

    # Automate text for initial sentence
    output$selected_ltla_name_charity <- renderText({

      # Select the LTLA name
      ltla_name_select <- vuln_drivers_flood_ltla |>
        dplyr::filter(
          ltla21_name == ltlas_for_filtering()
          ) |>
        select(ltla21_name) |>
        distinct()

      # Add the automated text
      paste0(
        "The top drivers of social vulnerability to flooding in ",
        ltla_name_select,
        " local authority are: "
      )
    })

    # Calc top drivers for chosen LTLA ----
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

    # Dropdown for UI ----
    # Section 'Using renderUI within modules' from https://shiny.rstudio.com/articles/modules.html
    output$top_vuln_drivers <- renderUI({

      # Catch errors if no area has been selected
      # Leave message blank as error message shown in map below (avoid duplicate message)
      validate(need(length(ltlas_for_filtering()) != 0, ""))

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

    # Output text for top drivers ----
    output$top_flooding_drivers_ltla_text <- renderUI({

      # Catch errors if no area has been selected
      # Leave message blank as error message shown in map below (avoid duplicate message)
      validate(need(length(ltlas_for_filtering()) != 0, ""))


      tags$ol(
        tags$li(paste(top_flooding_drivers_ltla()$domain_variable_name[1])),
        tags$li(paste(top_flooding_drivers_ltla()$domain_variable_name[2])),
        tags$li(paste(top_flooding_drivers_ltla()$domain_variable_name[3]))
      )
    })


    # Dataset to return ----
    charities_data_subset <- reactive({

      # browser()

      charities_in_area_codes <- charities_ltla_lookup |>
        dplyr::filter(ltla21_name %in% ltlas_for_filtering()) |>
        distinct(organisation_number) |>
        pull()

      charities_working_in_top_flooding_drivers_codes <- charities_vuln_drivers_flood_lookup |>
        # Using input from renderUI
        dplyr::filter(variable_name %in% input$charity_top_vuln_drivers_chosen) |>
        distinct(organisation_number) |>
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

    # if(nrow(charities_data_subset()) == 0) {browser()}

    # Text to mask error messages to pass into the map and table ----
    # TO DO: Improve the language of these messages to make easier to understand
    error_text <- reactive({
      text <- c()


      if (length(ltlas_for_filtering()) == 0) {
        text <- c(
          text,
          "Please select an area on the first tab."
        )
      } else if (length(ltlas_for_filtering()) > 0 &&
        length(input$charity_top_vuln_drivers_chosen) == 0) {
        text <- c(
          text,
          "Please select vulnerability drivers from the dropdown."
        )
      } else if (length(ltlas_for_filtering()) > 0 &&
        length(input$charity_top_vuln_drivers_chosen) > 0 &&
        nrow(charities_data_subset()) == 0) {
        text <- c(
          text,
          "There are no charities which operate in the area which work in categories for the top vulnerability drivers you have selected."
        )
      } else if (length(ltlas_for_filtering()) > 0 &&
        length(input$charity_top_vuln_drivers_chosen) > 0 &&
        nrow(charities_data_subset()) > 0 &&
        nrow(filter(charities_data_subset(), flag_contact_in_ltla == 1)) == 0) {
        text <- c(
          text,
          "There are no charities whose work relates to the top vulnerability drivers you have selected with a contact address in your area."
        )
      } else if (length(ltlas_for_filtering()) > 0 &&
        length(input$charity_top_vuln_drivers_chosen) > 0 &&
        nrow(charities_data_subset()) > 0 &&
        nrow(filter(charities_data_subset(), flag_contact_in_ltla == 1)) > 0 &&
        nrow(filter(charities_data_subset(), flag_contact_in_ltla == 1, !is.na(lat), !is.na(long))) == 0) {
        text <- c(
          text,
          "There are no charities whose work relates to the top vulnerability drivers you have selected with a contact address in your area that have been able to map location. Please see table for results."
        )
      } else {
        text
      }
    })


    # Return 2 items from this module
    # https://mastering-shiny.org/scaling-modules.html?q=list#multiple-outputs
    list(
      data = reactive(charities_data_subset()),
      error_text = reactive(error_text())
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
#       ltlas_for_filtering = reactive(c("Hartlepool"))
#     )
#
#     #  observe(print(charities_subset$data()))
#     observe(print(charities_subset$error_text()))
#   }
#   shinyApp(ui, server)
# }
#
# # Run test
# subsetCharitiesDataTest()
