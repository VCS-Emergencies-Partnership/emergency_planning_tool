# UI ----
topDriversTableUI <- function(id) {
  tagList(
    textOutput(NS(id, "lsoas_clicked_name")),
    br(),
    fluidRow(box(
      tableOutput(NS(id, "top_drivers_table_domains")),
      title = "Neighbourhood drivers of vulnerabilities ranked - Domains",
      solidHeader = TRUE,
      width = 11,
      status = "primary",
      collapsible = TRUE
    )),
    fluidRow(box(
      tableOutput(NS(id, "top_drivers_table_variables")),
      title = "Neighbourhood drivers of vulnerabilities ranked - Indicators",
      solidHeader = TRUE,
      width = 11,
      status = "primary",
      collapsible = TRUE
    ))
  )
}

# Server ----
topDriversTableServer <- function(id,
                                  vuln_drivers,
                                  lsoas_clicked,
                                  selected_ltlas) {

  # Checks to ensure the inputs are reactive (data not reactive)
  stopifnot(is.reactive(lsoas_clicked))

  moduleServer(id, function(input, output, session) {
    observeEvent(
      lsoas_clicked(),
      {
        top_drivers_data <- reactive({
          vuln_drivers |>
            dplyr::filter(lsoa11_name %in% lsoas_clicked()) |>
            # explain the concept of quantiles in plain language
            # variable_quantiles = 1 means in top 10% worst scoring neighborhoods nationally
            mutate(quantiles_eng = case_when(
              quantiles_eng <= 5 ~ paste0("in ", quantiles_eng, "0% most vulnerable neighbourhoods"),
              quantiles_eng > 5 ~ paste0("in ", 11 - quantiles_eng, "0% least vulnerable neighbourhoods"),
            )) |>
            select(
              `Rank` = normalised_rank,
              `Driver of flooding vulnerability` = domain_variable_name,
              `Domain or variable` = domain_variable,
              `Comparison of value nationally` = quantiles_eng
              #     `Values` = values
            ) |>
            arrange(`Domain or variable`, Rank) |>
            mutate(Rank = if_else(is.na(Rank), "-", as.character(Rank))) |>
            mutate(`Comparison of value nationally` = if_else(is.na(`Comparison of value nationally`), "No data available", `Comparison of value nationally`))
        })

        output$top_drivers_table_domains <- renderTable({

          # Message to user if no LTLA selected ----
          # Catch errors if no area has been selected - leave blank as captured in 'topVuln' module
          validate(need(
            length(selected_ltlas()) > 0,
            ""
          ))

          # Message to user if no LSOA clicked ----
          validate(need(
            length(lsoas_clicked()) > 0,
            "Please click on a neighbourhood on the map to view the drivers of vulnerability to your chosen emergency event."
          ))

          top_drivers_data() |>
            filter(`Domain or variable` == "domain") |>
            select(-`Domain or variable`)
        })


        output$top_drivers_table_variables <- renderTable({

          # Message to user if no LTLA selected ----
          # Catch errors if no area has been selected - leave blank as captured in 'top_drivers_table_domains' module
          validate(need(
            length(selected_ltlas()) > 0,
            ""
          ))

          # Message to user if no LSOA clicked ----
          # Catch errors if no area has been selected - leave blank as captured in 'top_drivers_table_domains' module
          validate(need(
            length(lsoas_clicked()) > 0,
            ""
          ))

          top_drivers_data() |>
            filter(`Domain or variable` == "variable") |>
            select(-`Domain or variable`)
        })

        output$lsoas_clicked_name <- renderText({

          # Message to user if no LSOAs selected ----
          # Blank since error message captured in 'top_drivers_table' object
          validate(need(
            length(lsoas_clicked()) > 0,
            ""
          ))

          paste("Neighbourhood: ", lsoas_clicked())
        })
      },
      ignoreNULL = FALSE # means event triggered when the input (i.e. lsoa_clicked()) is NULL. Needed to trigger the validate message
    )

    observeEvent(
      selected_ltlas(),
      {
        lsoas_clicked(NULL)
      }
    )
  })
}

# --------------------------------------------------------------------------------
# Test -------------------------------------------------------------------------
# --------------------------------------------------------------------------------
#
# load("data/vuln_drivers_flood_lsoa.rda")
#
# topDriversTableTest <- function() {
#   ui <- fluidPage(
#     topDriversTableUI("test")
#   )
#   server <- function(input, output, session) {
#     topDriversTableServer("test",
#       selected_ltlas = reactive(c("City of London")),
#       vuln_drivers = vuln_drivers_flood_lsoa,
#       lsoas_clicked = reactive(c("City of London 001A"))
#     )
#   }
#   shinyApp(ui, server)
# }
# # Run test
# topDriversTableTest()
