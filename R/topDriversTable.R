# UI ----
topDriversTableUI <- function(id) {
  tagList(
    "Neighbourhoods drivers of vulnerabilities ranked",
    textOutput(NS(id, "lsoas_clicked_name")),
    # actionButton(NS(id, "hide_show_val_col"),
    #              label = "Hide/show values"
   # ),
    tableOutput(NS(id, "top_drivers_table"))
  )
}

# Server ----
topDriversTableServer <- function(id, vuln_drivers, lsoas_clicked, selected_ltlas) {

  # Checks to ensure the inputs are reactive (data not reactive)
  stopifnot(is.reactive(lsoas_clicked))

  moduleServer(id, function(input, output, session) {
    observeEvent(
      list(lsoas_clicked(), input$hide_show_val_col),
      {
        output$top_drivers_table <- renderTable({

          # Message to user if no LSOAs selected
          validate(need(
            length(lsoas_clicked()) > 0,
            "Please click on a neighbourhood on the map to view the drivers of vulnerability to your chosen emergency event."
          ))

          drivers <- vuln_drivers |>
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

        #   # So only show values when user clicks the button
        #   if (input$hide_show_val_col[1] %% 2 == 0) {
        #     drivers_for_table <- drivers |>
        #       select(-Value)
        #   } else {
        #     drivers_for_table <- drivers
        #   }
        #   drivers_for_table

          drivers
         })

        output$lsoas_clicked_name <- renderText({
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

# load("data/vuln_drivers_flood.rda")
#
# topDriversTableTest <- function() {
#   ui <- fluidPage(
#     topDriversTableUI("test")
#   )
#   server <- function(input, output, session) {
#     topDriversTableServer("test",
#       selected_ltlas = reactive(c("City of London")),
#       vuln_drivers = vuln_drivers_flood,
#       lsoas_clicked = reactive(c("City of London 001A"))
#     )
#   }
#   shinyApp(ui, server)
# }
# # Run test
# topDriversTableTest()