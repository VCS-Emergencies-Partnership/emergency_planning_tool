# UI ----
topDriversTableUI <- function(id) {
  tagList(
    "Neighbourhoods drivers of vulnerabilities ranked",
    textOutput(NS(id, "lsoas_clicked_name")),
    tableOutput(NS(id, "top_drivers_table"))
  )
}

# Server ----
topDriversTableServer <- function(id, vuln_drivers, lsoas_clicked, selected_ltlas) {

  # Checks to ensure the inputs are reactive (data not reactive)
  stopifnot(is.reactive(lsoas_clicked))

  moduleServer(id, function(input, output, session) {
    observeEvent(
      lsoas_clicked(),
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
            mutate(variable_quantiles = case_when(
              variable_quantiles <= 5 ~ paste0("in ", variable_quantiles, "0% most vulnerable neighbourhoods"),
              variable_quantiles > 5 ~ paste0("in ", 11-variable_quantiles, "0% least vulnerable neighbourhoods"),
              )) |>
            select(
              `Rank` = normalised_rank,
              `Driver of flooding vulnerability` = variable,
              Value = value,
              `Comparison of value nationally` = variable_quantiles
            ) |> 
            arrange(Rank)
            
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

# vuln_drivers_flood <- read_rds("data/flooding_drivers.rds")
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
