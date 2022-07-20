# UI ----
topDriversTableUI <- function(id) {
  tagList(
    textOutput(NS(id, "lsoas_clicked_name")),
    tableOutput(NS(id, "top_drivers_table"))
  )
}

# Server ----
topDriversTableServer <- function(id, vuln_drivers, lsoas_clicked) {

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
            select(variable, value)
        })

        output$lsoas_clicked_name <- renderText(lsoas_clicked())
      },
      ignoreNULL = FALSE # means event triggered when the input (i.e. lsoa_clicked()) is NULL. Needed to trigger the validate message
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
#                      vuln_drivers = vuln_drivers_flood,
#                      lsoas_clicked = reactive(c("E01000001")))
#   }
#   shinyApp(ui, server)
# }
# # Run test
# topDriversTableTest()
