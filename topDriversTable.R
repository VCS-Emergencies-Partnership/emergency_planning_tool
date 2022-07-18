# UI ----
topDriversTableOutput <- function(id) {
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
          vuln_drivers |>
            dplyr::filter(lsoa11_name %in% lsoas_clicked()) |>
            select(variable, value)
        })

        output$lsoas_clicked_name <- renderText(lsoas_clicked())
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
#     topDriversTableOutput("test")
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
