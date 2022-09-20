# UI -----
selectAreasDropdownUI <- function(id) {
  selectizeInput(
    NS(id, "selectAreasDropdown"),
    label = NULL,
    choices = sort(unique(lsoa_flood_risk_ltla_lookup$ltla21_name)),
    multiple = TRUE,
    options = list(
      plugins = list("remove_button"),
      placeholder = "Select Local Authorities...",
      onInitialize = I('function() { this.setValue(""); }')
    )
  )
}

# Server -----
selectAreasDropdownServer <- function(id, selected_ltlas) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$selectAreasDropdown,
      {
        selected_ltlas(input$selectAreasDropdown)
      },
      ignoreNULL = FALSE
    )

    # This sits in its own observer because it needs to track any changes to
    # the global `selected_ltlas$ltlas` reactive values, not just the selectizeInput
    observeEvent(selected_ltlas(), {
      updateSelectizeInput(
        session,
        "selectAreasDropdown",
        selected = selected_ltlas()
      )
    })
  })
}

#--------------------------------------------------------------------------------
# Test -------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# lsoa_flood_risk_ltla_lookup <- read_rds("data/lsoa_flood_risk_ltla_lookup.rds")
# 
# selectAreasDropdownTest <- function() {
#   ui <- fluidPage(
#     selectAreasDropdownUI("test")
#   )
#   server <- function(input, output, session) {
#     selected_ltlas <- reactiveVal(vector())
# 
#     selectAreasDropdownServer("test",
#                                selected_ltlas = selected_ltlas)
#   }
#   shinyApp(ui, server)
# }
# 
# selectAreasDropdownTest()
