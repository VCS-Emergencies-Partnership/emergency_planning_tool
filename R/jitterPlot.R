jitterPlotUI <- function(id) {
  tagList(
      div(
        style = "text-align: left; font-size: 120%",
        h2(strong("Flood risk")),
        h4("This section of the tool looks exclusively at the flood risk score of the neighbourhood.")
      ),
      plotlyOutput(
        NS(id, "plot")
      )
    )
}

jitterPlotServer <- function(id,
                             lsoa_vuln_scores_sf_subset,
                             lsoas_clicked) {
  moduleServer(id, function(input, output, session) {
          eai_data <- reactive({
            lsoa_vuln_scores_sf_subset() |>
              select(lsoa11_name, eai, eai_flood)

    })

    output$plot <- renderPlotly({
      if (is.null(lsoas_clicked)) {
        jitter_plot_null(data = eai_data())
      } else {
        jitter_plot_prep(data = eai_data(), lsoas_clicked) |>
          jitter_plot_selected(
            lsoas_clicked
          )
      }
    })
  })
}





# jitterPlotTest <- function() {
#   ui <- fluidPage(
#     jitterPlotUI("test")
#   )
#
#   server <- function(input, output, session) {
#     selected <- reactiveValues(
#       areas = vector(), geography = "ltla_shp_england"
#     )
#     jitterPlotServer("test", selected, type = "demographics_age")
#   }
#
#   shinyApp(ui, server)
# }

# Examples
# jitterPlotTest()
