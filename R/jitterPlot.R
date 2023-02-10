jitterPlotUI <- function(id) {
  tagList(
      div(
        style = "text-align: left; font-size: 120%",
        h4(strong("Flood risk")),
        p("This section of the tool looks exclusively at the flood risk score of the neighbourhood, showing the expected annual probability of flooding.")
      ),
      plotlyOutput(
        NS(id, "plot")
      )
    )
}

jitterPlotServer <- function(id,
                             lsoa_vuln_scores_sf_subset,
                             lsoas_clicked,
                             selected_ltlas) {

  moduleServer(id, function(input, output, session) {
          eai_data <- reactive({
            lsoa_vuln_scores_sf_subset() |>
              select(lsoa11_name, eai, eai_flood)

    })

    output$plot <- renderPlotly({
      # Message to user if no LTLA selected ----
      # Catch errors if no area has been selected - leave blank as captured in 'top_drivers_table_domains' module
      validate(need(
        length(selected_ltlas()) > 0,
        ""
      ))

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
