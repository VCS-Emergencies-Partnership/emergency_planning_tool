# UI -----
vulnMapUI <- function(id) {
  tagList(
    radioButtons(NS(id, "flood_risk_radio_button"),
      label = "",
      choices = c(
        "Show all neighbourhoods" = "all",
        "Only show neighbourhoods at flood risk" = "risk",
        "Only show  neighbourhoods with current flood alerts" = "alert"
      ),
      selected = "all"
    ),
    # Dropdown to select different levels of flood alerts
    # TO DO: If use this code then only make this drop down visible when 
    # above radio button is set at 'alert' 
    pickerInput(
      NS(id, "flood_alert_level_choice"),
      label = "Levels of flood risk:",
      choices =
        c(
          "Flooding is Possible, Be Prepared" = 3,
          "Flooding is Expected, Immediate Action Required" = 2,
          "Severe Flooding, Danger to Life" = 1
        ),
      selected = c(1, 2, 3),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    ),
    leafletOutput(
      NS(id, "vuln_map"),
      height = 630
    )
  )
}

# Server -----
vulnMapServer <- function(id,
                          lsoa_vuln_scores_sf_subset,
                          flood_risk_data,
                          flood_alert_data,
                          lsoas_clicked) {

  # Checks to ensure the input is reactive (data input not reactive)
  stopifnot(is.reactive(lsoa_vuln_scores_sf_subset))

  moduleServer(id, function(input, output, session) {
    pal <- reactive({
      colorBin("Reds",
        domain = lsoa_vuln_scores_sf_subset()$vulnerability_quantiles,
        bins = c(1:10)
      )
    })


    output$vuln_map <- renderLeaflet({

      # Catch errors if no area has been selected - blank message as not at top of the page
      validate(need(nrow(lsoa_vuln_scores_sf_subset()) != 0, ""))

      pal <- pal()

      #    lsoa_vuln_scores_sf_subset() |>
      leaflet() |>
        addProviderTiles(providers$CartoDB.Positron) |>
        vuln_map_function(
          vuln_data = lsoa_vuln_scores_sf_subset(),
          pal_input = pal
        ) |>
        addLegend(
          data = lsoa_vuln_scores_sf_subset(),
          pal = pal,
          values = ~vulnerability_quantiles,
          opacity = 0.7,
          title = "Flood vulnerability index",
          position = "bottomright"
        )
    })


    observeEvent(c(input$flood_risk_radio_button, input$flood_alert_level_choice),
      {
        if (input$flood_risk_radio_button == "all") {
          pal <- pal()

          leafletProxy("vuln_map") |>
            clearShapes() |>
            addProviderTiles(providers$CartoDB.Positron) |>
            vuln_map_function(
              vuln_data = lsoa_vuln_scores_sf_subset(),
              pal_input = pal
            )
        } else if (input$flood_risk_radio_button == "risk") {
          pal <- pal()

          lsoa_vuln_scores_sf_subset_flood_risk <- reactive({
            flood_risk_lsoas <- flood_risk_data |>
              filter(flood_risk == 1) |>
              pull(lsoa11_code)

            lsoa_vuln_scores_sf_subset() |>
              filter(lsoa11_code %in% flood_risk_lsoas)
          })


          # if statement needed below to avoid error when addPolygons with an empty dataframe
          if (nrow(lsoa_vuln_scores_sf_subset_flood_risk()) > 0) {
            leafletProxy("vuln_map") |>
              clearShapes() |>
              addProviderTiles(providers$CartoDB.Positron) |>
              vuln_map_function(
                vuln_data = lsoa_vuln_scores_sf_subset_flood_risk(),
                pal_input = pal
              )
          } else {
            leafletProxy("vuln_map") |>
              clearShapes() |>
              addProviderTiles(providers$CartoDB.Positron)
          }
        } else {
          pal <- pal()

          flood_alert_data_filter <- reactive({
            flood_alert_lsoas <- flood_alert_data |>
              filter(severityLevel %in% input$flood_alert_level_choice) |>
              distinct(lsoa11_code) |>
              pull()

            lsoa_vuln_scores_sf_subset() |>
              filter(lsoa11_code %in% flood_alert_lsoas)
          })

          if (nrow(flood_alert_data_filter()) > 0) {
            leafletProxy("vuln_map") |>
              clearShapes() |>
              addProviderTiles(providers$CartoDB.Positron) |>
              vuln_map_function(
                vuln_data = flood_alert_data_filter(),
                pal_input = pal
              )
          } else {
            leafletProxy("vuln_map") |>
              clearShapes() |>
              addProviderTiles(providers$CartoDB.Positron)
          }
        }
      },
      ignoreInit = TRUE
    )


    # Update the reactive value lsoa_selected() with what LSOA the user selects
    observeEvent(input$vuln_map_shape_click,
      {
        lsoas_clicked(input$vuln_map_shape_click$id)
      },
      ignoreNULL = FALSE
    )
  })
}

#--------------------------------------------------------------------------------
# Test -------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# library(geographr)
#
# subset_lsoas <- lookup_lsoa11_ltla21 |>
#   filter(ltla21_code == "E06000059") |>
#   select(lsoa11_code)
#
# lsoa_vuln_scores_flood <- read_rds("data/flooding_vuln_scores_sf.rds")
#
# lsoa_vuln_scores_subset_flood <- lsoa_vuln_scores_flood |>
#   inner_join(subset_lsoas, by = "lsoa11_code")
#
# lsoa_flood_risk_ltla_lookup <- read_rds("data/lsoa_flood_risk_ltla_lookup.rds")
#
# lsoa_flood_alert_data <- read_rds("data/flood_alert_lsoas.Rds")
#
# vulnMapTest <- function() {
#   ui <- fluidPage(
#     vulnMapUI("test"),
#   )
#   server <- function(input, output, session) {
#
#     # Variable to store the LSOA of clicked
#     lsoas_clicked <- reactiveVal()
#
#     vulnMapServer("test",
#       lsoa_vuln_scores_sf_subset = reactive(lsoa_vuln_scores_subset_flood),
#       flood_risk_data = lsoa_flood_risk_ltla_lookup,
#       flood_alert_data = lsoa_flood_alert_data,
#       lsoas_clicked = lsoas_clicked
#     )
#
#     observe(print(lsoas_clicked()))
#   }
#   shinyApp(ui, server)
# }
#
# vulnMapTest()
