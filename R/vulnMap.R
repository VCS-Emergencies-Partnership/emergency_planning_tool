# UI -----
vulnMapUI <- function(id) {
  tagList(
    checkboxInput(NS(id, "flood_risk_checkbox"),
      label = "Only show flood risk neighbourhoods"
    ),
    leafletOutput(
      NS(id, "vuln_map"),
      height = 630
    )
  )
}

# Server -----
vulnMapServer <- function(id, lsoa_vuln_scores_sf_subset, flood_risk_data, lsoas_clicked) {

  # Checks to ensure the input is reactive (data input not reactive)
  stopifnot(is.reactive(lsoa_vuln_scores_sf_subset))

  moduleServer(id, function(input, output, session) {

    # Once loaded .rda file no longer recognised as spatial object?
    lsoa_vuln_scores_sf_subset_clean <- reactive({

      lsoa_vuln_scores_sf_subset() |>
      st_as_sf(crs = 4326) |>
      rename(vulnerability_quantiles = nvfi_quantiles_eng)
    })



    pal <- reactive({
      colorBin("inferno",
        reverse = TRUE,
        domain = lsoa_vuln_scores_sf_subset_clean()$vulnerability_quantiles,
        bins = c(1:10)
      )
    })


    output$vuln_map <- renderLeaflet({

      # Catch errors if no area has been selected - blank message as not at top of the page
      validate(need(nrow(lsoa_vuln_scores_sf_subset_clean()) != 0, ""))

      legend_labels = c("Least vulnerable", rep("", times = 7) , "Most vulnerable")

      pal <- pal()

      leaflet() |>
        addProviderTiles(providers$CartoDB.Positron) |>
        vuln_map_function(
          vuln_data = lsoa_vuln_scores_sf_subset_clean(),
          pal_input = pal
        ) |>
        addLegend(
          data = lsoa_vuln_scores_sf_subset_clean(),
          pal = pal,
          values = ~vulnerability_quantiles,
          opacity = 0.7,
          title = "Flood vulnerability",
          position = "bottomright",
          # To change from numeric values to 'most/least vulnerable labels'
          labFormat = function(type, cuts, p) {
            paste0(legend_labels)
          }
        )
    })


    observeEvent(input$flood_risk_checkbox,
      {
        if (input$flood_risk_checkbox == FALSE) {
          pal <- pal()

          leafletProxy("vuln_map") |>
            clearShapes() |>
            addProviderTiles(providers$CartoDB.Positron) |>
            vuln_map_function(
              vuln_data = lsoa_vuln_scores_sf_subset_clean(),
              pal_input = pal
            )
        } else {

          pal <- pal()

          flood_risk_lsoas <- flood_risk_data |>
            filter(flood_risk == 1) |>
            pull(lsoa11_code)

          lsoa_vuln_scores_sf_subset_flood_risk <- reactive({
            lsoa_vuln_scores_sf_subset_clean() |>
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
#   filter(ltla21_code == "E06000001") |>
#   select(lsoa11_code)
#
# load("data/vuln_scores_flood.rda")
# load("data/lsoa_flood_risk_ltla_lookup.rda")
#
# lsoa_vuln_scores_subset_flood <- vuln_scores_flood |>
#   inner_join(subset_lsoas, by = "lsoa11_code")
#
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
#                   lsoa_vuln_scores_sf_subset = reactive(lsoa_vuln_scores_subset_flood),
#                   flood_risk_data = lsoa_flood_risk_ltla_lookup,
#                   lsoas_clicked = lsoas_clicked
#     )
#
#     observe(print(lsoas_clicked()))
#   }
#   shinyApp(ui, server)
# }
#
# vulnMapTest()
