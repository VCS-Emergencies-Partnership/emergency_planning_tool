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

      lsoa_vuln_scores_sf_subset() |>
        leaflet() |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(
          # outline of polygon
          weight = 0.7,
          opacity = 0.5,
          color = "#5C747A",
          dashArray = "0.1",
          # fill of polygon
          fillColor = ~ pal(vulnerability_quantiles),
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#5C747A",
            dashArray = "",
            fillOpacity = 0.5,
            bringToFront = TRUE
          ),
          layerId = ~lsoa11_name,
          label = ~lsoa11_name,
          group = "base",
        ) |>
        addLegend(
          pal = pal,
          values = ~vulnerability_quantiles,
          opacity = 0.7,
          title = "Flood vulnerability index",
          position = "bottomright"
        )
    })


    observeEvent(input$flood_risk_checkbox,
      {
        if (input$flood_risk_checkbox == FALSE) {
          pal <- pal()

          leafletProxy("vuln_map") |>
            clearShapes() |>
            addProviderTiles(providers$CartoDB.Positron) |>
            addPolygons(
              data = lsoa_vuln_scores_sf_subset(),
              # outline of polygon
              weight = 0.7,
              opacity = 0.5,
              color = "#5C747A",
              dashArray = "0.1",
              # fill of polygon
              fillColor = ~ pal(vulnerability_quantiles),
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5,
                color = "#5C747A",
                dashArray = "",
                fillOpacity = 0.5,
                bringToFront = TRUE
              ),
              layerId = ~lsoa11_name,
              label = ~lsoa11_name,
              group = "base",
            )
        } else {
          pal <- pal()

          flood_risk_lsoas <- flood_risk_data |>
            filter(flood_risk == 1) |>
            pull(lsoa11_code)

          lsoa_vuln_scores_sf_subset_flood_risk <- reactive({
            lsoa_vuln_scores_sf_subset() |>
              filter(lsoa11_code %in% flood_risk_lsoas)
          })


          # if statement needed below to avoid error when addPolygons with an empty dataframe
          if (nrow(lsoa_vuln_scores_sf_subset_flood_risk()) > 0) {
            leafletProxy("vuln_map",
              data = lsoa_vuln_scores_sf_subset_flood_risk()
            ) |>
              clearShapes() |>
              addProviderTiles(providers$CartoDB.Positron) |>
              addPolygons(
                # outline of polygon
                weight = 0.7,
                opacity = 0.5,
                color = "#5C747A",
                dashArray = "0.1",
                # fill of polygon
                fillColor = ~ pal(vulnerability_quantiles),
                fillOpacity = 0.7,
                highlight = highlightOptions(
                  weight = 5,
                  color = "#5C747A",
                  dashArray = "",
                  fillOpacity = 0.5,
                  bringToFront = TRUE
                ),
                layerId = ~lsoa11_name,
                label = ~lsoa11_name,
                group = "base",
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
#   filter(ltla21_code == "E06000007") |>
#   select(lsoa11_code)
# 
# lsoa_vuln_scores_flood <- read_rds("data/flooding_vuln_scores_sf.rds")
# 
# lsoa_vuln_scores_subset_flood <- lsoa_vuln_scores_flood |>
#   inner_join(subset_lsoas, by = "lsoa11_code")
# 
# lsoa_flood_risk_ltla_lookup <- read_rds("data/lsoa_flood_risk_ltla_lookup.rds")
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
#       lsoas_clicked = lsoas_clicked
#     )
# 
#     observe(print(lsoas_clicked()))
#   }
#   shinyApp(ui, server)
# }
# 
# vulnMapTest()
