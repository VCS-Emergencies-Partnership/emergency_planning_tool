# UI -----
vulnMapUI <- function(id) {
  leafletOutput(
    NS(id, "vuln_map"),
    height = 630
  )
}

# Server -----
vulnMapServer <- function(id, lsoa_vuln_scores_sf_subset, lsoas_clicked) {

  # Checks to ensure the input is reactive (data input not reactive)
  stopifnot(is.reactive(lsoa_vuln_scores_sf_subset))

  moduleServer(id, function(input, output, session) {


    output$vuln_map <- renderLeaflet({
      
      pal <- colorBin("Reds", domain = lsoa_vuln_scores_sf_subset()$vulnerability_quantiles, bins = c(1:10))
      
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
  
# subset_lsoas <- lookup_lsoa11_ltla21 |>
#   filter(ltla21_code == "E06000001") |>
#   select(lsoa11_code)
# 
# lsoa_vuln_scores_flood <- read_rds("data/flooding_vuln_scores_sf.rds")
# 
# lsoa_vuln_scores_subset_flood <- lsoa_vuln_scores_flood |>
#   inner_join(subset_lsoas, by = "lsoa11_code")
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
#       lsoas_clicked = lsoas_clicked
#     )
# 
#     observe(print(lsoas_clicked()))
#   }
#   shinyApp(ui, server)
# }
# 
# vulnMapTest()
# 
