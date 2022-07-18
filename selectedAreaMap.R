# UI -----
selectedAreaMapUI <- function(id) {
  leafletOutput(
    NS(id, "map"),
    height = 630
  )
}

# Server -----
selectedAreaMapServer <- function(id, boundaries_data, selected_ltlas) {
  moduleServer(id, function(input, output, session) {
    output$map <-
      renderLeaflet({
        leaflet() |>
          setView(lat = 52.75, lng = -2.0, zoom = 6) |>
          addProviderTiles(
            providers$CartoDB.Positron,
            options = providerTileOptions(minZoom = 6)
          ) |>
          setMaxBounds(-12, 49, 3.0, 61) |>
          addPolygons(
            data = boundaries_data,
            layerId = ~ltla21_name,
            group = "base",
            label = ~ltla21_name,
            weight = 0.7,
            opacity = 0.5,
            color = "#5C747A",
            dashArray = "0.1",
            fillOpacity = 0.2,
            highlight = highlightOptions(
              weight = 5,
              color = "#5C747A",
              dashArray = "",
              fillOpacity = 0.2,
              bringToFront = TRUE
            )
          ) |>
          addPolygons(
            data = boundaries_data,
            layerId = ~ltla21_code,
            # Create number of groups equal to the length of the number of areas
            # Match group names to layerId above so that group visibility can
            # be toggled on/off
            group = ~ltla21_name,
            label = ~ltla21_name,
            weight = 0.7,
            opacity = 0.5,
            color = "#D0021B",
            dashArray = "0.1",
            fillOpacity = 0.4,
            highlight = highlightOptions(
              weight = 5,
              color = "#D0021B",
              dashArray = "",
              fillOpacity = 0.4,
              bringToFront = TRUE
            )
          ) |>
          hideGroup(boundaries_data$ltla21_name)
      })

    # Logic: create two sets of reactive values. One contained to the module
    # namespace (`clicked`), and one to the global namespace (`selected`)

    # Contained to local namespace
    clicked_ltlas <- reactiveValues(ltlas = vector())

    # As polygons are clicked, update both the local and global reactive values
    observeEvent(input$map_shape_click, {
      if (input$map_shape_click$group == "base") {
        selected_ltlas$ltlas <- c(selected_ltlas$ltlas, input$map_shape_click$id)
        clicked_ltlas$ltlas <- c(clicked_ltlas$ltlas, input$map_shape_click$id)
        leafletProxy("map") |> showGroup(input$map_shape_click$id)
      } else {
        selected_ltlas$ltlas <- setdiff(selected_ltlas$ltlas, input$map_shape_click$group)
        clicked_ltlas$ltlas <- setdiff(clicked_ltlas$ltlas, input$map_shape_click$group)
        leafletProxy("map") |> hideGroup(input$map_shape_click$group)
      }
    })

    # Track differences in the local and global reactive values. If activity has
    # occurred outside of this module, update the local reactive values to match
    # the global reactive values and update the map polygons
    observeEvent(selected_ltlas$ltlas,
      {
        removed <- setdiff(clicked_ltlas$ltlas, selected_ltlas$ltlas)
        added <- setdiff(selected_ltlas$ltlas, clicked_ltlas$ltlas)

        leafletProxy("map") |>
          hideGroup(removed) |>
          showGroup(added)

        clicked_ltlas$ltlas <- selected_ltlas$ltlas
      }
    )
  })
}

#--------------------------------------------------------------------------------
# Test -------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# boundaries_ltlas <- read_rds("data/boundaries_ltlas.rds")
# 
# selectedAreaMapTest <- function() {
#   ui <- fluidPage(
#     selectedAreaMapUI("test")
#   )
#   server <- function(input, output, session) {
#     selected_ltlas <- reactiveValues(ltlas = c())
# 
#     selectedAreaMapServer("test", 
#                           boundaries_data = boundaries_ltlas,
#                           selected_ltlas = selected_ltlas)
#   }
#   shinyApp(ui, server)
# }
# 
# selectedAreaMapTest()
