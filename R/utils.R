#' Function to add Polygons of neighborhood vulnerability onto a base map 
#'
#' @param base_map_input The base map 
#' @param vuln_data The dataset to be used for the polygons
#' This dataset should have columns 'vulnerability_quantiles' & 'lsoa11_name' 
#' @param pal_input The pallette to be used for the polygons
#' 
vuln_map_function <- function (base_map_input, vuln_data, pal_input) {
  base_map_input |>
    addPolygons(
      data = vuln_data,
      # outline of polygon
      weight = 0.7,
      opacity = 0.5,
      color = "#5C747A",
      dashArray = "0.1",
      # fill of polygon
      fillColor = ~ pal_input(vulnerability_quantiles),
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
}