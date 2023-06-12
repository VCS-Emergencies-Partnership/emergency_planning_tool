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
      fillColor = ~ pal_input(class_cleaned),
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

#' Function to standardise a set of standardised and summed columns
#' #'
#' @param data The data
#' @param id_columns The id columns
#' @param calc_columns The columns to perform calculations across
#'
standarise_summed_standarise_cols <- function(data, id_columns, calc_columns) {

  data |>
    select(all_of({{ id_columns }}), all_of({{ calc_columns }})) |>
    mutate(across(all_of({{ calc_columns }}), standardise)) |>
    standarise_summed_cols(id_columns = id_columns, calc_columns = calc_columns)

}

#' Function to standardise summed columns
#' #'
#' @param data The data
#' @param id_columns The id columns
#' @param calc_columns The columns to perform calculations across
#'
standarise_summed_cols <- function(data, id_columns, calc_columns) {
  data |>
    select(all_of({{ id_columns }}), all_of({{ calc_columns }})) |>
    rowwise(all_of({{ id_columns }})) |>
    summarise(sum = sum(c_across(all_of({{ calc_columns }})))) |>
    ungroup() |>
    mutate(sum_stand = standardise(sum)) |>
    select(sum_stand) |>
    pull()

}
