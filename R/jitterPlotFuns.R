# ---- Prepare selected data ----
jitter_plot_prep <- function(data,
                             lsoas_clicked) {
  data |>
    mutate(
      selected = if_else(
        lsoa11_name %in% lsoas_clicked(),
        lsoa11_name,
        paste0("Other neighbourhoods in the area")
      )
    )  |>
  mutate(alpha = if_else(selected != paste0("Other neighbourhoods in the area"), 1, 0.1)) |>
  mutate(selected = factor(selected)) |>
  mutate(selected = relevel(selected, ref = paste0("Other neighbourhoods in the area")))
}

# ---- ggplotly fun ----
ggplotly_default <- function(plot, annotation_y) {
  ggplotly(
    plot,
    tooltip = c("text")
  ) |>
    config(displayModeBar = FALSE) |>
    layout(
      xaxis = list(
        range = list(-0.02, 1.05),
        fixedrange = TRUE
        ),
      legend = list(
        orientation = "h",
        title = NA
        # x = 0,
        # y = 1.4
      )
    ) |>
    # add_annotations(
    #   x = 0.5,
    #   y = annotation_y,
    #   text = "Average",
    #   showarrow = F
    # ) |>
    add_annotations(
      x = 0.2,
      y = annotation_y,
      text = "◄ Lower flood risk",
      showarrow = F
    ) |>
    add_annotations(
      x = 0.8,
      y = annotation_y,
      text = "Higher flood risk ►",
      showarrow = F
    )
}

# ---- Plot while waiting for selection ----
jitter_plot_null <- function(data) {
  annotation_y <- 1 + 0.6

  plot <- ggplot(
    data,
    aes(
      x = eai,
      y = eai_flood,
      text =
          paste0(
            "<b>", lsoa11_name, "</b>",
            "<br>", eai, "<b>"
          )
    )
  ) +
    geom_point(
      position = position_jitter(height = 0.3, width = 0, seed = 123),
      size = 4,
      shape = 21,
      alpha = 0.1,
      fill = "#717171",
      colour = "#262626"
    ) +
    annotate(
      "segment",
      x = 0.5,
      xend = 0.5,
      y = 0.5,
      yend = annotation_y,
      colour = "#262626",
      linetype = "dashed",
      alpha = .5,
      size = .5
    ) +
    theme_minimal() +
    labs(x = NULL, y = NULL) +
    theme(axis.text.y=element_blank(),
          text = element_text(size = 12))

  ggplotly_default(plot, annotation_y)
}

# ---- Plot selected areas ----
jitter_plot_selected <- function(data,
                                 lsoas_clicked) {
  annotation_y <- 1 + 0.6

  plot <- ggplot(
    data,
    aes(
      x = eai,
      y = eai_flood,
      fill = selected,
      text = paste0(
        "<b>", lsoa11_name, "</b>",
        "<br>", eai, "<b>"
      )
    )
  ) +
    geom_point(
      aes(alpha = alpha),
      position = position_jitter(height = 0.3, width = 0, seed = 123),
      size = 4,
      shape = 21,
      colour = "#262626"
    ) +
    annotate(
      "segment",
      x = 0.5,
      xend = 0.5,
      y = 0.5,
      yend = annotation_y - 0.1,
      colour = "#262626",
      linetype = "dashed",
      alpha = .5,
      size = .5
    ) +
    theme_minimal() +
    scale_fill_manual(
      values = c("#D0021B", "#262626"),
      breaks = lsoas_clicked()
    ) +
    scale_x_continuous(limits = c(-0.02, 1)) +
    labs(x = NULL, y = NULL) +
    theme(axis.text.y=element_blank(),
          text = element_text(size = 12))

  ggplotly_default(plot, annotation_y)
}
