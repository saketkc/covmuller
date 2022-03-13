#' Set them
#' @importFrom ggplot2 theme element_text
#' @importFrom ggtext element_markdown
#' @export
EpicovrTheme <- function() {
  themex <- hrbrthemes::theme_ipsum() + theme(
    axis.text = element_text(size = 10, color = "black"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(angle = 20),
    legend.position = "bottom", legend.direction = "horizontal",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_markdown(hjust = 0),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    strip.text.x = element_text(size = 12, face = "bold", hjust = 0, color = "#225ea8"),
    strip.background = element_blank()
  )
  return(themex)
}

#' Plot heatmap of cases sequenced
#' @param cases_and_shared A dataframe containing counts of confirmed cases and sequenced cases
#' @returns A heatmap of total percentage of cases sequenced
#' @export
#' @importFrom ggplot2 ggplot geom_tile geom_text scale_color_identity
#' scale_fill_gradient2 labs guides theme ylab xlab element_text element_blank
#' scale_x_discrete
#' @importFrom hrbrthemes theme_ipsum
#' @importFrom ggtext element_markdown
PlotSequencedPropHeatmap <- function(df) {
    p <- ggplot(df, aes(MonthYear,
    State,
    fill = percent_sequenced_toplot
  )) +
    geom_tile(color = "black") +
    geom_text(aes(
      label = round(percent_sequenced_collected, 1),
      color = ifelse(percent_sequenced_collected > 2,
        "white", "black"
      )
    )) +
    scale_color_identity() +
    scale_fill_gradient2(
      low = "red",
      mid = "white",
      high = "#1c9099",
      midpoint = 0.1,
      space = "Lab",
      na.value = "grey50",
      name = "% cases sequenced and shared"
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    xlab("Month collected") +
    ylab("") +
    EpicovrTheme()

  return(p)
}
