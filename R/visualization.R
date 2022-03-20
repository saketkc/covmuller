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
#' @importFrom ggplot2 ggplot geom_tile geom_text scale_color_identity
#' scale_fill_gradient2 labs guides theme ylab xlab element_text element_blank
#' scale_x_discrete
#' @importFrom hrbrthemes theme_ipsum
#' @importFrom ggtext element_markdown
#' @importFrom patchwork wrap_plots
#' @export
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

  return(wrap_plots(p))
}

#' @importFrom magrittr %>%
#' @importFrom tibble deframe
#' @importFrom ggplot2 coord_cartesian ggplot geom_bar geom_text scale_y_continuous scale_x_discrete xlab ylab guide_axis
#' @importFrom scales comma label_percent label_number_si
#' @importFrom ggtext element_markdown
#' @importFrom patchwork wrap_plots
#' @export
BarPlot <- function(df, yaxis = "value", color = "dodgerblue2", ylabel = NULL, label_si = FALSE,
                    label_as_percent = TRUE) {
  df$MonthYearFactor <- as.factor(df$MonthYear)
  values <- df %>% pull(!!yaxis)
  is_int <- (values[length(values)] %% 1 == 0)
  p <- ggplot(df, aes_string("MonthYearFactor", yaxis)) +
    geom_bar(stat = "identity", fill = color)
  if (is_int) {
    if (!label_si) {
      df[, paste0(yaxis, "_acc")] <- comma(x = values, accuracy = 1)
      p <- p + geom_text(
        data = df,
        mapping = aes_string(
          label = paste0(yaxis, "_acc")
        ),
        position = position_dodge(width = 0.9), vjust = -0.25
      )
    } else {
      df[, paste0(yaxis, "_acc")] <- label_number_si(accuracy = 1)(values)
      p <- p + geom_text(
        data = df,
        mapping = aes_string(
          label = paste0(yaxis, "_acc")
        ),
        position = position_dodge(width = 0.9), vjust = -0.25
      )
    }
  } else {
    if (label_as_percent) {
      df[, paste0(yaxis, "_acc")] <- label_percent(accuracy = .1)(values / 100)
    } else {
      df[, paste0(yaxis, "_acc")] <- round(x = values, digits = 2)
    }

    p <- p + geom_text(
      data = df,
      mapping = aes_string(
        label = paste0(yaxis, "_acc")
      ),
      position = position_dodge(width = 0.9), vjust = -0.25
    )
  }
  p <- p + scale_y_continuous(labels = label_number_si(accuracy = 0.1)) +
    scale_x_discrete(guide = guide_axis(angle = 30)) +
    xlab("") +
    ylab(ylabel) +
    EpicovrTheme() +
    coord_cartesian(clip = "off")
  return(wrap_plots(p))
}

#' @importFrom ggplot2 ggplot geom_bar labs scale_fill_brewer scale_x_discrete xlab ylab guide_axis
#' @importFrom scales label_number_si
#' @importFrom ggtext element_markdown
#' @importFrom patchwork wrap_plots
#' @export
StackedBarPlotPrevalence <- function(prevalence_df) {
  p <- ggplot(
    data = prevalence_df,
    aes(
      x = MonthYearCollectedFactor,
      y = prevalence,
      fill = lineage_collapsed
    )
  ) +
    geom_bar(stat = "identity") +
    scale_fill_brewer(type = "qual", name = "Pangolin lineage") +
    EpicovrTheme() +
    xlab("Date collected") +
    ylab("% composition of variant") +
    labs(caption = "**Source:** gisaid.org<br>") +
    scale_x_discrete(guide = guide_axis(angle = 30))
  return(wrap_plots(p))
}
