#' Set them
#' @importFrom ggplot2 theme element_text
#' @importFrom ggtext element_markdown
#' @export
EpicovrTheme <- function() {
  themex <- hrbrthemes::theme_ipsum() + theme(
    axis.text = element_text(size = 9, color = "black"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
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
#' @importFrom ggplot2 aes_string coord_cartesian ggplot geom_bar geom_text labs scale_y_continuous scale_x_discrete xlab ylab guide_axis
#' @importFrom scales comma label_percent label_number cut_short_scale
#' @importFrom ggtext element_markdown
#' @importFrom patchwork wrap_plots
#' @export
BarPlot <- function(df, xaxis = "MonthYear", yaxis = "value", color = "dodgerblue2",
                    ylabel = NULL, label_si = FALSE,
                    label_as_percent = TRUE, xangle = 90, title = NULL, caption = NULL) {
  df <- as.data.frame(df)
  df$MonthYearFactor <- as.factor(df[, xaxis])
  values <- df %>% pull(!!yaxis)
  values[is.nan(values)] <- NA
  df[, yaxis] <- values
  is_int <- (values %% 1 == 0)
  is_int <- all(is_int, na.rm = TRUE)
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
        position = position_dodge(width = 0.9),
        hjust = -0.25,
        angle = 90,
        size = 3
      )
    } else {
      df[, paste0(yaxis, "_acc")] <- label_number(accuracy = 1, scale_cut = cut_short_scale())(values)
      p <- p + geom_text(
        data = df,
        mapping = aes_string(
          label = paste0(yaxis, "_acc")
        ),
        position = position_dodge(width = 0.9),
        hjust = -0.25,
        angle = 90,
        size = 3
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
      position = position_dodge(width = 0.9),
      hjust = -0.25,
      angle = 90,
      size = 3
    )
  }
  p <- p + scale_y_continuous(labels = label_number(accuracy = 1, scale_cut = cut_short_scale())) +
    scale_x_discrete(guide = guide_axis(angle = xangle)) +
    xlab("") +
    ylab(ylabel) +
    EpicovrTheme() +
    coord_cartesian(clip = "off") +
    ggtitle(title) +
    labs(caption = caption)
  return(wrap_plots(p))
}

#' @importFrom ggplot2 ggplot geom_bar labs scale_fill_brewer scale_x_discrete xlab ylab guide_axis
#' @importFrom scales label_number cut_short_scale
#' @importFrom ggtext element_markdown
#' @importFrom patchwork wrap_plots
#' @export
StackedBarPlotPrevalence <- function(prevalence_df, xangle = 90) {
  p <- ggplot(
    data = prevalence_df,
    aes(
      x = MonthYearCollectedFactor,
      y = prevalence,
      fill = lineage_collapsed
    )
  ) +
    geom_bar(stat = "identity") +
    scale_fill_brewer(type = "qual", palette = "Set3", name = "Pangolin lineage") +
    EpicovrTheme() +
    xlab("Date collected") +
    ylab("% composition of variant") +
    labs(caption = "**Source:** gisaid.org<br>") +
    scale_x_discrete(guide = guide_axis(angle = xangle))
  return(wrap_plots(p))
}

#' @importFrom ggplot2 ggplot facet_wrap geom_area scale_fill_brewer scale_x_date xlab ylab guide_axis
#' @importFrom scales label_number cut_short_scale
#' @importFrom ggtext element_markdown
#' @importFrom patchwork wrap_plots
#' @export
#'
PlotMullerDailyPrevalence <- function(df, ncol = 4) {
  p <- ggplot(
    data = df,
    aes(x = DateCollected, y = prob, group = lineage_collapsed)
  ) +
    facet_wrap(~State, ncol = ncol) +
    geom_area(aes(lwd = I(1.2), colour = NULL, fill = lineage_collapsed, group = lineage_collapsed), position = "stack") +
    scale_fill_brewer(type = "qual", palette = "Set3", name = "Pangolin lineage") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
  wrap_plots(p)
}

#' @importFrom ggplot2 ggplot geom_line geom_label scale_fill_brewer scale_y_continuous xlab ylab labs ggtitle guide_axis theme element_text
#' @importFrom scales label_number cut_short_scale
#' @importFrom gganimate transition_reveal view_follow animate gifski_renderer
#' @importFrom tsibble scale_x_yearweek
#' @importFrom ggtext element_markdown
#' @importFrom patchwork wrap_plots
#' @export
#'
PlotVariantPrevalenceAnimated <- function(df, title = NULL, caption = "**Source: gisaid.org<br>**") {
  color_values <- as.character(x = paletteer::paletteer_d("ggsci::default_igv"))
  the_anim <- ggplot(
    df,
    aes(x = WeekYearCollected, color = variant, y = value, group = variant)
  ) +
    geom_line() +
    scale_x_yearweek(date_breaks = "1 month", date_labels = "%b %Y", guide = guide_axis(angle = 90)) +
    scale_y_continuous(label = label_number(accuracy = 1, scale_cut = cut_short_scale())) +
    geom_label(hjust = 0, aes(label = variant), nudge_x = 10, show.legend = FALSE) +
    geom_point() +
    coord_cartesian(ylim = c(0, NA), clip = "off") +
    # scale_color_brewer(type = "qual", palette = "Paired", name = "Variant") +
    scale_color_manual(values = color_values, name = "Variant") +
    xlab("") +
    ylab("\nAverage weekly cases\n") +
    ggtitle(title) +
    labs(
      subtitle = "Estimation based on a multinomial fit to weekly genomic surveillance data deposited to GISAID",
      caption = caption
    ) +
    theme(legend.position = "bottom", axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
  anim <- the_anim + transition_reveal(Date) + view_follow(fixed_y = c(0, NA), fixed_x = T)
  anim <- animate(anim, renderer = gifski_renderer(), height = 900, width = 1100, res = 150, nframes = 300, rewind = T, end_pause = 30)
  return(anim)
}
