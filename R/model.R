#' @importFrom magrittr %>%
#' @importFrom tibble deframe
#' @importFrom nnet multinom
FitMultinom <- function(data,
                        formula = as.formula("lineage_collapsed ~ DateCollectedNumeric + State"),
                        weights = "total_sequences") {
  wt <- data %>% pull(!!weights) # %>% deframe()
  print(wt)
  fit <- multinom(
    formula = formula,
    data = data,
    weights = wt,
    maxit = 1000
  )
  return(fit)
}

#' @importFrom magrittr %>%
#' @importFrom tibble deframe
#' @importFrom nnet multinom
#' @importFrom reshape2 melt
#' @importFrom tsibble yearmonth
#' @importFrom splines ns
#' @export
#'
FitMultinomWeekly <- function(variant_df, newdata_df) {
  model <- multinom(formula = lineage_collapsed ~ ns(WeekYearCollected, 2), weights = n, data = variant_df, maxit = 1000)
  preds <- predict(model, newdata = newdata_df, type = "prob")
  x <- sweep(x = preds, MARGIN = 1, STATS = newdata_df$n / 7, FUN = "*")
  mode(x) <- "integer"

  newdata_df <- cbind(newdata_df[, c("WeekYearCollected"), drop = FALSE], x)

  newdata_df_long <- melt(newdata_df,
    id.vars = c("WeekYearCollected"),
    variable.name = "variant"
  )

  newdata_df_long$MonthYear <- yearmonth(newdata_df_long$WeekYearCollected)
  newdata_df_long$Date <- as.Date(newdata_df_long$WeekYearCollected)
  return(newdata_df_long)
}

#' @importFrom magrittr %>%
#' @importFrom tibble deframe
#' @importFrom nnet multinom
#' @importFrom reshape2 melt
#' @importFrom tsibble yearmonth
#' @importFrom splines ns
#' @importFrom emmeans emmeans
#' @export
#'
FitMultinomStatewiseDaily <- function(variant_df, newdata_df) {
  fit <- multinom(
    formula = lineage_collapsed ~ State + ns(DateCollectedNumeric, 2),
    data = variant_df,
    weights = n,
    maxit = 10000
  )

  date.from <- min(variant_df$DateCollectedNumeric)
  date.to <- max(variant_df$DateCollectedNumeric)

  preds <- data.frame(emmeans(fit,
    ~lineage_collapsed,
    by = c("DateCollectedNumeric", "State"),
    at = list(DateCollectedNumeric = seq(date.from, date.to, by = 7)),
    mode = "prob", df = NA
  ))

  preds$DateCollected <- as.Date(preds$DateCollectedNumeric, origin = "1970-01-01")
  return(preds)
}
