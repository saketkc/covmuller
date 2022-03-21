#' @importFrom magrittr %>%
#' @importFrom tibble deframe
#' @importFrom nnet multinom
FitMultinom <- function(data,
                        formula = as.formula("lineage_collapsed ~ DateCollectedNumeric + State"),
                        weights = "total_sequences") {
  wt <- data %>% pull(!!weights) #%>% deframe()
  print(wt)
  fit <- multinom(
    formula = formula,
    data = data,
    weights = wt,
    maxit = 1000
  )
  return(fit)
}
