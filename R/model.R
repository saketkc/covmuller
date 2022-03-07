#' @importFrom nnet multinom
FitMultinom <- function(data, formula = as.formula("lineage_collapsed ~ MonthYearNumeric")) {
  fit <- multinom(
    formula = formula,
    data = data,
    weights = total_sequences,
    maxit = 1000
  )
  return(fit)
}
