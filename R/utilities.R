#' Get MonthYear (zoo) from a dataframe string formatted data column
#' @param datecol A vector of string formatted dates
#' @param datefmt Format of the date (default is Y-m-d)
#'
#' @returns A vector with dates converted to MonthYear format (zoo::as.yearmon)
#' @importFrom  dplyr pull
#' @importFrom zoo as.yearmon
#' @export
GetMonthYear <- function(datecol, datefmt = "%Y-%m-%d") {
  Date <- as.Date(datecol, format = datefmt)
  Month <- strftime(Date, "%m")
  Year <- strftime(Date, "%Y")
  MonthYear <- paste0(Month, "/", Year)
  MonthYear <- as.yearmon(MonthYear, format = "%m/%Y")
  return(MonthYear)
}

#' Determine filetype
#' @param path Path to file
#' @returns Extension of file
GetFiletype <- function(path) {
  fopen <- file(path)
  extension <- summary(fopen)$class
  close.connection(fopen)
  return(extension)
}

#' Get variants of concern
#' @returns A list of variants of concern with their pangolin lineage
#' @export
GetVOCs <- function() {
  alpha_voc <- c("B.1.1.7", "Q.1", "Q.2", "Q.3", "Q.4", "Q.5", "Q.6", "Q.7", "Q.8")
  beta_voc <- c("B.1.351", "B.1.351.1", "B.1.351.2", "B.1.351.3", "B.1.351.4", "B.1.351.5")
  gamma_voc <- c(
    "P.1", "P.1.2", "P.1.3", "P.1.4", "P.1.5", "P.1.6", "P.1.7", "P.1.7.1", "P.1.8", "P.1.9", "P.1.10", "P.1.10.1", "P.1.10.2", "P.1.11", "P.1.12",
    "P.1.12.1", "P.1.13", "P.1.14", "P.1.15", "P.1.16", "P.1.17", "P.1.17.1"
  )
  delta_voc <- c(
    "B.1.617.2", "AY.1", "AY.2", "AY.3", "AY.3.1", "AY.3.2", "AY.3.3", "AY.4", "AY.4.1", "AY.4.2", "AY.4.2.1", "AY.4.2.2", "AY.4.2.3", "AY.4.3",
    "AY.4.4", "AY.4.5", "AY.4.6", "AY.4.7", "AY.4.8", "AY.4.9", "AY.4.10", "AY.5", "AY.5.1", "AY.5.2", "AY.5.3", "AY.5.4", "AY.5.5", "AY.6", "AY.7", "AY.7.1",
    "AY.7.2", "AY.8", "AY.9", "AY.9.1", "AY.9.2", "AY.9.2.1", "AY.9.2.2", "AY.10", "AY.11", "AY.12", "AY.13", "AY.14", "AY.15", "AY.16", "AY.16.1", "AY.17",
    "AY.18", "AY.19", "AY.20", "AY.20.1", "AY.21", "AY.22", "AY.23", "AY.23.1", "AY.23.2", "AY.24", "AY.25", "AY.25.1", "AY.25.1.1", "AY.26", "AY.26.1",
    "AY.27", "AY.28", "AY.29", "AY.29.1", "AY.30", "AY.31", "AY.32", "AY.33", "AY.33.1", "AY.34", "AY.34.1", "AY.34.1.1", "AY.34.2", "AY.35", "AY.36",
    "AY.37", "AY.38", "AY.39", "AY.39.1", "AY.39.1.1", "AY.39.1.2", "AY.39.1.3", "AY.39.2", "AY.40", "AY.41", "AY.42", "AY.42.1", "AY.43", "AY.43.1", "AY.43.2",
    "AY.43.3", "AY.43.4", "AY.43.5", "AY.43.6", "AY.43.7", "AY.44", "AY.45", "AY.46", "AY.46.1", "AY.46.2", "AY.46.3", "AY.46.4", "AY.46.5", "AY.46.6",
    "AY.46.6.1", "AY.47", "AY.48", "AY.49", "AY.50", "AY.51", "AY.52", "AY.53", "AY.54", "AY.55", "AY.56", "AY.57", "AY.58", "AY.59", "AY.60", "AY.61",
    "AY.62", "AY.63", "AY.64", "AY.65", "AY.66", "AY.67", "AY.68", "AY.69", "AY.70", "AY.71", "AY.72", "AY.73", "AY.74", "AY.75", "AY.75.1", "AY.75.2",
    "AY.75.3", "AY.76", "AY.77", "AY.78", "AY.79", "AY.80", "AY.81", "AY.82", "AY.83", "AY.84", "AY.85", "AY.86", "AY.87", "AY.88", "AY.89", "AY.90", "AY.91",
    "AY.91.1", "AY.92", "AY.93", "AY.94", "AY.95", "AY.96", "AY.97", "AY.98", "AY.98.1", "AY.99", "AY.99.1", "AY.99.2", "AY.100", "AY.101", "AY.102", "AY.102.1",
    "AY.102.2", "AY.103", "AY.104", "AY.105", "AY.106", "AY.107", "AY.108", "AY.109", "AY.110", "AY.111", "AY.112", "AY.112.1", "AY.113", "AY.114", "AY.115",
    "AY.116", "AY.116.1", "AY.117", "AY.118", "AY.119", "AY.119.1", "AY.119.2", "AY.120", "AY.120.1", "AY.120.2", "AY.120.2.1", "AY.121", "AY.121.1", "AY.122",
    "AY.122.1", "AY.122.2", "AY.122.3", "AY.123", "AY.123.1", "AY.124", "AY.124.1", "AY.125", "AY.126", "AY.127", "AY.127.1", "AY.128", "AY.129", "AY.130",
    "AY.131", "AY.132", "AY.133"
  )
  omicron_voc <- c("B.1.1.529", "BA.1", "BA.1.1", "BA.2", "BA.3")
  lambda_voc <- c("C.37", "C.37.1")
  mu_voc <- c("B.1.621", "B.1.621.1", "B.1.621.2", "BB.1", "BB.2")
  vocs <- list(
    alpha = alpha_voc, beta = beta_voc,
    gamma = gamma_voc, delta = delta_voc,
    omicron = omicron_voc, lambda = lambda_voc, mu = mu_voc
  )
  return(vocs)
}

#' Sanitize a vector to remove special characters and convert it to tile
#'
#' This function removes:
#' 1. Commas with space
#' 2. Hyphen with space
#' 3. Multiple dots with single space
#' 4. Multiple spacies with single space
#' 5. NA with "UNKNOWN"
#' @param col String vector
#' @returns A string vector with sanitized entries
#' @importFrom stringr str_squish str_to_title
#' @export
SanitizeColumn <- function(col) {
  col <- gsub(pattern = ",", replacement = " ", x = col)
  col <- gsub(pattern = "-", replacement = " ", x = col)
  col <- gsub(pattern = "\\.", replacement = " ", x = col)
  col <- gsub(pattern = "\\s+", replacement = " ", x = col)

  col <- str_squish(string = col)
  col <- str_to_title(string = col)
  col[is.na(col)] <- "UNKNOWN"
  col[col == " "] <- "UNKNOWN"
  return(col)
}


#'
