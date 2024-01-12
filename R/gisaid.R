`%notin%` <- Negate(`%in%`)
#' Read metadata downloaded from GISAID
#' @param path Path to tar.xz or metadata.txt downloaded from GISAID
#' @param showProgress Whether to print progress bars for fread
#' @param ... Other parameters for data.table::fread
#' @returns A data frame with parsed metadata with some extra columns
#'
#' @importFrom stringr str_split_fixed str_trim
#' @importFrom tools file_ext
#' @importFrom data.table fread
#' @importFrom utils untar
#' @export
ReadGISAIDMetada <- function(path, showProgress = FALSE, ...) {
  file.ext <- file_ext(path)
  if (file.ext == "xz") {
    fnames <- untar(tarfile = path, list = TRUE)
    if ("metadata.tsv" %notin% fnames) {
      stop(paste("Could not find metadata.tsv in", path))
    }
    td <- tempdir()
    fname <- "metadata.tsv"
    untar(path, files = fname, exdir = td)
    path <- file.path(td, fname)
  }
  gisaid_metadata <- fread(input = path, showProgress = showProgress, ...)
  location_split <- str_split_fixed(string = gisaid_metadata$Location, pattern = " / ", n = 5)
  new_columns <- c("Continent", "Country", "State", "District", "City")
  for (i in seq_along(new_columns)) {
    gisaid_metadata[, new_columns[i]] <- str_trim(string = gsub(pattern = "\\/", replacement = "", x = location_split[, i]))
  }
  gisaid_metadata$pangolin_lineage <- gisaid_metadata$`Pango lineage`
  gisaid_metadata$`Pango lineage` <- NULL
  gisaid_metadata <- FormatGISAIDMetadata(df = gisaid_metadata)
  gisaid_metadata$delay <- gisaid_metadata$DateSubmitted - gisaid_metadata$DateCollected
  gisaid_metadata$delay <- as.numeric(gisaid_metadata$delay, units = "days")
  return(gisaid_metadata)
}

#' Format metadata.tar.x file downloaded from GISAID with some missing columns
#' @param df GISAID metadata dataframe
#' @param collection_col Name of column representing date of collection (Collection date)
#' @param submission_col Name of column representing date of submission (Submission date)
#'
#' @returns  A data frame with following extra columns:
#' DateCollected, MonthYearCollected, YearCollected, MonthCollected, WeekCollected
#' DateCollectedNumeric, MonthYearCollectedNumeric
#' DateSubmitted, MonthYearSubmitted, YearSubmitted, MonthSubmitted, WeekSubmitted
#' DateSubmittedNumeric, MonthYearSubmittedNumeric
#'
#' @importFrom  dplyr pull
#' @importFrom lubridate year month week
#' @importFrom tsibble yearweek
#' @export
FormatGISAIDMetadata <- function(df, collection_col = "Collection date", submission_col = "Submission date") {
  datecol_sel <- pull(df, as.name(collection_col))
  Date <- as.Date(datecol_sel, format = "%Y-%m-%d")
  df$DateCollected <- Date
  df$YearCollected <- year(df$DateCollected)
  df$MonthCollected <- month(df$DateCollected)
  df$WeekCollected <- week(df$DateCollected)
  df$MonthYearCollected <- GetMonthYear(
    datecol = datecol_sel, datefmt = "%Y-%m-%d"
  )
  df$WeekYearCollected <- yearweek(df$DateCollected)

  df$DateCollectedNumeric <- as.numeric(df$DateCollected)
  df$MonthYearCollectedNumeric <- as.numeric(df$MonthYearCollected)

  datecol_sel <- pull(df, as.name(submission_col))
  Date <- as.Date(datecol_sel, format = "%Y-%m-%d")
  df$DateSubmitted <- Date
  df$MonthYearSubmitted <- GetMonthYear(datecol = datecol_sel, datefmt = "%Y-%m-%d")
  df$WeekYearSubmitted <- yearweek(datecol_sel)

  df$YearSubmitted <- year(df$DateSubmitted)
  df$MonthSubmitted <- month(df$DateSubmitted)
  df$WeekSubmitted <- week(df$DateSubmitted)
  df$DateSubmittedNumeric <- as.numeric(df$DateSubmitted)
  df$MonthYearSubmittedNumeric <- as.numeric(df$MonthYearSubmitted)

  return(df)
}

#' Read instrument metadata from GISAID batch downloads
#' @param path Path to a list of directories with "sequence.tsv" inside each subdirector
#' @returns A dataframe with all the instrument related metadata
#' @importFrom stringi stri_split_fixed
#' @importFrom dplyr bind_rows distinct
#' @importFrom utils read.csv
#' @export
ReadAuspiceInstrument <- function(path) {
  metadata <- list()
  for (file in list.files(
    path = path, pattern = "sequence.tsv", full.names = T,
    recursive = T
  )) {
    date_path <- unlist(stri_split_fixed(str = file, pattern = "/"))
    date_path <- date_path[length(date_path) - 1]
    metadata[[date_path]] <- read.csv(file = file, sep = "\t")
  }
  seq_metadata <- bind_rows(metadata) %>% distinct()

  seq_metadata$instrument_cleaned <- SanitizeColumn(seq_metadata$Sequencing.technology)
  seq_metadata$gisaid_epi_isl <- seq_metadata$Accession.ID
  return(seq_metadata)
}

#' Read all metadata from GISAID batch downloads
#' @param path Path to a list of directories with "sequence.tsv" inside each subdirector
#' @returns A dataframe with all the collated metadata
#' @importFrom stringi stri_split_fixed
#' @importFrom dplyr bind_rows distinct
#' @export
ReadAuspiceMetadata <- function(path) {
  metadata <- list()
  for (file in list.files(
    path = path, pattern = "*\\.metadata.tsv", full.names = T,
    recursive = T
  )) {
    date_path <- unlist(stringi::stri_split_fixed(file, "/"))
    date_path <- date_path[length(date_path) - 1]
    df <- read.csv(file, sep = "\t")
    df$age <- as.character(df$age)
    metadata[[date_path]] <- df
  }

  gisaid_metadata <- bind_rows(metadata) %>% distinct()
  gisaid_metadata$State <- gisaid_metadata$division
  gisaid_metadata$District <- gisaid_metadata$location
  gisaid_metadata <- FormatGISAIDMetadata(df = gisaid_metadata, collection_col = "date", submission_col = "date_submitted")
  gisaid_metadata$originating_lab_clean <- SanitizeColumn(col = gisaid_metadata$originating_lab)
  gisaid_metadata$submitting_lab_clean <- SanitizeColumn(col = gisaid_metadata$originating_lab)
  return(gisaid_metadata)
}
