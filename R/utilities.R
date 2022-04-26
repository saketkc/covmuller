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
    "B.1.617.2", "AY.1", "AY.2", "AY.3", "AY.3.1", "AY.3.2", "AY.3.3", "AY.3.4", "AY.4", "AY.4.1", "AY.4.2", "AY.4.2.1", "AY.4.2.2", "AY.4.2.3", "AY.4.2.4", "AY.4.3", "AY.4.4", "AY.4.5", "AY.4.6", "AY.4.7", "AY.4.8", "AY.4.9", "AY.4.10", "AY.4.11", "AY.4.12", "AY.4.13", "AY.4.14", "AY.4.15", "AY.4.16", "AY.4.17", "AY.5", "AY.5.1", "AY.5.2", "AY.5.3", "AY.5.4", "AY.5.5", "AY.5.6", "AY.6", "AY.7", "AY.7.1", "AY.7.2", "AY.8", "AY.9", "AY.9.1", "AY.9.2", "AY.9.2.1", "AY.9.2.2", "AY.10", "AY.11", "AY.12", "AY.13", "AY.14", "AY.15", "AY.16", "AY.16.1", "AY.17", "AY.18", "AY.19", "AY.20", "AY.20.1", "AY.21", "AY.22", "AY.23", "AY.23.1", "AY.23.2", "AY.24", "AY.24.1", "AY.25", "AY.25.1", "AY.25.1.1", "AY.25.1.2", "AY.25.2", "AY.25.3", "AY.26", "AY.26.1", "AY.27", "AY.28", "AY.29", "AY.29.1", "AY.30", "AY.31", "AY.32", "AY.33", "AY.33.1", "AY.34", "AY.34.1", "AY.34.1.1", "AY.34.2", "AY.35", "AY.36", "AY.37", "AY.38", "AY.39", "AY.39.1", "AY.39.1.1", "AY.39.1.2", "AY.39.1.3", "AY.39.2", "AY.39.3", "AY.40", "AY.41", "AY.42", "AY.42.1", "AY.43", "AY.43.1", "AY.43.2", "AY.43.3", "AY.43.4", "AY.43.5", "AY.43.6", "AY.43.7", "AY.43.8", "AY.43.9", "AY.44", "AY.45", "AY.46", "AY.46.1", "AY.46.2", "AY.46.3", "AY.46.4", "AY.46.5", "AY.46.6", "AY.46.6.1", "AY.47", "AY.48", "AY.49", "AY.50", "AY.51", "AY.52", "AY.53", "AY.54", "AY.55", "AY.56", "AY.57", "AY.58", "AY.59", "AY.60", "AY.61", "AY.62", "AY.63", "AY.64", "AY.65", "AY.66", "AY.67", "AY.68", "AY.69", "AY.70", "AY.71", "AY.72", "AY.73", "AY.74", "AY.75", "AY.75.1", "AY.75.2", "AY.75.3", "AY.76", "AY.77", "AY.78", "AY.79", "AY.80", "AY.81", "AY.82", "AY.83", "AY.84", "AY.85", "AY.86", "AY.87", "AY.88", "AY.89", "AY.90", "AY.91", "AY.91.1", "AY.92", "AY.93", "AY.94", "AY.95", "AY.96", "AY.97", "AY.98", "AY.98.1", "AY.99", "AY.99.1", "AY.99.2", "AY.100", "AY.101", "AY.102", "AY.102.1", "AY.102.2", "AY.103", "AY.103.1", "AY.103.2", "AY.104", "AY.105", "AY.106", "AY.107", "AY.108", "AY.109", "AY.110", "AY.111", "AY.112", "AY.112.1", "AY.113", "AY.114", "AY.115", "AY.116", "AY.116.1", "AY.117", "AY.118", "AY.119", "AY.119.1", "AY.119.2", "AY.120", "AY.120.1", "AY.120.2", "AY.120.2.1", "AY.121", "AY.121.1", "AY.122", "AY.122.1", "AY.122.2", "AY.122.3", "AY.122.4", "AY.122.5", "AY.123", "AY.123.1", "AY.124", "AY.124.1", "AY.124.1.1", "AY.125", "AY.125.1", "AY.126", "AY.127", "AY.127.1", "AY.127.2", "AY.128", "AY.129", "AY.130", "AY.131", "AY.132", "AY.133"
  )
  omicron_voc <- c("BA.1.1", "BA.2.9", "BA.1", "BA.2", "BA.1.17.2", "BA.1.15", "BA.1.1.15", "BA.1.15.1", "BA.2.10", "BA.1.1.1", "BA.1.18", "BA.1.14", "BA.1.17", "BA.1.1.2", "BA.1.16", "BA.2.3", "BA.2.7", "BA.1.1.14", "BA.1.1.11", "BA.2.1", "BA.1.1.13", "BA.1.1.16", "BA.2.6", "BA.1.1.12", "BA.1.13", "BA.1.1.10", "BA.1.7", "BA.1.19", "BA.1.9", "BA.2.8", "BA.2.5", "BA.1.2", "BA.1.13.1", "BA.1.1.4", "BA.1.10", "BA.1.12", "BA.3", "BA.1.6", "BA.1.8", "BA.1.1.9", "BA.1.1.7", "BA.1.1.3", "BA.1.1.8", "BA.2.2", "BA.1.3", "BA.1.5", "BA.1.17.1", "BA.1.1.6", "BA.1.1.5", "BA.2.3.1", "BA.1.4", "BA.2.4", "BA.1.16.1", "B.1.1.529", "BA.1.11")
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

#' Clean Indian States
#' @param state A vector of state names
#' @returns A vector of cleaned state names
#' @importFrom stringr str_squish str_to_title
#' @importFrom dplyr recode
#' @export
CleanIndianStates <- function(states) {
  states <- str_squish(states)
  states <- str_to_title(states)
  states <- recode(
    .x = states,
    `Maharastra` = "Maharashtra",
    `Andhra pradesh` = "Andhra Pradesh", Pondicherry = "Puducherry",
    `Jammu and Kashmīr` = "Jammu and Kashmir", `Andaman & Nicobar` = "Andaman & Nicobar",
    Chhatisgarh = "Chhattisgarh", Jaipur = "Rajasthan", `Dadra and Nagar Haveli` = "Dadra and Nagar Haveli and Daman and Diu",
    Harayana = "Haryana", Jammu = "Jammu and Kashmir", `Jammu and kashmir` = "Jammu and Kashmir",
    `Jammu & Kashmir` = "Jammu and Kashmir", Maharasthra = "Maharashtra",
    `Andaman and Nicobar Islands` = "Andaman & Nicobar",
    `Andaman And Nicobar Islands` = "Andaman & Nicobar",
    `Dadra And Nagar Haveli` = "Dadra and Nagar Haveli and Daman and Diu",
    `Dadra and Nagar Haveli and Daman and Diu` = "Dadra and Nagar Haveli and Daman and Diu",
    `Dadra And Nagar Haveli And Daman And Diu` = "Dadra and Nagar Haveli and Daman and Diu",
    `Dadra and Nagar Haveli and Daman and Diui` = "Dadra and Nagar Haveli and Daman and Diu",
    Tamilnadu = "Tamil Nadu",
    Maharshtra = "Maharashtra",
    `Jammu & kashmīr` = "Jammu and Kashmir",
    `Gujrat` = "Gujarat",
    `Gujart` = "Gujarat",
    `Chandighar` = "Chandigarh",
    `Jammu And Kashmir` = "Jammu and Kashmir",
    `Jammu And Kashmīr` = "Jammu and Kashmir",
    `New Delhi` = "Delhi"
  )
  states[states == "Undefined"] <- "Unknown"
  states[is.na(states)] <- "Unknown"
  states[states == ""] <- "Unknown"
  return(states)
}

#' Clean states from South Africa
#' @param state A vector of state names
#' @returns A vector of cleaned state names
#' @importFrom stringr str_squish str_to_title
#' @importFrom dplyr recode
#' @export
CleanSouthAfricanStates <- function(states) {
  states <- str_squish(states)
  states <- str_to_title(states)
  states <- gsub(pattern = " Province", replacement = "", x = states)
  states <- recode(
    .x = states,
    `Kwazulu Natal` = "Kwazulu-Natal",
    `Kzn` = "Kwazulu-Natal"
  )
  states[states == "Undefined"] <- "Unknown"
  states[is.na(states)] <- "Unknown"
  states[states == ""] <- "Unknown"
  return(states)
}

#' Clean states from the USA
#' @param state A vector of state names
#' @returns A vector of cleaned state names
#' @importFrom stringr str_squish str_to_title str_split_fixed
#' @importFrom dplyr recode
#' @export
CleanAmericanStates <- function(states) {
  states <- str_squish(states)
  states <- str_to_title(states)
  states <- str_split_fixed(string = states, pattern = ",", n = 2)[, 1]
  states <- recode(
    .x = states,
    `Virgin Islands Of The U.s.` = "US Virgin Islands",
    `Us Virgin Islands` = "US Virgin Islands",
  )
  states[states == "Undefined"] <- "Unknown"
  states[is.na(states)] <- "Unknown"
  states[states == ""] <- "Unknown"
  states[states == "Southwest"] <- "Unknown"
  return(states)
}

#' Clean states from Canada
#' @param state A vector of state names
#' @returns A vector of cleaned state names
#' @importFrom stringr str_squish str_to_title str_split_fixed
#' @importFrom dplyr recode
#' @export
CleanCanadaStates <- function(states) {
  states <- str_squish(states)
  states <- recode(
    .x = states,
    `Newfoundland` = "Newfoundland and Labrador"
  )
  states[states == "Undefined"] <- "Unknown"
  states[is.na(states)] <- "Unknown"
  states[states == ""] <- "Unknown"
  return(states)
}

#' Create a combined dataframe of sequenced cases and confirmed cases
#' @param cases_sequenced A long dataframe of per state sequenced cases
#' @param cases_total A long dataframe of total monthly cases
#' @param prune_oversequenced Whether to round of percentages above 100 to 100. Default is TRUE
#'
#' @returns a combined dataframe with case load and sequenced
#' @importFrom zoo as.yearmon
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows group_by summarise_all filter
#' @importFrom tidyr spread
#' @export
CombineSequencedCases <- function(cases_sequenced, confirmed_long,
                                  prune_oversequenced = TRUE,
                                  month.min = NULL, month.max = NULL,
                                  max.percent = 100) {
  empty_df <- expand.grid(as.yearmon(x = unique(x = as.character(confirmed_long$MonthYear))),
    sort(unique(confirmed_long$State)),
    0,
    stringsAsFactors = F
  )
  colnames(empty_df) <- c("MonthYear", "State", "value") # , 'type')
  empty_df <- empty_df %>% arrange(State, MonthYear)
  total_seq <- bind_rows(empty_df, cases_sequenced) %>%
    group_by(State, MonthYear) %>%
    summarise_all(list(value = sum))
  total_seq$type <- "Sequenced"

  cases_and_shared <- bind_rows(confirmed_long, total_seq) %>% arrange(State, MonthYear)
  cases_and_shared <- spread(data = cases_and_shared, key = type, value = value)
  cases_and_shared[is.na(cases_and_shared)] <- 0

  cases_and_shared["percent_sequenced_collected"] <- 100 * cases_and_shared$Sequenced / cases_and_shared$Confirmed
  cases_and_shared$percent_sequenced_collected[is.infinite(cases_and_shared$percent_sequenced_collected)] <- NA
  if (prune_oversequenced) {
    cases_and_shared$percent_sequenced_collected[(cases_and_shared$percent_sequenced_collected) > 100] <- 100
  }
  if (!is.null(month.min)) {
    cases_and_shared <- cases_and_shared %>% filter(MonthYear > month.min)
  }
  if (!is.null(month.max)) {
    cases_and_shared <- cases_and_shared %>% filter(MonthYear <= month.max)
  }
  cases_and_shared$percent_sequenced_toplot <- cases_and_shared$percent_sequenced_collected
  cases_and_shared$percent_sequenced_toplot[cases_and_shared$percent_sequenced_toplot > max.percent] <- max.percent
  cases_and_shared$MonthYear <- factor(x = cases_and_shared$MonthYear)

  return(cases_and_shared)
}


#' Filter GISAID India Metadata for India
#' @param gisaid_metadata_all A dataframe with all GISAID metadata
#' @returns A dataframe with only Indian entries in Human and where the date is known
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows group_by summarise_all
FilterGISAIDIndia <- function(gisaid_metadata_all) {
  gisaid_india_all <- gisaid_metadata_all %>%
    filter(Country == "India") %>%
    filter(Host == "Human")
  india_states <- GetIndianStates()
  gisaid_india_all$State <- CleanIndianStates(gisaid_india_all$State)
  gisaid_india_all <- gisaid_india_all %>% filter(State %in% india_states)
  gisaid_india_all <- gisaid_india_all %>%
    arrange(State, MonthYearCollected)
  return(gisaid_india_all)
}


#' Collpase pangolin lineage a list of VOCs
#' @param variant_df A dataframe with "pangolin_lineage" column
#' @param vocs A named list with VOC (variant of concernt) name as key and a list of lineages under the VOC.
#' The method will replace all lineages under a VOC to its name.
#' @param custom_voc_mapping A named vector with a custom mapping for naming some lineages. See example.
#' @returns  A dataframe with a new column "lineage_collpased".
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr case_when  group_by mutate select_if summarise_all
#' @importFrom stringr str_to_title
#' @export
CollapseLineageToVOCs <- function(variant_df, vocs = GetVOCs(), custom_voc_mapping = NULL, summarize = TRUE) {
  if (!"pangolin_lineage" %in% colnames(variant_df)) {
    stop("pangolin_lineage column is not present in the input")
  }
  variant_df$lineage_collapsed <- "Others"
  for (name in names(vocs)) {
    variant_df <- variant_df %>% mutate(lineage_collapsed = case_when(
      pangolin_lineage %in% vocs[[!!name]] ~ str_to_title(!!name),
      TRUE ~ lineage_collapsed
    ))
  }
  if (!is.null(custom_voc_mapping)) {
    for (name in names(custom_voc_mapping)) {
      variant_df <- variant_df %>% mutate(lineage_collapsed = case_when(
        pangolin_lineage %in% !!name ~ custom_voc_mapping[[!!name]],
        TRUE ~ lineage_collapsed
      ))
    }
  }
  if (summarize) {
    variant_df$pangolin_lineage <- NULL

    variant_df <- variant_df %>%
      group_by(MonthYearCollected, lineage_collapsed) %>%
      select_if(is.numeric) %>%
      summarise_all(list(prevalence = sum)) %>%
      ungroup()
  }
  variant_df$MonthYearCollectedFactor <- factor(as.character(variant_df$MonthYearCollected),
    levels = as.character(sort(unique(variant_df$MonthYearCollected)))
  )

  return(variant_df)
}

#' Summarize the total number of variants per month
#' @param variant_df A dataframe
#' @returns A dataframe with monthwise counts of each variant sequenced
#' @importFrom dplyr count group_by ungroup
#' @importFrom tidyr drop_na
#' @export
SummarizeVariantsMonthwise <- function(variant_df, by_state = FALSE) {
  if (by_state) {
    variant_df <- variant_df %>%
      group_by(State, MonthYearCollected, pangolin_lineage) %>%
      count() %>%
      ungroup() %>%
      drop_na()
    return(variant_df)
  } else {
    variant_df <- variant_df %>%
      group_by(MonthYearCollected, pangolin_lineage) %>%
      count() %>%
      ungroup() %>%
      drop_na()
  }
  return(variant_df)
}

#' Summarize the total number of variants per week
#' @param variant_df A dataframe
#' @returns A dataframe with monthwise counts of each variant sequenced
#' @importFrom dplyr count group_by ungroup
#' @importFrom tidyr drop_na
#' @export
SummarizeVariantsWeekwise <- function(variant_df, by_state = FALSE) {
  if (by_state) {
    variant_df <- variant_df %>%
      group_by(State, WeekYearCollected, lineage_collapsed) %>%
      count() %>%
      ungroup() %>%
      drop_na()
    return(variant_df)
  } else {
    variant_df <- variant_df %>%
      group_by(WeekYearCollected, lineage_collapsed) %>%
      count() %>%
      ungroup() %>%
      drop_na()
  }
}


#' Summarize the total number of variants per week
#' @param variant_df A dataframe
#' @returns A dataframe with monthwise counts of each variant sequenced
#' @importFrom dplyr count group_by ungroup
#' @importFrom tidyr drop_na
#' @export
SummarizeVariantsDatewise <- function(variant_df, by_state = FALSE) {
  if (by_state) {
    variant_df <- variant_df %>%
      group_by(State, DateCollectedNumeric, lineage_collapsed) %>%
      count() %>%
      ungroup() %>%
      drop_na()
    return(variant_df)
  } else {
    variant_df <- variant_df %>%
      group_by(DateCollectedNumeric, lineage_collapsed) %>%
      count() %>%
      ungroup() %>%
      drop_na()
  }
}



#' Convert monthwise counts to prevalence
#' @param variant_df A dataframe
#' @returns A dataframe with monthwise prevalence of variants
#' @importFrom magrittr %>%
#' @importFrom stringr str_to_title
#' @importFrom dplyr group_by mutate summarise ungroup filter
#' @export
CountsToPrevalence <- function(variant_df) {
  variant_df <- variant_df %>%
    group_by(MonthYearCollected) %>%
    mutate(n_sum = sum(n)) %>%
    ungroup() %>%
    group_by(MonthYearCollected, pangolin_lineage) %>%
    summarise(prevalence = 100 * n / n_sum) %>%
    ungroup() %>%
    filter(!is.na(MonthYearCollected))
  variant_df$prevalence <- as.numeric(variant_df$prevalence)
  return(variant_df)
}


#' Get total number of sequenced samples per month in a Country
#' @returns A dataframe with sequencing statistics per state per country
#' @importFrom dplyr arrange count group_by rename
#' @importFrom tidyr drop_na
#' @export
TotalSequencesPerMonthStatewise <- function(variant_df, drop_country = FALSE) {
  df <- variant_df %>%
    group_by(Country, State, MonthYearCollected) %>%
    count() %>%
    arrange(Country, State, MonthYearCollected) %>%
    drop_na() %>%
    rename(MonthYear = MonthYearCollected, value = n)
  if (drop_country) {
    df$Country <- NULL
  }
  return(df)
}

#' Get total sequenced samples per country
#' @param variant_df A dataframe
#' @returns A dataframe with sequencing statistics per country
#' @importFrom dplyr arrange count group_by rename
#' @importFrom tidyr drop_na
#' @export
TotalSequencesPerMonthCountrywise <- function(variant_df, rename_country_as_state = TRUE) {
  df <- variant_df %>%
    group_by(Country, MonthYearCollected) %>%
    count() %>%
    arrange(Country, MonthYearCollected) %>%
    drop_na() %>%
    rename(MonthYear = MonthYearCollected, value = n)

  if (rename_country_as_state) {
    # drop the country
    # replace the state with
    df$State <- df$Country
    df$Country <- NULL
  }
  return(df)
}
