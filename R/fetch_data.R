#' Get a list of Indian name states
#'
#' @returns A list with state abbreviation as the key and full name as the value
#' @export
GetIndianStates <- function() {
  state_names <- list(
    TT = "India",
    AP = "Andhra Pradesh", AR = "Arunachal Pradesh",
    AS = "Assam", BR = "Bihar",
    CT = "Chhattisgarh",
    GA = "Goa", GJ = "Gujarat",
    HR = "Haryana", HP = "Himachal Pradesh",
    JH = "Jharkhand",
    KA = "Karnataka", KL = "Kerala",
    MP = "Madhya Pradesh", MH = "Maharashtra", MN = "Manipur", ML = "Meghalaya", MZ = "Mizoram",
    NL = "Nagaland",
    OR = "Odisha",
    PB = "Punjab",
    RJ = "Rajasthan",
    SK = "Sikkim",
    TN = "Tamil Nadu", TG = "Telangana", TR = "Tripura", UT = "Uttarakhand",
    UP = "Uttar Pradesh",
    WB = "West Bengal",
    AN = "Andaman & Nicobar",
    CH = "Chandigarh",
    DN = "Dadra and Nagar Haveli and Daman and Diu", DL = "Delhi",
    JK = "Jammu and Kashmir",
    LA = "Ladakh", LD = "Lakshadweep",
    PY = "Puducherry"
  )
  return(state_names)
}

#' Get daily statewise data for India
#'
#' The 'Status' column is one of 'Confirmed', 'Deceased' or 'Recovered'
#'
#' @param url URL to fetch data from
#' @importFrom readr read_csv
#' @returns A data frame containing daily cases for each state
#' @export
GetIndiaDailyData <- function(url = "https://saketkc.github.io/covid19bharat_data//csv/latest/state_wise_daily.csv") {
  statewise_cases <- read_csv(file = url)
  state_names <- GetIndianStates()
  other_cols <- list(DD = "DD", UN = "UN", Date = "Date", Date_YMD = "Date_YMD", Status = "Status")
  new_cols <- c(state_names, other_cols)
  colnames(statewise_cases) <- new_cols[colnames(statewise_cases)]
  return(statewise_cases)
}

#' Get daily cases for India for a given status type
#' @param url URL to fetch data from
#' @param status One of "Confirmed", "Recovered", or "Deceased"
#' @returns A data frame containing daily cases for each state

#' @importFrom magrittr %>%
#' @importFrom dplyr arrange group_by summarise_all
#
#' @export
GetIndiaDailyCasesCumulative <- function(url = "https://saketkc.github.io/covid19bharat_data//csv/latest/state_wise_daily.csv", status = "Confirmed") {
  statewise_cases <- GetIndiaDailyData(url = url)
  select_status <- statewise_cases[statewise_cases$Status == status, ]
  select_status$MonthYear <- GetMonthYear(select_status$Date_YMD)
  state_names <- GetIndianStates()

  select_status_subset <- select_status[, c("MonthYear", as.character(state_names))]

  for (col in as.character(state_names)) {
    select_status_subset[, col] <- as.numeric(select_status_subset[, col])
  }
  select_status_subset_monthwise <- select_status_subset %>%
    group_by(MonthYear) %>%
    summarise_all(sum, na.rm = T) %>%
    arrange(MonthYear)
  select_status_subset_monthwise_cumsum <- select_status_subset_monthwise
  for (col in state_names) {
    select_status_subset_monthwise_cumsum[, col] <- cumsum(select_status_subset_monthwise[, col])
  }
  return(select_status_subset_monthwise_cumsum)
}

#' Get daily confirmed cases for India
#' @param url URL to fetch data from
#' @returns A data frame containing daily cases for each state

#' @importFrom magrittr %>%
#' @importFrom dplyr arrange group_by summarise_all
#
#' @export
GetIndiaConfirmedCasesCumulative <- function(url = "https://saketkc.github.io/covid19bharat_data//csv/latest/state_wise_daily.csv") {
  statewise_cases <- GetIndiaDailyCases(url = url)
  return(statewise_cases)
}

#' Get daily deceased cases for India
#' @param url URL to fetch data from
#' @returns A data frame containing daily cases for each state

#' @importFrom magrittr %>%
#' @importFrom dplyr arrange group_by summarise_all
#
#' @export
GetIndiaDeceasedCasesCumulative <- function(url = "https://saketkc.github.io/covid19bharat_data//csv/latest/state_wise_daily.csv") {
  statewise_cases <- GetIndiaDailyCases(url = url, status = "Deceased")
  return(statewise_cases)
}

#' Get hospitalization data for India
#'
GetIndiaHospitalization <- function(url = "") {
  return(NA)
}


#' Get India monthwose cases long
#'
#' @param url URL to fetch data from
#' @returns A data frame containing monthly cases for each state in long form

#' @importFrom magrittr %>%
#' @importFrom dplyr arrange group_by summarise_all rename
#' @importFrom readr read_csv
#' @importFrom reshape2 melt
#' @export
GetIndiaConfirmedCasesMonthlyLong <- function(url = "http://saketkc.github.io/covid19bharat_data//csv/latest/state_wise_daily.csv", level = "State") {
  if (level == "State") {
    statewise_cases <- GetIndiaDailyData(url = url)
    confirmed <- statewise_cases[statewise_cases$Status == "Confirmed", ]
    confirmed$MonthYear <- GetMonthYear(confirmed$Date_YMD)
    state_names <- GetIndianStates()
    confirmed_subset <- confirmed[, c("MonthYear", as.character(state_names))]
    confirmed_subset_monthwise <- confirmed_subset %>%
      group_by(MonthYear) %>%
      summarise_all(list(~ sum(., na.rm = TRUE))) %>%
      arrange(MonthYear)
    confirmed_subset_monthwise_long <- melt(data = confirmed_subset_monthwise, id.vars = "MonthYear", varnames = c("State")) %>%
      rename(State = variable)
    confirmed_subset_monthwise_long$State <- as.character(confirmed_subset_monthwise_long$State)
    confirmed_subset_monthwise_long$type <- "Confirmed"
    return(confirmed_subset_monthwise_long)
  } else if (level == "district") {
    district_cases_cumulative <- read_csv("https://saketkc.github.io/covid19bharat_data//csv/latest/districts.csv") %>% arrange(Date)
    district_cases_cumulative$Date <- as.Date(district_cases_cumulative$Date, format = "%Y-%m-%d")
    district_cases_cumulative <- district_cases_cumulative %>%
      select(Date, State, District, Confirmed) %>%
      group_by(State, District) %>%
      arrange(Date) %>%
      mutate(value = diff(c(0, Confirmed)))
    district_cases_cumulative$value[district_cases_cumulative$value < 0] <- 0

    confirmed_subset_monthwise_long <- district_cases_cumulative %>% select(-Confirmed)


    confirmed_subset_monthwise_long$MonthYear <- GetMonthYear(confirmed_subset_monthwise_long$Date)

    confirmed_subset_monthwise_long <- confirmed_subset_monthwise_long %>% select(MonthYear, State, District, value)

    # take mean of monthly cases
    confirmed_subset_monthwise_long <- confirmed_subset_monthwise_long %>%
      group_by(MonthYear, State, District) %>%
      summarise(value = sum(value, na.rm = TRUE))
    return(confirmed_subset_monthwise_long)
  }
}


#' Get India weekwise cases long
#'
#' @param url URL to fetch data from
#' @returns A data frame containing monthly cases for each state in long form

#' @importFrom magrittr %>%
#' @importFrom dplyr arrange group_by summarise_all rename
#' @importFrom reshape2 melt
#' @importFrom readr read_csv
#' @importFrom tsibble yearweek
#' @export
GetIndiaConfirmedCasesWeeklyLong <- function(url = "https://saketkc.github.io/covid19bharat_data//csv/latest/state_wise_daily.csv", level = "State") {
  if (level == "State") {
    statewise_cases <- GetIndiaDailyData(url = url)
    confirmed <- statewise_cases[statewise_cases$Status == "Confirmed", ]
    confirmed$MonthYear <- GetMonthYear(confirmed$Date_YMD)
    confirmed$WeekYear <- yearweek(confirmed$Date_YMD)

    state_names <- GetIndianStates()
    confirmed_subset <- confirmed[, c("WeekYear", as.character(state_names))]
    confirmed_subset_weekwise <- confirmed_subset %>%
      group_by(WeekYear) %>%
      summarise_all(list(~ mean(., na.rm = TRUE))) %>%
      arrange(WeekYear)

    confirmed_subset_weekwise_long <- melt(data = confirmed_subset_weekwise, id.vars = "WeekYear", varnames = c("State")) %>%
      rename(State = variable)
    confirmed_subset_weekwise_long$State <- as.character(confirmed_subset_weekwise_long$State)
    confirmed_subset_weekwise_long$value <- ceiling(confirmed_subset_weekwise_long$value)
  } else if (level == "district") {
    district_cases_cumulative <- read_csv("https://saketkc.github.io/covid19bharat_data//csv/latest/districts.csv") %>% arrange(Date)
    district_cases_cumulative$Date <- as.Date(district_cases_cumulative$Date, format = "%Y-%m-%d")
    district_cases_cumulative <- district_cases_cumulative %>%
      select(Date, State, District, Confirmed) %>%
      group_by(State, District) %>%
      arrange(Date) %>%
      mutate(value = diff(c(0, Confirmed)))
    confirmed_subset_weekwise_long <- district_cases_cumulative %>% select(-Confirmed)
    confirmed_subset_weekwise_long$value[confirmed_subset_weekwise_long$value < 0] <- 0


    confirmed_subset_weekwise_long$MonthYear <- GetMonthYear(confirmed_subset_weekwise_long$Date)
    confirmed_subset_weekwise_long$WeekYear <- yearweek(confirmed_subset_weekwise_long$Date)

    confirmed_subset_weekwise_long <- confirmed_subset_weekwise_long %>% select(WeekYear, State, District, value)

    # take mean of weekly cases
    confirmed_subset_weekwise_long <- confirmed_subset_weekwise_long %>%
      group_by(WeekYear, State, District) %>%
      summarise(value = mean(value, na.rm = TRUE))
    confirmed_subset_weekwise_long$value <- ceiling(confirmed_subset_weekwise_long$value)
  }
  confirmed_subset_weekwise_long$type <- "Confirmed"
  return(confirmed_subset_weekwise_long)
}
