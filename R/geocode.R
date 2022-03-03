#' Geocode a given address to get its latitude, longitude from Gooogle
#' @param address String address to geocode
#' @param api.key API key for Google Maps
#' @param restrict.to Country name where the search should be restricted to,
#' Currently only supports 'India'
#' @returns A dataframe with geocoded address
#'
#' @importFrom utils URLencode
#' @importFrom RJSONIO fromJSON
#' @importFrom ggmap register_google
#' @export
GeocodeAddress <- function(address, api.key = NULL, restrict.to = NULL) {
  if (!is.null(api.key)) register_google(key = api.key)
  url <- "https://maps.googleapis.com/maps/api/geocode/json?address="
  url <- URLencode(URL = paste(url, address, sep = ""))
  url <- URLencode(URL = paste(url, "&key=", api.key, sep = ""))
  if (!is.null(restrict.to) && restrict.to == "India") {
    url <- URLencode(URL = paste(url, "&bounds=6.4626999,68.1097|35.513327,97.3953587",
      sep = ""
    ))
  }

  x <- fromJSON(content = url, simplify = FALSE)
  if (x$status == "OK") {
    address_components <- x$results[[1]]$address_components
    city <- NA
    district <- NA
    state <- NA
    country <- NA
    zip <- NA
    for (address_component in address_components) {
      types <- address_component$types
      for (type in types) {
        if (type == "postal_code") {
          zip <- address_component$long_name
          break
        } else if (type == "administrative_area_level_1") {
          state <- address_component$long_name
          break
        } else if (type == "administrative_area_level_2") {
          district <- address_component$long_name
          break
        } else if (type == "locality") {
          city <- address_component$long_name
          break
        } else if (type == "country") {
          country <- address_component$long_name
          break
        }
      }
    }

    if (FALSE) {
      if (length(address_components) > 4) {
        city <- address_components[length(address_components) - 4][[1]]$long_name
        district <- address_components[length(address_components) - 3][[1]]$long_name
        state <- address_components[length(address_components) - 2][[1]]$long_name
        country <- address_components[length(address_components) - 1][[1]]$long_name
        zip <- address_components[length(address_components)][[1]]$long_name
      } else if (length(address_components) == 4) {
        city <- address_components[length(address_components) - 3][[1]]$long_name
        district <- address_components[length(address_components) - 2][[1]]$long_name
        state <- address_components[length(address_components) - 1][[1]]$long_name
        country <- address_components[length(address_components)][[1]]$long_name
        zip <- NA
      } else if (length(address_components) == 3) {
        district <- NA
        zip <- NA
        city <- address_components[length(address_components) - 2][[1]]$long_name
        state <- address_components[length(address_components) - 1][[1]]$long_name
        country <- address_components[length(address_components)][[1]]$long_name
      } else if (length(address_components) == 2) {
        district <- NA
        zip <- NA
        city <- NA
        state <- address_components[length(address_components) - 1][[1]]$long_name
        country <- address_components[length(address_components)][[1]]$long_name
      } else {
        city <- NA
        district <- NA
        state <- NA
        country <- NA
        zip <- NA
      }
    }


    longitude <- as.numeric(x = x$results[[1]]$geometry$location$lng)
    latitude <- as.numeric(x = x$results[[1]]$geometry$location$lat)
    formatted_address <- x$results[[1]]$formatted_address

    if (!is.null(restrict.to) && restrict.to == "India") {
      if (city %in% c("Itanagar", "Naharlagun")) {
        state <- "Arunachal Pradesh"
      } else if (city %in% c("Srinagar")) {
        state <- "Jammu and Kashmir"
      }
      state <- gsub("असम", "Assam", state)
      state <- gsub("कर्नाटक", "Karnataka", state)
      state <- gsub("महाराष्ट्र", "Maharashtra", state)
      state <- gsub("Bangalore Urban", "Karnataka", state)
      state <- gsub("Pune", "Maharashtra", state)
      country <- gsub("Maharashtra", "India", country)
      city <- gsub("पुणे", "Pune", city)
    }


    out <- c(
      formatted_address, longitude, latitude, district, city, state, country,
      zip
    )

    names(out) <- c(
      "formatted_address", "longitude", "latitude", "District",
      "City", "State", "Country", "Zip"
    )
  } else {
    out <- rep(x = NA, times = 8)
    names(out) <- c(
      "formatted_address", "longitude", "latitude", "District",
      "City", "State", "Country", "Zip"
    )
  }
  # API only allows 5 requests per second
  Sys.sleep(time = 0.21)
  return(out)
}
