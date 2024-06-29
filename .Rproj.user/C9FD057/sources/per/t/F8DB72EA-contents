#' Process raw data into a clean dataset
#' @keywords internal
cams_solar_radiation_process_l0_to_l1 <- function(place, data, meta){

  meta <- dplyr::bind_cols(variable = c("from", "to", "lat", "lon", "alt", "tz"), meta[9:14, 2]) %>%
    dplyr::mutate(variable = stringr::str_trim(variable, side = "both")) %>%
    tidyr::spread("variable", `utf-8`) %>%
    dplyr::mutate(alt = as.numeric(alt),
                  lat = as.numeric(lat),
                  lon = as.numeric(lon),
                  to = as.POSIXct(to),
                  from = as.POSIXct(from),
                  tz = dplyr::case_when(
                    tz == " Universal time (UT)" ~ "UTC",
                    TRUE ~ tz
                  ))
  # Solar data
  # Standard col names
  colnames(data) <- c("date", "TOA", "clearsky_GHI", "clearsky_BHI",
                      "clearsky_DHI", "clearsky_BNI", "GHI", "BHI", "DHI", "BNI", "reliability")
  # Add metadata
  data$lat <- meta$lat
  data$place <- place
  data$lon <- meta$lon
  data$alt <- meta$alt
  # Add date elements
  data$date <- as.POSIXct(purrr::map_chr(strsplit(data$date, "/"), ~.x[1]), tz = meta$tz)
  data$date <- as.Date(data$date)

  for(i in 6:nrow(data)){
    if(is.na(data$TOA[i])){
      data$TOA[i] <- mean(data$TOA[(i-5):(i-1)])
      data$reliability[i] <- 0
      print(paste0("TOA NAs imputed idx: ", i))
    }
    if(is.na(data$GHI[i])){
      data$GHI[i] <- mean(data$GHI[(i-5):(i-1)])
      data$reliability[i] <- 0
      print(paste0("GHI NAs imputed idx: ", i))
    }
    if(is.na(data$BHI[i])){
      data$BHI[i] <- mean(data$BHI[(i-5):(i-1)])
      data$reliability[i] <- 0
      print(paste0("BHI NAs imputed idx: ", i))
    }
    if(is.na(data$DHI[i])){
      data$DHI[i] <- mean(data$DHI[(i-5):(i-1)])
      data$reliability[i] <- 0
      print(paste0("DHI NAs imputed idx: ", i))
    }
    if(is.na(data$BNI[i])){
      data$BNI[i] <- mean(data$BNI[(i-5):(i-1)])
      data$reliability[i] <- 0
      print(paste0("BNI NAs imputed idx: ", i))
    }
    if(is.na(data$clearsky_GHI[i])){
      data$clearsky_GHI[i] <- mean(data$clearsky_GHI[(i-5):(i-1)])
      print(paste0("clearsky_GHI NAs imputed idx: ", i))
    }
    if(is.na(data$clearsky_BHI[i])){
      data$clearsky_BHI[i] <- mean(data$clearsky_BHI[(i-5):(i-1)])
      print(paste0("clearsky_BHI NAs imputed idx: ", i))
    }
    if(is.na(data$clearsky_DHI[i])){
      data$clearsky_DHI[i] <- mean(data$clearsky_DHI[(i-5):(i-1)])
      print(paste0("clearsky_DHI NAs imputed idx: ", i))
    }
    if(is.na(data$clearsky_BNI[i])){
      data$clearsky_BNI[i] <- mean(data$clearsky_BNI[(i-5):(i-1)])
      print(paste0("clearsky_BNI NAs imputed idx: ", i))
    }
  }
  # Reorder variables
  data <- dplyr::select(data, date, place, lat, lon, alt,
                        TOA, GHI, BHI, DHI, BNI, clearsky_GHI,
                        clearsky_BHI, clearsky_DHI, clearsky_BNI,
                        reliability)
}

#' Get a time series of solar radiation from CAMS Api directly as `tibble` without the necessity of store `.csv` files.
#' Call the python function `cams_solar_radiation_ts`.
#' @keywords internal
cams_solar_radiation_ts_download = function(place, lat, lon, alt, from = "2005-01-01", to = Sys.Date()){

  to <- as.character(to)
  from <- as.character(from)
  # Create in a temporary directory a temporary file csv
  temp <- base::tempfile(pattern = place, fileext = ".csv")
  # Download the file
  cams_solar_radiation_ts(latitude = lat, longitude = lon, start = from, end = to,
                          altitude = alt, filename = temp)
  # Metadata
  meta <- readr::read_delim(temp,  skip = 0, n_max = 40, delim = ":", show_col_types = FALSE, progress = FALSE)
  # Solar data
  data <- readr::read_delim(temp, skip = 42, delim = ";", show_col_types = FALSE, progress = FALSE)
  # Processed data
  data <- cams_solar_radiation_process_l0_to_l1(place = place, data = data, meta = meta)
  # Unlink the connection created with temp
  base::unlink(temp)
  return(data)
}

#' @keywords internal
cams_solar_radiation_read = function(data, year_max = lubridate::year(Sys.Date()), from = NULL, to = NULL, scale_GHI = 1000){

  # Filter for minimum date
  if (!is.null(from)) {
    data <- dplyr::filter(data, date >= from)
  }
  # Filter for maximum date
  if (!is.null(to)) {
    data <- dplyr::filter(data, date <= to)
  }
  # Add auxiliary variables
  data$Year <- lubridate::year(data$date) # year
  data$Month <- lubridate::month(data$date) # month of the year
  data$Day <- lubridate::day(data$date) # day of the month
  data$n <- solarr::number_of_day(data$date)
  # Filter for maximum year
  data <- dplyr::filter(data, Year <= year_max)
  # Create dataset
  data <- dplyr::mutate(data, GHI = GHI/scale_GHI, clearsky = clearsky_GHI/scale_GHI)
  # Extraterrestrial radiation for an year with 366 days
  H0_extra <- solarr::solar_extraterrestrial_radiation(lat = data$lat[1],
                                                       day_date = "2020-01-01",
                                                       day_end = "2020-12-31")
  # Month, Day and rescale G0 in kWh/m2
  H0_extra <- dplyr::mutate(H0_extra,
                            H0 = G0/(3600*scale_GHI),
                            Day = lubridate::day(date),
                            Month = lubridate::month(date))

  data <- dplyr::left_join(data, dplyr::select(H0_extra, Month, Day, H0), by = c("Month", "Day"))
  attr(data, "place") <- data$place[1]
  attr(data, "coords") <- list(lat = data$lat[1],
                               lon = data$lon[1],
                               alt = data$alt[1])
  attr(data, "seasonal") <- H0_extra
  data <- dplyr::select(data, date, n, Year, Month, Day, GHI, clearsky, H0)
  return(data)
}


