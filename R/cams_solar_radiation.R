#' R6 Class for CAMS solar radiation
#'
#' @description
#' description
#'
#' @details
#' details
#'
#' @export

cams_solar_radiation <- R6::R6Class("cams_solar_radiation",
                          public = list(
                            initialize = function(dir_db = "databases/CAMS",
                                                  init_db = FALSE,
                                                  init_locations = FALSE){
                              private$dir_db <- dir_db
                              private$dir_l0 <- file.path(dir_db, "l0/solar")
                              private$dir_l1 <- file.path(dir_db, "l1/solar")
                              private$..locations <- init_CAMS_locations()

                              if (init_db) {
                                system(paste0("mkdir ", private$dir_db))
                                system(paste0("mkdir ", private$dir_db, "/l0"))
                                system(paste0("mkdir ", private$dir_db, "/l1"))
                                system(paste0("mkdir ", private$dir_l0))
                                system(paste0("mkdir ", private$dir_l1))
                                system(paste0("mkdir ", private$dir_db, "/l0/solar/metadata"))
                              }

                              if (init_locations) {
                                locations <- init_CAMS_locations()
                                readr::write_csv(locations, file = file.path(private$dir_l0, "0-locations.csv"))
                                readr::write_csv(locations, file = file.path(private$dir_l1, "0-locations.csv"))
                              } else {
                                locations <- readr::read_csv(file = file.path(private$dir_l1, "0-locations.csv"),
                                                             show_col_types = FALSE, progress = FALSE)
                              }
                              private$..locations <- locations
                            },
                            # Add the coordinates and the label for a new location
                            add = function(place, lat, lon, alt){

                              if (!(place %in% private$..locations$place)){
                                new_loc <- tibble(place = place, lat = lat, lon = lon, alt = alt,
                                                  from = as_date(NA), to = as_date(NA), nobs = 0)
                                private$..locations <- bind_rows(private$..locations, new_loc)
                                readr::write_csv(private$..locations, file = file.path(private$dir_l0, "0-locations.csv"))
                                readr::write_csv(private$..locations, file = file.path(private$dir_l1, "0-locations.csv"))
                              } else {
                                warning(paste0("Location ", place, " altready added! run the method `$locations`"))
                              }
                            },
                            # Download the raw data from CAMS
                            download = function(place, from = "2005-01-01", to = Sys.Date()){
                              to <- as.character(to)
                              from <- as.character(from)
                              lat <- private$..locations[private$..locations$place == place,]$lat
                              lon <- private$..locations[private$..locations$place == place,]$lon
                              alt <- private$..locations[private$..locations$place == place,]$alt
                              # File Name in Temp (as csv file)
                              filename <- paste0(place, ".csv")
                              filepath <- file.path(private$dir_l0, filename)
                              # Download the file
                              cams_solar_radiation_ts(latitude = lat,
                                                      longitude = lon,
                                                      start = from,
                                                      end = to,
                                                      altitude = alt,
                                                      filename = filepath)

                              filepath <- file.path(private$dir_l0, filename)
                              # Metadata
                              meta <- readr::read_delim(filepath,  skip = 0, n_max = 40, delim = ":",
                                                        show_col_types = FALSE, progress = FALSE)
                              readr::write_csv(meta, file = file.path(private$dir_l0, "metadata", filename))
                              # Solar data
                              data <- readr::read_delim(filepath, skip = 42, delim = ";", show_col_types = FALSE, progress = FALSE)
                              readr::write_csv(data, file = filepath)
                            },
                            # Read and process raw data from l0 to l1
                            process_l0_to_l1 = function(place, save = TRUE){
                              filename <- paste0(place, ".csv")
                              filepath <- file.path(private$dir_l0, filename)
                              filepath_meta <- file.path(private$dir_l0, "metadata", filename)
                              # Metadata
                              meta <- readr::read_csv(filepath_meta, show_col_types = FALSE, progress = FALSE)
                              # Solar data
                              data <- readr::read_csv(filepath, show_col_types = FALSE, progress = FALSE)
                              # Process data and metadata into a unique dataset
                              data <- cams_solar_radiation_process_l0_to_l1(place = place, data = data, meta = meta)
                              # Save files
                              if (save) readr::write_csv(data, file = file.path(private$dir_l1, filename))
                              private$..locations[private$..locations$place == place,]$from <- min(as.Date(data$date))
                              private$..locations[private$..locations$place == place,]$to <- max(as.Date(data$date))
                              private$..locations[private$..locations$place == place,]$nobs <- nrow(data)
                              readr::write_csv(private$..locations, file = file.path(private$dir_l0, "0-locations.csv"))
                              readr::write_csv(private$..locations, file = file.path(private$dir_l1, "0-locations.csv"))
                              return(data)
                            },
                            # Retrieve and process data from l1
                            read = function(place, year_max = lubridate::year(Sys.Date()),
                                            from = NULL, to = NULL, scale_GHI = 1000){
                              filename <- paste0(place, ".csv")
                              filepath <- file.path(private$dir_l1, filename)
                              # Solar data
                              data <- readr::read_csv(filepath, show_col_types = FALSE, progress = FALSE)
                              data$place <- place
                              data <- cams_solar_radiation_read(data, year_max = year_max, from = from, to = to, scale_GHI = scale_GHI)
                              return(data)
                            },
                            # Get processed CAMS data without downloading and storing files
                            get = function(place, lat, lon, alt, from = "2005-01-01", to = Sys.Date()){
                              cams_solar_radiation_ts_download(place, lat, lon, alt, from, to)
                            },
                            # Update data for a specific location
                            update = function(place){
                              df_meta <- private$..locations[private$..locations$place == place,]
                              lat <- df_meta$lat
                              lon <- df_meta$lon
                              alt <- df_meta$alt
                              from <- as.character(df_meta$to)
                              to <- as.character(Sys.Date())

                              # File Name in Temp (as csv file)
                              filename <- paste0(place, ".csv")
                              # Create in a temporary directory a temporary file csv
                              temp <- base::tempfile(pattern = place, fileext = ".csv")
                              # Download the file
                              cams_solar_radiation_ts(latitude = lat,
                                                      longitude = lon,
                                                      start = from,
                                                      end = to,
                                                      altitude = alt,
                                                      filename = temp)

                              # Solar data
                              filepath <- file.path(private$dir_l0, filename)
                              new_data <- readr::read_delim(temp, skip = 42, delim = ";", show_col_types = FALSE, progress = FALSE)
                              old_data <- readr::read_csv(file = filepath, show_col_types = FALSE, progress = FALSE)
                              upd_data <- dplyr::bind_rows(old_data, new_data)
                              upd_data <- upd_data[!duplicated(upd_data[,1]),]
                              readr::write_csv(upd_data, file = filepath)
                              unlink(temp)
                            }
                          ),
                          private = list(
                            dir_db = NA,
                            dir_l0 = NA,
                            dir_l1 = NA,
                            ..locations = NA
                          ),
                          active = list(
                            #' @field locations
                            #' Get all streams
                            locations = function(){
                              private$..locations
                            }
                          )
)
