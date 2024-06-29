cams_solar_radiation_ts <- NULL
.onLoad <- function(libname, pkgname) {
  module <- reticulate::import_from_path(module = "cams_solar_radiation_ts", path = system.file("py", package = packageName()))
  cams_solar_radiation_ts <<- module$cams_solar_radiation_ts
}

