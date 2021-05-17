# rename MODIS active fire detection data to be more user friendly
# First step is to download all MODIS active fire detection data (MCD14ML) by
# putting in requests at a global extent for individual years from the
# raw data source (FIRMS)

# put them into the data/raw/mcd14ml folder in your local disk

library(sf)
library(data.table)
library(clock)
library(lubridate)
library(oce)
library(pbapply)
library(parallel)
library(stringr)

collection <- "c006"
version <- "v03"

dir.create("data/out/mcd14ml/temp", recursive = TRUE, showWarnings = FALSE)
afd_files <- list.files("data/raw/mcd14ml", pattern = ".zip", full.names = TRUE)

extract_and_rename_mcd14ml <- function(zipfile) {
  unzip(zipfile = zipfile, exdir = "data/out/mcd14ml/temp")
  readme_fname <- list.files("data/out/mcd14ml/temp", pattern = ".txt", full.names = TRUE)
  unlink(readme_fname)
  
  obj_fnames <- list.files("data/out/mcd14ml/temp", full.names = TRUE)
  
  if(any(grepl(x = obj_fnames, pattern = ".shp"))) {
    obj_fname <- list.files("data/out/mcd14ml/temp", pattern = ".shp", full.names = TRUE)  
    obj <- sf::st_read(obj_fname) %>% sf::st_drop_geometry()
  } else if(any(grepl(x = obj_fnames, pattern = ".geojson"))) {
    obj_fname <- list.files("data/out/mcd14ml/temp", pattern = ".geojson", full.names = TRUE)  
    obj <- sf::st_read(obj_fname) %>% sf::st_drop_geometry()
  } else if(any(grepl(x = obj_fnames, pattern = ".csv"))) {
    obj_fname <- list.files("data/out/mcd14ml/temp", pattern = ".csv", full.names = TRUE)  
    obj <- data.table::fread(obj_fname, colClasses = c(acq_time = "character"))
  }
  
  names(obj) <- tolower(names(obj))
  
  obj <- as.data.table(obj)
  # Just in case, make sure that the acq_time column is a character with padded 0's on the left
  # such that it has a width of 4 individual characters
  obj[, acq_time := stringr::str_pad(string = acq_time, width = 4, side = "left", pad = "0")]
  
  # add solar elevation angle as a new column
  obj[, acq_dttme := lubridate::ymd_hm(paste(acq_date, acq_time), tz = "UTC")]
  obj[, solar_ang := oce::sunAngle(t = acq_dttme,
                                   longitude = longitude,
                                   latitude = latitude,
                                   useRefraction = FALSE)$altitude]
  
  obj[, dn_detect := ifelse(solar_ang >= 0, yes = "day", no = "night")]
  
  this_year <- unique(clock::get_year(obj$acq_date))
  
  out_fname <- paste0("data/out/mcd14ml/mcd14ml_", collection, "_", version, "_", this_year, ".csv")
  
  data.table::fwrite(x = obj, file = out_fname)
  
  unlink("data/out/mcd14ml/temp/*", recursive = TRUE)
}

pblapply(X = afd_files, FUN = extract_and_rename_mcd14ml)
