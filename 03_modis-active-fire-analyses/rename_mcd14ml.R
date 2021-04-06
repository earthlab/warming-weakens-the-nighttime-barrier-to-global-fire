# rename MODIS active fire detection data to be more user friendly
# First step is to download all MODIS active fire detection data (MCD14ML) by
# putting in requests at a global extent for individual years from the
# raw data source (FIRMS)

# put them into the data/raw/mcd14ml folder in your local disk

library(sf)
library(data.table)
library(clock)

collection <- "c006"
version <- "v03"

dir.create("data/out/mcd14ml/temp", recursive = TRUE, showWarnings = FALSE)
afd_files <- list.files("data/raw/mcd14ml", pattern = ".zip", full.names = TRUE)

extract_and_rename_mcd14ml <- function(zipfile) {
  unzip(zipfile = zipfile, exdir = "data/out/mcd14ml/temp")
  readme_fname <- list.files("data/out/mcd14ml/temp", pattern = ".txt", full.names = TRUE)
  unlink(readme_fname)
  
  shp_fname <- list.files("data/out/mcd14ml/temp", pattern = ".shp", full.names = TRUE)  
  shp <- sf::st_read(shp_fname) %>% sf::st_drop_geometry()
  
  this_year <- unique(clock::get_year(shp$ACQ_DATE))
  
  out_fname <- paste0("data/out/mcd14ml/mcd14ml_", collection, "_", version, "_", this_year, ".csv")
  
  if(!file.exists(out_fname)) {
    data.table::fwrite(x = shp, file = out_fname)
  }
  
  unlink("data/out/mcd14ml/temp/*", recursive = TRUE)
}

for (i in 1:length(afd_files)) {
  extract_and_rename_mcd14ml(afd_files[i])
}
