# Assign landcovers to each active fire detection

library(dplyr)
library(sf)
library(raster)
library(data.table)
library(pbapply)

n_workers <- 7

dir.create("data/out/mcd14ml_analysis-ready", showWarnings = FALSE, recursive = TRUE)

afd_files <- list.files(path = "data/out/mcd14ml", pattern = ".csv", full.names = TRUE)

# landcover areas
lc <- terra::rast("data/out/lc_koppen_2010_mode.tif") %>% 
  terra::rotate()

lc_pixel_area <-
  lc %>% 
  terra::area(sum = FALSE) %>%
  setNames("area_m2") %>% 
  c(lc) %>% 
  as.data.frame(xy = TRUE, cell = TRUE) %>% 
  dplyr::rename(cell_id_lc = "cell", x_lc = "x", y_lc = "y", lc = "lc_koppen_2010_mode") %>% 
  as.data.table()

data.table::fwrite(lc_pixel_area, file = "data/out/area-per-lc-pixel.csv")
area_per_lc <- lc_pixel_area[, .(area_km2 = sum(area_m2) / 1e6), by = .(lc)]
data.table::fwrite(area_per_lc, file = "data/out/area-per-lc.csv")

# adding landcover per pixel
# setup parallelization
if (.Platform$OS.type == "windows") {
  cl <- parallel::makeCluster(n_workers)
  parallel::clusterEvalQ(cl = cl, expr = {
    library(dplyr)
    library(sf)
    library(raster)
    library(terra)
    library(data.table)
    
    # landcover
    lc <- terra::rast("data/out/lc_koppen_2010_mode.tif") %>% 
      terra::rotate()
    
  })
} else {
  cl <- n_workers

  # landcover
  lc <- terra::rast("data/out/lc_koppen_2010_mode.tif") %>% 
    terra::rotate()
}

pblapply(X = afd_files, FUN = function(x) {
  afd <- 
    data.table::fread(x, colClasses = c(acq_time = "character")) %>% 
    cbind(terra::extract(x = lc, y = .[, c("longitude", "latitude")], cells = TRUE)) %>% 
    cbind(terra::xyFromCell(object = lc, cell = .$cell)) %>% 
    dplyr::rename(cell_id_lc = "cell", lc = "lc_koppen_2010_mode", x_lc = "x", y_lc = "y")
  
  afd[, ID := NULL]
  
  data.table::fwrite(x = afd, file = paste0("data/out/mcd14ml_analysis-ready/", basename(x)))
}, cl = cl)

parallel::stopCluster(cl)
system2(command = "aws", args = "s3 sync data/out/mcd14ml_analysis-ready/ s3://earthlab-mkoontz/mcd14ml_analysis-ready")
