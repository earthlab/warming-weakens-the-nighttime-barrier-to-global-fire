# Defining nighttime fire regime by FRP and detections

library(tidyverse)
library(raster)
library(data.table)
library(viridis)
library(terra)


# gridded MCD14ML data ---------------------------------------------------------

system2(command = "aws", args = "s3 sync s3://earthlab-jmcglinchy/night_fire/gridded/vars_refresh_may2021/CSV_nocorn_grid_0_25_degree_vars/ data/out/CSV_nocorn_grid_0_25_degree_vars")


# fire detection count ----------------------------------------------------

afd_files <- list.files("data/out/CSV_nocorn_grid_0_25_degree_vars/AFC_num/", full.names = TRUE) %>% 
  tibble::enframe(name = NULL) %>% 
  setNames("full_fname") %>% 
  dplyr::mutate(filename = basename(full_fname)) %>% 
  tidyr::separate(col = filename, into = c("satellite", "daynight", "afc", "datatype", "month", "year", "fileextension"), remove = FALSE) %>% 
  dplyr::select(-satellite, -afc, -datatype, -fileextension) %>% 
  dplyr::mutate(year = as.numeric(year),
                daynight = ifelse(daynight == "D", yes = "day", no = "night")) %>% 
  dplyr::filter(year >= 2003)

read_and_stack_rasters <-  function(full_fname, filename, daynight, month, year) {
  r <- terra::rast(full_fname)
  return(r)
}

# Stack all 216 year-month rasters representing total detections
# and sum them
if (!file.exists("data/out/total-detections_day_0.25.tif")) {
  afd_day <-
    afd_files %>% 
    dplyr::filter(daynight == "day") %>% 
    purrr::pmap(.f = read_and_stack_rasters) %>% 
    do.call("c", .) %>% 
    sum()
  
  # Line up the rasters with the GLDAS landcover raster
  afd_day <- terra::shift(x = afd_day, dx = -0.25, dy = 0.25)
  
  terra::writeRaster(x = afd_day, filename = "data/out/total-detections_day_0.25.tif", overwrite = TRUE)
}

# Repeat for night detections rasters
if (!file.exists("data/out/total-detections_night_0.25.tif")) {
  afd_night <-
    afd_files %>% 
    dplyr::filter(daynight == "night") %>% 
    purrr::pmap(.f = read_and_stack_rasters) %>% 
    do.call("c", .) %>% 
    sum()
  
  # Line up the rasters with the GLDAS landcover raster
  afd_night <- terra::shift(x = afd_night, dx = -0.25, dy = 0.25)
  
  terra::writeRaster(x = afd_night, filename = "data/out/total-detections_night_0.25.tif", overwrite = TRUE)
}

afd_day <- terra::rast("data/out/total-detections_day_0.25.tif")
afd_night <- terra::rast("data/out/total-detections_night_0.25.tif")

# FRP data ----------------------------------------------------------------

frp_files <- list.files("data/out/CSV_nocorn_grid_0_25_degree_vars/FRP_total/", full.names = TRUE) %>% 
  tibble::enframe(name = NULL) %>% 
  setNames("full_fname") %>% 
  dplyr::mutate(filename = basename(full_fname)) %>% 
  tidyr::separate(col = filename, into = c("satellite", "daynight", "afc", "datatype", "month", "year", "fileextension"), remove = FALSE) %>% 
  dplyr::select(-satellite, -afc, -datatype, -fileextension) %>% 
  dplyr::mutate(year = as.numeric(year),
                daynight = ifelse(daynight == "D", yes = "day", no = "night")) %>% 
  dplyr::filter(year >= 2003)

if(!file.exists("data/out/frp_day_0.25.tif")) {
  # Stack all 216 year-month rasters representing total FRP
  # and then sum them
  frp_day <-
    frp_files %>% 
    dplyr::filter(daynight == "day") %>% 
    purrr::pmap(.f = read_and_stack_rasters) %>% 
    do.call("c", .) %>% 
    sum()
  
  # Line up the rasters with the GLDAS landcover raster
  frp_day <- terra::shift(x = frp_day, dx = -0.25, dy = 0.25)
  
  terra::writeRaster(x = frp_day, filename = "data/out/frp_day_0.25.tif", overwrite = TRUE)
}

if(!file.exists("data/out/frp_night_0.25.tif")) {
  # Repeat for the night rasters
  frp_night <-
    frp_files %>% 
    dplyr::filter(daynight == "night") %>% 
    purrr::pmap(.f = read_and_stack_rasters) %>% 
    do.call("c", .) %>% 
    sum()
  
  # Line up the rasters with the GLDAS landcover raster
  frp_night <- terra::shift(x = frp_night, dx = -0.25, dy = 0.25)
  
  terra::writeRaster(x = frp_night, filename = "data/out/frp_night_0.25.tif", overwrite = TRUE)
}

frp_day <- terra::rast("data/out/frp_day_0.25.tif")
frp_night <- terra::rast("data/out/frp_night_0.25.tif")

# overpass count ----------------------------------------------------------

system2(command = "aws", args = "s3 sync s3://earthlab-mkoontz/MODIS-overpass-counts_0.25_analysis-ready data/out/modis-overpass-corrections/MODIS-overpass-counts_0.25_analysis-ready")

# Represents count of all day (or night) overpasses between 2003 and 2018
day_overpass_count <- terra::rast("data/out/modis-overpass-corrections/MODIS-overpass-counts_0.25_analysis-ready/2003-2020/2003-2020_day_overpass-count.tif")
night_overpass_count <- terra::rast("data/out/modis-overpass-corrections/MODIS-overpass-counts_0.25_analysis-ready/2003-2020/2003-2020_night_overpass-count.tif")

day_overpass_count <- terra::shift(day_overpass_count, dx = -0.25, dy = 0.25)
night_overpass_count <- terra::shift(night_overpass_count, dx = -0.25, dy = 0.25)

# landcover ---------------------------------------------------------------

# The lookup table to convert landcover index (in the raster) to
# the landcover name
landcover_table <- 
  read.csv(file = "data/out/zero-goes-af-vpd-thresholds-with-landcover-codes.csv")

# In the 4326 coordinate reference system, pixels have different
# areas, so we need to account for that as we do aggregations of the
# gridded product
landcover_DT <- data.table::fread("data/out/area-per-lc-pixel.csv", key = c("x_lc", "y_lc"))

# Convert landcover areas to a data.table for joining
total_landcover_areas_DT <- data.table::fread("data/out/area-per-lc.csv")[!is.na(lc)]

# correct for overpasses --------------------------------------------------
day_DT <-
  c(afd_day, frp_day, day_overpass_count) %>% 
  as.data.frame(xy = TRUE) %>% 
  setNames(c("x_lc", "y_lc", "total_detections", "total_frp", "total_overpasses")) %>% 
  dplyr::mutate(daynight = "day") %>% 
  as.data.table()

night_DT <-
  c(afd_night, frp_night, night_overpass_count) %>% 
  as.data.frame(xy = TRUE) %>% 
  setNames(c("x_lc", "y_lc", "total_detections", "total_frp", "total_overpasses")) %>% 
  dplyr::mutate(daynight = "night") %>% 
  as.data.table()

daynight_DT <- rbind(day_DT, night_DT)

daynight_DT <- daynight_DT[, `:=`(detections_per_overpass = total_detections / total_overpasses,
                                  frp_per_overpass = total_frp / total_overpasses)]

daynight_DT <- daynight_DT[!is.na(detections_per_overpass)]
data.table::setkey(x = daynight_DT, x_lc, y_lc, daynight)

# join fire product with landcover -----------------------------------------------------

lon_lat_landcover <- landcover_DT[daynight_DT, on = c("x_lc", "y_lc")][lc %in% landcover_table$koppen_modis_code, ]

# summarize across all pixels belonging to each landcover type (and daynight)
afd_frp_landcover <- lon_lat_landcover[detections_per_overpass != Inf, .(total_detections = sum(total_detections),
                                                                         detections_per_overpass = sum(detections_per_overpass),
                                                                         total_frp = sum(total_frp),
                                                                         frp_per_overpass = sum(frp_per_overpass)),
                                       by = .(lc, daynight)]

afd_frp_landcover[, mean_frp_per_detection := total_frp / total_detections]
afd_frp_landcover <- total_landcover_areas_DT[afd_frp_landcover, on = "lc"]

afd_frp_landcover <- afd_frp_landcover[, `:=`(detections_per_overpass_per_Mkm2 = 1e6 * detections_per_overpass / area_km2,
                                              frp_per_overpass_per_Mkm2 = 1e6 * frp_per_overpass / area_km2)]

#####
# summarize across all pixels belonging to burnable landcover types (and daynight)
afd_frp_just_daynight <- lon_lat_landcover[detections_per_overpass != Inf, .(total_detections = sum(total_detections),
                                                                             detections_per_overpass = sum(detections_per_overpass),
                                                                             total_frp = sum(total_frp),
                                                                             frp_per_overpass = sum(frp_per_overpass)),
                                           by = .(daynight)]

afd_frp_just_daynight[, mean_frp_per_detection := total_frp / total_detections]

total_burnable_area_km2 <-
  total_landcover_areas_DT %>% 
  dplyr::filter(lc %in% landcover_table$koppen_modis_code) %>% 
  dplyr::summarize(area_km2 = sum(area_km2)) %>% 
  pull()

afd_frp_just_daynight$area_km2 <- total_burnable_area_km2

afd_frp_just_daynight[, `:=`(detections_per_overpass_per_Mkm2 = 1e6 * detections_per_overpass / area_km2,
                             frp_per_overpass_per_Mkm2 = 1e6 * frp_per_overpass / area_km2)]

# pivot wider to put similar day and night measures in separate columns (so we can get
# percent)
afd_frp_landcover_wide <-
  afd_frp_landcover %>% 
  dplyr::select(lc, area_km2, daynight, detections_per_overpass, detections_per_overpass_per_Mkm2, mean_frp_per_detection, frp_per_overpass, frp_per_overpass_per_Mkm2) %>% 
  tidyr::pivot_wider(names_from = "daynight", values_from = c("detections_per_overpass", "detections_per_overpass_per_Mkm2", "mean_frp_per_detection", "frp_per_overpass", "frp_per_overpass_per_Mkm2")) %>% 
  dplyr::mutate(pct_night_afd = 100 * detections_per_overpass_per_Mkm2_night / (detections_per_overpass_per_Mkm2_night + detections_per_overpass_per_Mkm2_day),
                pct_night_frp = 100 * frp_per_overpass_per_Mkm2_night / (frp_per_overpass_per_Mkm2_night + frp_per_overpass_per_Mkm2_day))


afd_frp_just_daynight_wide <-
  afd_frp_just_daynight %>% 
  dplyr::select(daynight, area_km2, detections_per_overpass_per_Mkm2, mean_frp_per_detection, frp_per_overpass_per_Mkm2) %>% 
  dplyr::mutate(landcover = "All Burnable Landcovers",
                area_Mkm2 = area_km2 / 1e6) %>% 
  tidyr::pivot_wider(names_from = "daynight", values_from = c("detections_per_overpass_per_Mkm2", "mean_frp_per_detection", "frp_per_overpass_per_Mkm2")) %>% 
  dplyr::mutate(pct_night_afd = 100 * detections_per_overpass_per_Mkm2_night / (detections_per_overpass_per_Mkm2_night + detections_per_overpass_per_Mkm2_day),
                pct_night_frp = 100 * frp_per_overpass_per_Mkm2_night / (frp_per_overpass_per_Mkm2_night + frp_per_overpass_per_Mkm2_day))

# Make table suitable for printing
# final row to be appended as the "total"

total_row <-
  afd_frp_just_daynight_wide %>% 
  dplyr::select(landcover, 
                area_Mkm2,
                detections_per_overpass_per_Mkm2_day, 
                detections_per_overpass_per_Mkm2_night,
                pct_night_afd,
                mean_frp_per_detection_day,
                mean_frp_per_detection_night,
                frp_per_overpass_per_Mkm2_day,
                frp_per_overpass_per_Mkm2_night, 
                pct_night_frp) %>% 
  dplyr::mutate_if(is.numeric, sprintf, fmt = "%.1f")


night_fire_regime_table <-
  afd_frp_landcover_wide %>% 
  dplyr::left_join(landcover_table, by = c(lc = "koppen_modis_code")) %>% 
  dplyr::rename(landcover = "lc_name") %>% 
  dplyr::mutate(area_Mkm2 = area_km2 / 1e6) %>% 
  dplyr::arrange(dplyr::desc(pct_night_afd)) %>% 
  dplyr::select(-lc) %>% 
  dplyr::arrange(vpd_thresh_hpa) %>% 
  dplyr::select(landcover, 
                area_Mkm2,
                detections_per_overpass_per_Mkm2_day, 
                detections_per_overpass_per_Mkm2_night,
                pct_night_afd,
                mean_frp_per_detection_day,
                mean_frp_per_detection_night,
                frp_per_overpass_per_Mkm2_day,
                frp_per_overpass_per_Mkm2_night, 
                pct_night_frp) %>% 
  dplyr::mutate_if(is.numeric, sprintf, fmt = "%.1f") %>% 
  rbind(total_row)

night_fire_regime_table

dir.create("tables", showWarnings = FALSE)
write.csv(x = night_fire_regime_table, file = "tables/night-fire-regime-table.csv", row.names = FALSE)
