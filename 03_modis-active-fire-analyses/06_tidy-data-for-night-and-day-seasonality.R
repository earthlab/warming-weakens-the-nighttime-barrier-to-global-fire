# define the day and nighttime fire season by landcover type

library(dplyr)
library(sf)
library(terra)
library(data.table)
library(cowplot)
library(ncdf4)
library(rasterDT)
library(slider)
library(ggtext)
library(stringr)
library(purrr)
library(lubridate)
library(ggplot2)

# gridded MCD14ML data ---------------------------------------------------------
system2(command = "aws", args = "s3 sync s3://earthlab-jmcglinchy/night_fire/gridded/vars_refresh_may2021/CSV_nocorn_grid_0_25_degree_vars/ data/out/CSV_nocorn_grid_0_25_degree_vars")

# get a raster template ---------------------------------------------------
raster_template <- terra::rast("data/out/CSV_nocorn_grid_0_25_degree_vars/AFC_num/modis_D_AFC_num_April_2001.tif")
raster_template <- terra::shift(x = raster_template, dx = -0.25, dy = 0.25)
# get overpass correction data --------------------------------------------

system2(command = "aws", args = "s3 sync s3://earthlab-mkoontz/MODIS-overpass-counts_0.25_analysis-ready data/out/modis-overpass-corrections/MODIS-overpass-counts_0.25_analysis-ready")

months <- str_pad(string = as.character(1:12), width = 2, side = "left", pad = "0")

all_rasters <-
  tidyr::crossing(months, dn_detect = c("day", "night")) %>%
  dplyr::mutate(s3_path = paste0("https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/MODIS-overpass-counts_0.25_analysis-ready/month_2003-2020/", months, "_2003-2020_", dn_detect, "_overpass-count.tif"),
                local_path = paste0("data/out/modis-overpass-corrections/MODIS-overpass-counts_0.25_analysis-ready/month_2003-2020/", months, "_2003-2020_", dn_detect, "_overpass-count.tif"))

# local_files <- list.files("data/out/modis-overpass-corrections/MODIS-overpass-counts_0.25_analysis-ready/month_2003-2020/", full.names = TRUE)

dir.create("data/out/modis-overpass-corrections/MODIS-overpass-counts_0.25_analysis-ready/month_2003-2020_DT/", showWarnings = FALSE)

if (length(list.files("data/out/modis-overpass-corrections/MODIS-overpass-counts_0.25_analysis-ready/month_2003-2020_DT/", full.names = TRUE)) == 0) {
  all_overpasses <-
    all_rasters %>% 
    purrr::pmap(.f = function(months, dn_detect, s3_path, local_path) {
      r <- terra::rast(local_path)
      r <- terra::shift(x = r, dx = -0.25, dy = 0.25)
      r_df <- as.data.frame(r, xy = TRUE)
      DT <- as.data.table(r_df) %>% setNames(c("lon", "lat", "overpass_count"))
      DT[, `:=`(dn_detect = dn_detect,
                month = months)]
      
      dt_path <- paste0("data/out/modis-overpass-corrections/MODIS-overpass-counts_0.25_analysis-ready/month_2003-2020_DT/", months, "_2003-2020_", dn_detect, "_overpass-count_DT.csv")
      data.table::fwrite(x = DT, file = dt_path)
      print(dt_path)
      return(DT)
    })
}

all_overpasses <-
  list.files("data/out/modis-overpass-corrections/MODIS-overpass-counts_0.25_analysis-ready/month_2003-2020_DT/", full.names = TRUE) %>% 
  lapply(fread) %>% 
  data.table::rbindlist()

all_overpasses[, month := str_pad(string = month, width = 2, side = "left", pad = "0")]
data.table::setkeyv(all_overpasses, c("dn_detect", "month", "lon", "lat"))

# table of the first of each month in DOY ---------------------------------

first_of_months <-
  tibble(month = 1:12, name = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), name_abbrv = substr(name, start = 1, stop = 3), days_in_month = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
         doy_first = lag(cumsum(days_in_month) + 1)) %>% 
  dplyr::mutate(doy_first = ifelse(month == 1, yes = 1, no = doy_first),
                doy_last = lead(doy_first) - 1,
                doy_last = ifelse(month == 12, yes = 365, no = doy_last),
                doys_in_month = map2(doy_first, doy_last, seq)) %>% 
  dplyr::mutate(doy_first_leap = ifelse(month > 2, yes = doy_first + 1, no = doy_first),
                doy_last_leap = ifelse(month > 1, yes = doy_last + 1, no = doy_last),
                doys_in_month_leap = map2(doy_first_leap, doy_last_leap, seq))

# get MCD14ML active fire detections --------------------------------------

years <- 2003:2020

afd <- lapply(X = years, 
              FUN = function(this_year) {
                fread(paste0("data/out/mcd14ml_analysis-ready/mcd14ml_c006_v03_", this_year, ".csv"), colClasses = c(acq_time = "character"))
              })

afd <- data.table::rbindlist(afd)
afd <- afd[type == 0 & confidence >= 10]

afd[, acq_month := data.table::month(acq_dttme)]
afd[, acq_year := data.table::year(acq_dttme)]
afd[, doy := data.table::yday(acq_dttme)]

afd[, month := str_pad(string = acq_month, width = 2, side = "left", pad = "0")]

# Aggregate by doy and year first, so we can deal with leap years
# Note we sum the FRP here, so will have to divide by total number of detections later
# to get mean FRP per detection
night_fires_by_lon_lat_year <- afd[, .(N = .N, frp = sum(frp)), by = .(lon = x_lc, lat = y_lc, year = acq_year, doy, dn_detect)]

# Feburary is weird (with either 28 or 29 days), and so the doy (day of year) and month 
# from something like lubridate::month(datetime) don't always match up and it makes grouping by
# the lubridate::month(datetime) challenging. Instead, we grouped by doy first, then re-assigned
# month based on leap years
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month[[1]], month := "01"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month[[2]], month := "02"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month[[3]], month := "03"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month[[4]], month := "04"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month[[5]], month := "05"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month[[6]], month := "06"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month[[7]], month := "07"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month[[8]], month := "08"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month[[9]], month := "09"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month[[10]], month := "10"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month[[11]], month := "11"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month[[12]], month := "12"]

leap_years_in_record <- c(2004, 2008, 2012, 2016, 2020)

night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month_leap[[1]] & year %in% leap_years_in_record, month := "01"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month_leap[[2]] & year %in% leap_years_in_record, month := "02"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month_leap[[3]] & year %in% leap_years_in_record, month := "03"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month_leap[[4]] & year %in% leap_years_in_record, month := "04"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month_leap[[5]] & year %in% leap_years_in_record, month := "05"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month_leap[[6]] & year %in% leap_years_in_record, month := "06"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month_leap[[7]] & year %in% leap_years_in_record, month := "07"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month_leap[[8]] & year %in% leap_years_in_record, month := "08"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month_leap[[9]] & year %in% leap_years_in_record, month := "09"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month_leap[[10]] & year %in% leap_years_in_record, month := "10"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month_leap[[11]] & year %in% leap_years_in_record, month := "11"]
night_fires_by_lon_lat_year[doy %in% first_of_months$doys_in_month_leap[[12]] & year %in% leap_years_in_record, month := "12"]

data.table::setkeyv(night_fires_by_lon_lat_year, cols = c("dn_detect", "month", "lon", "lat"))

# overpasses_per_DOY_2003_2020 represents the cumulative number of overpasses for that DOY/lon/lat combination
# across all 18 years -- 2003 to 2020
detections_per_doy_lon_lat_year <- all_overpasses[night_fires_by_lon_lat_year, on = c("lon", "lat", "dn_detect", "month")]

detections_per_doy_lon_lat_year[month %in% c("01", "03", "05", "07", "08", "10", "12"), overpasses_per_DOY_2003_2020 := overpass_count / 31]
detections_per_doy_lon_lat_year[month %in% c("04", "06", "09", "11"), overpasses_per_DOY_2003_2020 := overpass_count / 30]
detections_per_doy_lon_lat_year[(month == "02") & (year %in% leap_years_in_record), overpasses_per_DOY_2003_2020 := overpass_count / 29]
detections_per_doy_lon_lat_year[(month == "02") & (!(year %in% leap_years_in_record)), overpasses_per_DOY_2003_2020 := overpass_count / 28]

data.table::setkeyv(detections_per_doy_lon_lat_year, cols = c("lon", "lat"))

# get landcover data ------------------------------------------------------

landcover_table <- 
  read.csv("data/out/zero-goes-af-vpd-thresholds-with-landcover-codes.csv") %>%
  dplyr::mutate(landcover_split = str_replace(lc_name, pattern = " ", replacement = "\n")) %>% 
  dplyr::mutate(landcover_split = gsub(pattern = "Natural Vegetation Mosaics", replacement = "Natural\nVegetation Mosaics", x = landcover_split)) %>% 
  dplyr::mutate(landcover_split = gsub(pattern = "Needleleaf Forests", replacement = "Needleleaf\nForests", x = landcover_split)) %>% 
  dplyr::mutate(landcover_split = gsub(pattern = "Broadleaf Forests", replacement = "Broadleaf\nForests", x = landcover_split))

landcover_DT <- data.table::fread("data/out/area-per-lc-pixel.csv") %>% setNames(c("cell_id_lc", "lon", "lat", "lc", "area_m2"))
data.table::setkeyv(landcover_DT, cols = c("lon", "lat"))

total_landcover_areas <- data.table::fread("data/out/area-per-lc.csv")[lc %in% landcover_table$koppen_modis_code]

total_landcover_areas_hemisphere <-
  landcover_DT %>%
  dplyr::mutate(hemisphere = ifelse(lat < 0, yes = "southern", no = "northern")) %>%
  dplyr::group_by(lc, hemisphere) %>%
  dplyr::summarize(area_km2 = as.numeric(sum(area_m2, na.rm = TRUE) / 1e6)) %>%
  dplyr::left_join(landcover_table, by = c(lc = "koppen_modis_code"))

detections_per_doy_lon_lat_year_landcover <- landcover_DT[detections_per_doy_lon_lat_year, on = c("lon", "lat")]
data.table::setkeyv(detections_per_doy_lon_lat_year_landcover, cols = c("lon", "lat", "cell_id_lc"))

detections_per_doy_year_landcover <- detections_per_doy_lon_lat_year_landcover[, .(area_with_detections = sum(area_m2)), by = .(lc, dn_detect, year, doy)]

detections_per_doy_year_landcover <-
  detections_per_doy_year_landcover %>% 
  dplyr::left_join(landcover_table, by = c(lc = "koppen_modis_code")) %>% 
  as.data.table()

area_with_detections_per_doy_landcover <- detections_per_doy_year_landcover[, .(mean_area_with_detections_per_year = mean(area_with_detections)), by = .(lc, dn_detect, doy)]

area_with_detections_per_doy_landcover <- 
  area_with_detections_per_doy_landcover %>% 
  dplyr::left_join(total_landcover_areas, by = "lc") %>% 
  as.data.table()

# Here, the mean of the overpasses_per_DOY_2003_2020 variable is taken across the stack of 18 years
# to account for leap year shifts in which DOY corresponds to which calendar day. For instance, in 
# 2003, the 29th day of the year was in March (March 1st), but in 2004-- a leap year, the 29th day was
# in February. We tried to account for this by carefully dividing up the number of overpasses per
# lon/lat per month into categorical months and dividing by the number of days in that month (given 
# whether or not it was a leap year)
# In any case, taking the mean of overpasses_per_DOY_2003_2018 won't likely change these values
# dramatically, but it helps get our data.frame into the right shape for joining and summarizing.
detections_per_doy_lon_lat <- 
  detections_per_doy_lon_lat_year[, .(total_detections = sum(N),
                                      overpasses_per_DOY_2003_2020 = mean(overpasses_per_DOY_2003_2020),
                                      n_years_the_pixel_burned = .N,
                                      total_frp = sum(frp)),
                                  by = .(lon, lat, doy, dn_detect)]

# mean_detections_per_overpass represents the total number of detections per lon/lat across
# the 18-year period divided by the total number of overpasses per lon/lat across the 18 year period
# Thus we can think of it as the expected number of detections per year per overpass on that DOY
detections_per_doy_lon_lat[, `:=`(mean_detections_per_overpass = total_detections / overpasses_per_DOY_2003_2020,
                                  frp_per_overpass = total_frp / overpasses_per_DOY_2003_2020)]
detections_per_doy_lon_lat[, hemisphere := ifelse(lat < 0, yes = "southern", no = "northern")]

detections_per_doy_lon_lat_landcover <- landcover_DT[detections_per_doy_lon_lat]

# Here, we sum across all the different lon/lat values into groups by landcover instead
# total_detections is all of the detections across 18 years in that landcover type
# mean_detetions_per_overpass_on_DOY is the expected annual detection/overpass across the whole
# landcover type
detections_per_doy_landcover <-  
  detections_per_doy_lon_lat_landcover[!is.infinite(mean_detections_per_overpass), 
                                       .(total_detections = sum(total_detections),
                                         total_frp = sum(total_frp),
                                         mean_detections_per_overpass_on_DOY = sum(mean_detections_per_overpass),
                                         frp_per_overpass_on_DOY = sum(frp_per_overpass),
                                         sum_area_affected_by_detections_on_DOY_km2 = sum(area_m2) / 1e6,
                                         mean_annual_area_affected_by_detections_on_DOY_km2 = sum(area_m2 / 1e6 * n_years_the_pixel_burned / 18),
                                         n_pixels_with_detections = .N), 
                                       by = .(lc, doy, dn_detect)]

# Join with the table representing how much land area is covered by each landcover type
detections_per_doy_landcover <-
  detections_per_doy_landcover %>% 
  dplyr::left_join(total_landcover_areas, by = "lc") %>% 
  dplyr::mutate(mean_detections_per_overpass_on_DOY_per_km2 = mean_detections_per_overpass_on_DOY / area_km2,
                pct_of_landcover_area_affected_by_detections = mean_annual_area_affected_by_detections_on_DOY_km2 / area_km2,
                detections_per_pixel = total_detections / n_pixels_with_detections,
                frp_per_detection = total_frp / total_detections,
                frp_per_overpass_on_DOY_per_km2 = frp_per_overpass_on_DOY / area_km2) %>% 
  as.data.table()

# Same set of aggregations as above, except now also breaking down by northern and southern
# hemisphere
detections_per_doy_landcover_hemisphere <-  
  detections_per_doy_lon_lat_landcover[!is.infinite(mean_detections_per_overpass), 
                                       .(total_detections = sum(total_detections),
                                         total_frp = sum(total_frp),
                                         mean_detections_per_overpass_on_DOY = sum(mean_detections_per_overpass),
                                         frp_per_overpass_on_DOY = sum(frp_per_overpass),
                                         sum_area_affected_by_detections_on_DOY_km2 = sum(area_m2) / 1e6,
                                         mean_annual_area_affected_by_detections_on_DOY_km2 = sum(area_m2 / 1e6 * n_years_the_pixel_burned / 18),
                                         n_pixels_with_detections = .N), 
                                       by = .(lc, hemisphere, doy, dn_detect)]

detections_per_doy_landcover_hemisphere <-
  detections_per_doy_landcover_hemisphere %>%
  dplyr::left_join(total_landcover_areas_hemisphere, by = c("lc", "hemisphere")) %>%
  dplyr::mutate(mean_detections_per_overpass_on_DOY_per_km2 = mean_detections_per_overpass_on_DOY / area_km2 ,
                pct_of_landcover_area_affected_by_detections = mean_annual_area_affected_by_detections_on_DOY_km2 / area_km2 ,
                detections_per_pixel = total_detections / n_pixels_with_detections,
                frp_per_detection = total_frp / total_detections,
                frp_per_overpass_on_DOY_per_km2 = frp_per_overpass_on_DOY / area_km2 ) %>%
  as.data.table()


# defining the seasons ----------------------------------------------------

doy_slider <- function(this_afd, doy, delta, var) {
  
  start_doy <- doy - 1 - delta
  end_doy <- doy + delta
  
  doy_seq <- start_doy:end_doy
  
  doy_seq <- ifelse(doy_seq < 1, yes = doy_seq + 365, no = doy_seq)
  doy_seq <- ifelse(doy_seq > 365, yes = doy_seq - 365, no = doy_seq)
  
  smoothed_data <- 
    this_afd %>% 
    dplyr::filter(doy %in% doy_seq) %>% 
    dplyr::pull({{var}}) %>% 
    mean()
  
  return(smoothed_data)
}


# Correct for the total landcover
alpha_low <- 0.25
alpha_high <- 1


afd_of_interest <- 
  detections_per_doy_landcover %>% 
  dplyr::filter(lc %in% landcover_table$koppen_modis_code) %>% 
  dplyr::filter(doy != 366) %>% 
  dplyr::group_by(lc, dn_detect) %>% 
  dplyr::group_modify(.f = function(.x, ...) {
    .x <-
      .x %>% 
      dplyr::mutate(smoothed_detections = purrr::map_dbl(this_afd = ., 
                                                         doy, 
                                                         .f = doy_slider,
                                                         delta = 15,
                                                         var = mean_detections_per_overpass_on_DOY_per_km2)) %>% 
      dplyr::mutate(smoothed_frp = purrr::map_dbl(this_afd = .,
                                                  doy,
                                                  .f = doy_slider,
                                                  delta = 15,
                                                  var = frp_per_detection)) %>% 
      dplyr::mutate(smoothed_detections = smoothed_detections * (sum(mean_detections_per_overpass_on_DOY_per_km2) / sum(smoothed_detections))) %>% 
      dplyr::mutate(smoothed_frp = smoothed_frp * (sum(frp_per_detection) / sum(smoothed_frp))) %>% 
      dplyr::mutate(over_threshold_raw_detections = ifelse(mean_detections_per_overpass_on_DOY_per_km2 < 
                                                             (12/3650) * sum(mean_detections_per_overpass_on_DOY_per_km2), 
                                                           yes = alpha_low,
                                                           no = alpha_high),
                    over_threshold_smooth_detections = ifelse(smoothed_detections < 
                                                                (12/3650) * sum(smoothed_detections),
                                                              yes = alpha_low,
                                                              no = alpha_high),
                    over_threshold_raw_frp = ifelse(frp_per_detection < 
                                                      (12/3650) * sum(frp_per_detection),
                                                    yes = alpha_low,
                                                    no = alpha_high),
                    over_threshold_smooth_frp = ifelse(smoothed_frp < 
                                                         (12/3650) * sum(smoothed_frp),
                                                       yes = alpha_low,
                                                       no = alpha_high)) %>% 
      dplyr::mutate(thresholded_detections_raw = ifelse(over_threshold_raw_detections == alpha_high,
                                                        yes = mean_detections_per_overpass_on_DOY_per_km2,
                                                        no = NA),
                    thresholded_detections_smooth = ifelse(over_threshold_smooth_detections == alpha_high,
                                                           yes = smoothed_detections,
                                                           no = NA)) %>% 
      dplyr::mutate(thresholded_frp_raw = ifelse(over_threshold_raw_frp == alpha_high,
                                                 yes = frp_per_detection,
                                                 no = NA),
                    thresholded_frp_smooth = ifelse(over_threshold_smooth_frp == alpha_high,
                                                    yes = smoothed_frp,
                                                    no = NA))
    
    
    return(.x)
    
  })

# Determine landcover order
landcover_order <-
  afd_of_interest %>%
  dplyr::filter(dn_detect == "night") %>%
  dplyr::summarize(detections = sum(smoothed_detections)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(landcover_table, by = c(lc = "koppen_modis_code")) %>%
  dplyr::mutate(landcover_split = factor(landcover_split, levels = landcover_split[order(detections, decreasing = TRUE)]),
                lc_name = factor(lc_name, levels = lc_name[order(detections, decreasing = TRUE)]),
                lc = factor(lc, levels = lc[order(detections, decreasing = TRUE)]))

afd_of_interest_lc <-
  afd_of_interest %>%
  dplyr::left_join(landcover_table, by = c(lc = "koppen_modis_code")) %>% 
  dplyr::mutate(landcover_split = factor(landcover_split, levels = levels(landcover_order$landcover_split)),
                lc_name = factor(lc_name, levels = levels(landcover_order$lc_name)),
                lc = factor(lc, levels = levels(landcover_order$lc)))

data.table::fwrite(afd_of_interest_lc, "data/out/seasonality_afd-and-frp-by-day-of-year-landcover.csv")

# Also split by hemisphere
afd_of_interest_by_hemisphere <- 
  detections_per_doy_landcover_hemisphere %>% 
  dplyr::filter(lc %in% landcover_table$koppen_modis_code) %>% 
  dplyr::filter(doy != 366) %>%
  dplyr::arrange(hemisphere, lc, dn_detect, doy) %>% 
  dplyr::group_by(lc, dn_detect, hemisphere) %>% 
  dplyr::group_modify(.f = function(.x, ...) {
    .x <-
      .x %>% 
      dplyr::mutate(smoothed_detections = purrr::map_dbl(this_afd = ., 
                                                         doy, 
                                                         .f = doy_slider,
                                                         delta = 15,
                                                         var = mean_detections_per_overpass_on_DOY_per_km2)) %>% 
      dplyr::mutate(smoothed_frp = purrr::map_dbl(this_afd = .,
                                                  doy,
                                                  .f = doy_slider,
                                                  delta = 15,
                                                  var = frp_per_detection)) %>% 
      dplyr::mutate(smoothed_detections = smoothed_detections * (sum(mean_detections_per_overpass_on_DOY_per_km2) / sum(smoothed_detections))) %>% 
      dplyr::mutate(smoothed_frp = smoothed_frp * (sum(frp_per_detection) / sum(smoothed_frp))) %>% 
      dplyr::mutate(over_threshold_raw_detections = ifelse(mean_detections_per_overpass_on_DOY_per_km2 < 
                                                             (12/3650) * sum(mean_detections_per_overpass_on_DOY_per_km2), 
                                                           yes = alpha_low,
                                                           no = alpha_high),
                    over_threshold_smooth_detections = ifelse(smoothed_detections < 
                                                                (12/3650) * sum(smoothed_detections),
                                                              yes = alpha_low,
                                                              no = alpha_high),
                    over_threshold_raw_frp = ifelse(frp_per_detection < 
                                                      (12/3650) * sum(frp_per_detection),
                                                    yes = alpha_low,
                                                    no = alpha_high),
                    over_threshold_smooth_frp = ifelse(smoothed_frp < 
                                                         (12/3650) * sum(smoothed_frp),
                                                       yes = alpha_low,
                                                       no = alpha_high)) %>% 
      dplyr::mutate(thresholded_detections_raw = ifelse(over_threshold_raw_detections == alpha_high,
                                                        yes = mean_detections_per_overpass_on_DOY_per_km2,
                                                        no = NA),
                    thresholded_detections_smooth = ifelse(over_threshold_smooth_detections == alpha_high,
                                                           yes = smoothed_detections,
                                                           no = NA)) %>% 
      dplyr::mutate(thresholded_frp_raw = ifelse(over_threshold_raw_frp == alpha_high,
                                                 yes = frp_per_detection,
                                                 no = NA),
                    thresholded_frp_smooth = ifelse(over_threshold_smooth_frp == alpha_high,
                                                    yes = smoothed_frp,
                                                    no = NA))
    
    
    return(.x)
    
  })

afd_of_interest_by_hemisphere_lc <-
  afd_of_interest_by_hemisphere %>% 
  dplyr::mutate(landcover_split = factor(landcover_split, levels = levels(landcover_order$landcover_split)))

data.table::fwrite(afd_of_interest_by_hemisphere_lc, "data/out/seasonality_afd-and-frp-by-day-of-year-landcover-hemisphere.csv")
