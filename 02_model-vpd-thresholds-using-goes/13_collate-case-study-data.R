# Make case study data
library(dplyr)
library(sf)
library(oce)
library(data.table)
library(pbapply)

system2(command = "aws", args = "s3 sync s3://earthlab-amahood/night_fires/goes_counts data/out/goes_counts")
system2(command = "aws", args = "s3 sync s3://earthlab-amahood/night_fires/vpd_lc_plus_2 data/out/vpd_lc")

# Raw-er VPD (but not the rawest) for North America (the site of our example fires)
system("aws s3 cp s3://earthlab-amahood/night_fires/na_vpd_long_2017-nids.csv data/out/na_vpd_long_2017-nids.csv")

# get thresholds
thresh <- read.csv("data/out/zero-goes-af-vpd-thresholds-with-landcover-codes.csv")

# Get GOES counts
goes_counts <- 
  pbsapply(list.files("data/out/goes_counts/", full.names = TRUE), FUN = data.table::fread, USE.NAMES = TRUE) %>% 
  data.table::rbindlist(idcol = "lc_name")

goes_counts[, fbasename := basename(lc_name)]
goes_counts[, vpd_fbasename := gsub(pattern = ".csv", replacement = "_vpds.csv", x = fbasename)]
goes_counts[, lc_name := gsub(pattern = ".csv", replacement = "", x = fbasename)]
goes_counts[, lc_name := gsub(pattern = "_", replacement = " ", x = lc_name)]

# Get all events so that we can match with MTBS and get FIRED event ID
events <- sf::st_read("data/out/fired_na_2017-nids_lc.gpkg")

# Tubbs Fire -- California -- 2017
tubbs_mtbs <- 
  sf::st_read("data/raw/2017_CA_tubbs_ca3859812261820171009/ca3859812261820171009_20170707_20180710_burn_bndy.shp") %>% 
  sf::st_transform(st_crs(events)) %>% 
  filter(BurnBndAc == max(BurnBndAc))

# Camp Fire -- California -- 2018
camp_mtbs <-
  sf::st_read("data/raw/2018_CA_camp_00016737.gpkg") %>% 
  sf::st_transform(st_crs(events))

# Rice Ridge FIre -- Montana -- 2017
rice_ridge_mtbs <-
  sf::st_read("data/raw/2017_MT_rice-ridge_mt4726811348520170724/mt4726811348520170724_20160724_20180730_burn_bndy.shp") %>% 
  sf::st_transform(st_crs(events))

join_vpd_goes <- function(mtbs, event_name) {
  # spatiotemporal join of the FIRED product with the MTBS product
  fired_event <-
    events %>% 
    filter(abs(difftime(first_date_7, unique(mtbs$Ig_Date), units = "week")) < 2) %>%
    st_intersection(., mtbs) %>% 
    filter(st_area(.) == max(st_area(.)))
  
  # what is the FIRED nid that cooresponds to the MTBS event of interest?
  this_nid <- fired_event$nid
  
  # subset GOES active fire counts to just the event of interest
  goes <- goes_counts[nid %in% this_nid, ]
  
  lc_name <- unique(goes$lc_name)
  # Get the VPD data by just reading in the .csv of the correct
  # landcover, given the nid, then subset to the VPD data for
  # the particular nid
  vpd_fname <- here::here("data", "out", "vpd_lc", unique(goes$vpd_fbasename))
  this_vpd <- data.table::fread(vpd_fname)[nid %in% this_nid]
  
  # Add the rounded_datetime to each hourly VPD observation 
  this_vpd <-
    this_vpd %>% 
    mutate(rounded_datetime = lubridate::ymd(first_date) + lubridate::days(day) + lubridate::hours(hour))
  
  # aggregate GOES active fire detections by hour
  goes <- goes[, .(n = sum(n)), by = .(rounded_datetime)]
  
  # join the VPD and GOES active fire data for the particular nid of interest
  # also calculate the solar elevation angle so that we can determine whether 
  # the observation is during the night or day
  vpd_goes <- 
    goes[this_vpd, on = "rounded_datetime"] %>% 
    dplyr::select(nid, rounded_datetime, VPD_hPa, n) %>% 
    mutate(lon = st_coordinates(st_centroid(st_transform(fired_event, 4326)))[, 1],
           lat = st_coordinates(st_centroid(st_transform(fired_event, 4326)))[, 2]) %>% 
    mutate(solar_ang = oce::sunAngle(t = rounded_datetime, longitude = lon, latitude = lat, useRefraction = FALSE)$altitude) %>% 
    dplyr::mutate(dn_detect = ifelse(solar_ang >= 0, yes = "day", no = "night")) %>% 
    dplyr::mutate(vpd_kPa = VPD_hPa / 10,
                  lc_name = lc_name,
                  event_name = event_name)
  
  return(vpd_goes)
}

tubbs <- join_vpd_goes(mtbs = tubbs_mtbs, event_name = "Tubbs Fire, California (2017)")
camp <- join_vpd_goes(mtbs = camp_mtbs, event_name = "Camp Fire, California (2018)")
rice_ridge <- join_vpd_goes(mtbs = rice_ridge_mtbs, event_name = "Rice Ridge Fire, Montana (2017)")

# long_pts <- rbind(tubbs, camp, rice_ridge)
long_pts <- rbind(tubbs, rice_ridge)

write.csv(x = long_pts, file = here("data", "out", "case_study_data_2017-2020.csv"))
