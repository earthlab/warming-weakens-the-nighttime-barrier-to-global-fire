library(tidyverse)
library(glue)
library(terra)
library(sf)
library(slider)
library(purrr)
library(furrr)
library(future)

dir.create("data/out/goes16/", showWarnings = FALSE)

get_latest_goes <- FALSE

if(get_latest_goes | !file.exists("data/out/goes16-filenames.csv")) {
  source("01_get-goes16/get-af-metadata.R")
}  

# Read in the GOES metadata acquired from Amazon Earth using get-af-metadata.R script
goes_af <- read.csv("data/out/goes16-filenames.csv", colClasses = c(scan_center = "character"))

# Get the flag values that are important using an example .nc file if not done already
# flag_vals                            flag_meanings
# 10                                   good_fire_pixel
# 11                              saturated_fire_pixel
# 12                     cloud_contaminated_fire_pixel
# 13                       high_probability_fire_pixel
# 14                     medium_probability_fire_pixel
# 15                        low_probability_fire_pixel
# 30               temporally_filtered_good_fire_pixel
# 31          temporally_filtered_saturated_fire_pixel
# 32 temporally_filtered_cloud_contaminated_fire_pixel
# 33   temporally_filtered_high_probability_fire_pixel
# 34 temporally_filtered_medium_probability_fire_pixel
# 35    temporally_filtered_low_probability_fire_pixel
if(!file.exists("data/out/goes16-flag-mask-meanings.csv")) {
  # Get example .nc file
  ex_aws_path <- goes_af$fullname[1]
  ex_filename <- goes_af$fullname[1]
  ex_local_path <- "data/raw/goes16-example.nc"
  
  system2(command = "aws", args = glue::glue("s3 cp s3://noaa-goes16/ABI-L2-FDCF/{ex_aws_path} {ex_local_path} --no-sign-request"))
  
  nc <- ncdf4::nc_open(ex_local_path) %>% ncdf4::ncatt_get(varid = "Mask")
  flag_vals <- nc[["flag_values"]]
  flag_meanings <- nc[["flag_meanings"]] %>% str_split(pattern = " ", simplify = TRUE) %>% as.vector()
  flag_df <- data.frame(flag_vals, flag_meanings)
  
  readr::write_csv(x = flag_df, file = "data/out/goes16-flag-mask-meanings.csv")
}

fire_flags <- 
  readr::read_csv(file = "data/out/goes16-flag-mask-meanings.csv") %>% 
  dplyr::filter(stringr::str_detect(flag_meanings, pattern = "_fire_pixel")) %>% 
  dplyr::filter(stringr::str_detect(flag_meanings, pattern = "no_fire_pixel", negate = TRUE)) %>% 
  dplyr::pull(flag_vals)

# Function that reads each GOES-16 image using the {terra} package, subsets
# out the active fire data, and writes it to a .csv
get_goes_points <- function(this_batch) {
  
  for (k in 1:nrow(this_batch)) {
    # Read in the .nc file using the {terra} package in order to preserve CRS data and values properly (and its fast!)
    goes <- terra::rast(glue::glue("data/raw/goes16/{this_batch$fullname[k]}"))
    scan_center <- this_batch$scan_center[k]
    rounded_datetime <- this_batch$rounded_datetime[k]
    rounded_datetime_txt <- this_batch$rounded_datetime_txt[k]
    filename <- this_batch$filename[k]
    
    # For joining to our curvilinear grid later, if we wish, we also want to add a
    # new raster layer with the cellindex values
    cellindex <- goes["DQF"]
    cellindex <- terra::setValues(x = cellindex, values = terra::cells(cellindex)) %>% stats::setNames("cellindex")
    
    # Stack the original .nc file with the `cellindex` raster layer
    goes <- c(goes, cellindex)
    
    # Get the crs of the .nc file; This will be important later because the satellite moved in 2017
    # from its initial testing position to its operational position, and so the raster cells
    # are representing different areas on the Earth when that happened (encoded in the CRS though)
    goes_crs <- terra::crs(goes)
    
    goes_modis_sinu <-
      goes %>%
      terra::as.data.frame(xy = TRUE) %>%
      dplyr::filter(Mask %in% fire_flags) %>%
      sf::st_as_sf(coords = c("x", "y"), crs = goes_crs, remove = FALSE) %>%
      sf::st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs") %>%
      dplyr::mutate(scan_center = scan_center) %>%
      dplyr::mutate(rounded_datetime = rounded_datetime) %>%
      dplyr::select(scan_center, rounded_datetime, cellindex, x, y, dplyr::everything()) %>%
      dplyr::mutate(sinu_x = sf::st_coordinates(.)[, 1],
                    sinu_y = sf::st_coordinates(.)[, 2]) %>%
      sf::st_drop_geometry()
    
    readr::write_csv(x = goes_modis_sinu, file = glue::glue("data/out/goes16/{rounded_datetime_txt}_{scan_center}_{filename}.csv"))
    
    # unlink(this_batch$fullname[k])
    # unlink(glue::glue("data/out/goes16/{rounded_datetime_txt}_{scan_center}_{filename}.csv"))
    # 
    # rm(goes)
    # rm(goes_modis_sinu)
    # rm(cellindex)
  }
  
  system2(command = "aws", args = glue::glue("s3 sync data/out/goes16/ s3://earthlab-mkoontz/goes16/"))
  
  return(NULL)
}

# Get the file names of the data that have already been processed
processed_goes <- 
  tibble::tibble(aws_files_raw = system2(command = "aws", args = glue::glue("s3 ls s3://earthlab-mkoontz/goes16/ --recursive"), stdout = TRUE)) %>% 
  dplyr::filter(nchar(aws_files_raw) == 139) %>% 
  dplyr::mutate(filename_full = stringr::str_sub(string = aws_files_raw, start = 39),
                filename = stringr::str_sub(string = filename_full, start = 29, end = -5))

# going for a parallelized parallelization approach
# Divide data into 4 separate batches, work on a different EC2 instance for each
# Parallelize on each of the EC2 instances

# number of cores on machine
n_cores <- 4
# precision of the progress bar
pb_precision <- 100

base::set.seed(1959)
# Only need to process the GOES file if processed data don't yet exist
batches <- 
  goes_af %>% 
  dplyr::mutate(filebase = stringr::str_sub(string = filename, start = 1, end = -4)) %>% 
  dplyr::filter(!(filebase %in% processed_goes$filename)) %>% 
  dplyr::filter(!(filebase %in% c("OR_ABI-L2-FDCF-M3_G16_s20172632115407_e20172632126173_c20172632126283",
                                  "OR_ABI-L2-FDCF-M3_G16_s20181231215382_e20181231226149_c20181231226258",
                                  "OR_ABI-L2-FDCF-M3_G16_s20183241845341_e20183241856108_c20183241856213",
                                  "OR_ABI-L2-FDCF-M6_G16_s20202471650186_e20202471659494_c20202471700318"))) %>% 
  dplyr::mutate(filename = stringr::str_sub(filename, start = 1, end = -4),
                local_path = glue::glue("data/raw/goes16/{scan_center}_{filename}.nc")) %>% 
  # lubridate::ymd_hms() is failing me here for e.g., "2020052200050.9"
  dplyr::mutate(rounded_datetime = lubridate::round_date(lubridate::parse_date_time2(scan_center, orders = "%Y%m%d%H%M%S"), unit = "hour"),
                rounded_datetime_txt = paste0(lubridate::year(rounded_datetime),
                                              stringr::str_pad(lubridate::month(rounded_datetime), width = 2, side = "left", pad = "0"),
                                              stringr::str_pad(lubridate::day(rounded_datetime), width = 2, side = "left", pad = "0"),
                                              stringr::str_pad(lubridate::hour(rounded_datetime), width = 2, side = "left", pad = "0"),
                                              "00")) %>% 
  base::split(f = sample(1:(pb_precision), size = nrow(.), replace = TRUE))

# parallelize
(start <- Sys.time())
pblapply(X = batches, FUN = get_goes_points)
(difftime(Sys.time(), start))
