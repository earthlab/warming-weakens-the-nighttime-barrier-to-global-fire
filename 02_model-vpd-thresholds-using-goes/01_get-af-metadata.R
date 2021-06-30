library(tidyverse)
library(glue)
library(lubridate)
library(pbapply)

dir.create("data/raw/", recursive = TRUE, showWarnings = FALSE)
dir.create("data/out/", showWarnings = FALSE, recursive = TRUE)

get_latest_goes <- TRUE

# Sync all GOES-16 data between 2017 and 2020
# Took about 12 hours on a m5.4xlarge EC2 instance
# 156,159 total files, about 475GB
# Instead, seems to work better to sync by year, each on a different EC2 instance

# 2017 includes 20,371 files (126 of which are actually labeled as 2000; 
# Additionally, 1 isn't readable-- see dplyr::filter() statement in next script 
# for excluding it)
system2(command = "aws", args = "s3 sync s3://noaa-goes16/ABI-L2-FDCF/2017/  data/raw/goes16/2017/", stdout = TRUE)

# 2018 includes 35,019 files (2 of which aren't readable; see dplyr::filter()
# statement in next script for excluding)
# system2(command = "aws", args = "s3 sync s3://noaa-goes16/ABI-L2-FDCF/2018/  data/raw/goes16/2018/", stdout = TRUE)

# 2019 includes XXXXX files
# system2(command = "aws", args = "s3 sync s3://noaa-goes16/ABI-L2-FDCF/2019/  data/raw/goes16/2019/", stdout = TRUE)

# 2020 includes XXXXXX files
# system2(command = "aws", args = "s3 sync s3://noaa-goes16/ABI-L2-FDCF/2020/  data/raw/goes16/2020/", stdout = TRUE)

if(get_latest_goes | !file.exists("data/out/goes16-filenames.csv")) {
  # GOES-16 record begins on 2017-05-24
  # List all the GOES-16 files that have synced to local machine from AWS
  # goes_aws_files <- 
  #   system2("aws", "s3 ls noaa-goes16/ABI-L2-FDCF/ --recursive --no-sign-request", stdout = TRUE)
  goes16_files <- 
    list.files("data/raw/goes16", recursive = TRUE)
  
  # bundle the list of filenames and extract some attributes from the
  # metadata embedded in those filenames
  goes_af <-
    tibble::tibble(fullname = goes16_files) %>% 
    tidyr::separate(col = fullname, into = c("year", "doy", "hour", "filename"), sep = "/", remove = FALSE) %>% 
    dplyr::mutate(doy = as.numeric(doy), year = as.numeric(year)) %>% 
    dplyr::mutate(tmp_date = as.Date(doy - 1, origin = glue::glue("{year}-01-01")), # Note that R uses 0-indexing for dates from an origin
                  month = lubridate::month(tmp_date),
                  day = lubridate::day(tmp_date)) %>% 
    dplyr::mutate(scan_start = stringr::str_sub(filename, start = 24, end = 37),
                  scan_end = stringr::str_sub(filename, start = 40, end = 53),
                  scan_start_year = as.numeric(stringr::str_sub(scan_start, start = 1, end = 4)),
                  scan_end_year = as.numeric(stringr::str_sub(scan_end, start = 1, end = 4)),
                  scan_start_doy = as.numeric(stringr::str_sub(scan_start, start = 5, end = 7)),
                  scan_end_doy = as.numeric(stringr::str_sub(scan_end, start = 5, end = 7)),
                  scan_start_hour = as.numeric(stringr::str_sub(scan_start, start = 8, end = 9)),
                  scan_end_hour = as.numeric(stringr::str_sub(scan_end, start = 8, end = 9)),
                  scan_start_min = as.numeric(stringr::str_sub(scan_start, start = 10, end = 11)),
                  scan_end_min = as.numeric(stringr::str_sub(scan_end, start = 10, end = 11)),
                  scan_start_sec = as.numeric(stringr::str_sub(scan_start, start = 12, end = 14)) / 10,
                  scan_end_sec = as.numeric(stringr::str_sub(scan_end, start = 12, end = 14)) / 10,
                  scan_start_date = as.character(as.Date(scan_start_doy - 1, origin = glue::glue("{scan_start_year}-01-01"))),
                  scan_end_date = as.character(as.Date(scan_end_doy - 1, origin = glue::glue("{scan_end_year}-01-01"))),
                  scan_start_full = lubridate::ymd_hms(glue::glue("{scan_start_date} {scan_start_hour}:{scan_start_min}:{scan_start_sec}")),
                  scan_end_full = lubridate::ymd_hms(glue::glue("{scan_end_date} {scan_end_hour}:{scan_end_min}:{scan_end_sec}")),
                  scan_center_full = scan_start_full + difftime(scan_end_full, scan_start_full) / 2) %>% # The midpoint between scan start and scan end; we'll use this to define a single scan time
    dplyr::mutate(year = lubridate::year(scan_center_full),
                  month = lubridate::month(scan_center_full),
                  day = lubridate::day(scan_center_full),
                  hour = lubridate::hour(scan_center_full),
                  min = lubridate::minute(scan_center_full),
                  sec = round(lubridate::second(scan_center_full), digits = 0), # round to nearest second to keep the same number of characters for each entry
                  scan_center = paste0(year,
                                       stringr::str_pad(string = month, width = 2, side = 'left', pad = '0'),
                                       stringr::str_pad(string = day, width = 2, side = 'left', pad = '0'),
                                       stringr::str_pad(string = hour, width = 2, side = 'left', pad = '0'),
                                       stringr::str_pad(string = min, width = 2, side = 'left', pad = '0'),
                                       stringr::str_pad(string = sec, width = 2, side = 'left', pad = '0'))) %>% 
    dplyr::select(year, month, day, hour, min, sec, doy, filename, scan_start_full, scan_end_full, scan_center_full, scan_start, scan_end, scan_center, fullname)
  
  readr::write_csv(x = goes_af, file = "data/out/goes16-filenames.csv")
}