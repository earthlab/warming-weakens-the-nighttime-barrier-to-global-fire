
library(sf); library(tidyverse); library(purrr); library(data.t)

system("aws s3 sync s3://earthlab-amahood/night_fires data")

ragged_reader<- function(path){
  # read each line separately as a single very long character string
  # split the string by the comma delineations and make it a list item
  # keep all values as integers to keep the disk space requirement small
  x <-read_lines(path, progress = TRUE) %>%
    purrr::map(.x = ., .f = function(x) {
      char_list <- stringr::str_split(string = x, pattern = ",")
      char_vec <- unlist(char_list)
      num_vec <- as.integer(char_vec) # make the characters into integers for more compressed storage
      return(num_vec[!is.na(num_vec)]) # there is a trailing NA that we remove
    })
  
  max_col_nums <- purrr::map_int(x, .f = length) %>% max()
  
  # This is a key step that fills in the individual fire VPD time series with NAs when there
  # were more columns than observations
  xx <- purrr::map(x, .f = function(x) {
    col_nums <- length(x)
    NA_data_fill <- rep(NA_integer_, times = max_col_nums - col_nums)
    vec <-  c(x, NA_data_fill)
    return(vec)
  })
  
  # total number of days
  max_days <- (max_col_nums - 1) %/% 24
  
  # new column names
  datetime_combos <- as.vector(t(base::outer(X = 0:max_days, Y = 0:23, FUN = paste, sep = "_")))
  datetime_combos <- datetime_combos[1:(max_col_nums - 1)]
  new_col_names <- c("fireID", datetime_combos)
  
  d <- 
    do.call(rbind, xx) %>%
    as_tibble() %>%
    setNames(new_col_names) %>% 
    as.data.table()
  
  return(d)
}

# North America
good_nums <- st_read("data/out/fired_na_2017-.gpkg") %>%
  st_drop_geometry() %>%
  dplyr::select(id) %>%
  pull() %>%
  unique()

d <- ragged_reader("data/out/na_vpd.csv")

# equivalent of tidyr::pivot_longer() to make table go from wide to long form
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html
out <- data.table::melt(data = d, id.vars = c("fireID"), variable.name = "day_hour", value.name = "VPD_hPa")
out <- out[!is.na(VPD_hPa)]

data.table::fwrite(out, "data/out/na_vpd_long_2017.csv")

system2(command = "aws", args = "s3 cp data/out/na_vpd_long_2017.csv s3://earthlab-amahood/night_fires/na_vpd_long_2017.csv")
# memory.limit()
# memory.limit(size=56000)

# South America
good_nums <- st_read("data/out/fired_sa_2017-.gpkg") %>%
  st_drop_geometry() %>%
  dplyr::select(id) %>%
  pull() %>%
  unique()

d <- ragged_reader("data/out/sa_vpd.csv")

# equivalent of tidyr::pivot_longer() to make table go from wide to long form
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html
out <- data.table::melt(data = d, id.vars = c("fireID"), variable.name = "day_hour", value.name = "VPD_hPa")
out <- out[!is.na(VPD_hPa)]

data.table::fwrite(out, "data/out/sa_vpd_long_2017.csv")

system2(command = "aws", args = "s3 cp data/out/sa_vpd_long_2017.csv s3://earthlab-amahood/night_fires/sa_vpd_long_2017.csv")