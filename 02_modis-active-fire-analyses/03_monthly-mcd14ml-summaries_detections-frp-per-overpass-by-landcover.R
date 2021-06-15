# Summarize active fire detections

library(terra)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(pbapply)

n_workers <- 8

system2(command = "aws", args = "s3 sync s3://earthlab-mkoontz/MODIS-overpass-counts_0.25_analysis-ready/year-month/ data/out/modis-overpass-corrections/MODIS-overpass-counts_0.25_analysis-ready/year-month/")

# overpass corrections
overpass_files <- list.files(path = "data/out/modis-overpass-corrections/MODIS-overpass-counts_0.25_analysis-ready/year-month/", full.names = TRUE)

afd_files <- list.files(path = "data/out/mcd14ml_analysis-ready/", pattern = ".csv", full.names = TRUE)

years <- regmatches(afd_files, 
                    regexpr(text = afd_files, 
                            pattern = "\\d+(?=\\.\\w+$)", 
                            perl = TRUE))

# setup parallelization
if (.Platform$OS.type == "windows") {
  cl <- parallel::makeCluster(n_workers)
  parallel::clusterEvalQ(cl = cl, expr = {
    library(terra)
    library(data.table)
    library(dplyr)
    library(stringr)
    library(lubridate)
    
    # overpass corrections
    overpass_files <- list.files(path = "data/out/modis-overpass-corrections/MODIS-overpass-counts_0.25_analysis-ready/year-month/", full.names = TRUE)

    afd_files <- list.files(path = "data/out/mcd14ml_analysis-ready/", pattern = ".csv", full.names = TRUE)
    
    years <- regmatches(afd_files, 
                        regexpr(text = afd_files, 
                                pattern = "\\d+(?=\\.\\w+$)", 
                                perl = TRUE))
    
  })
} else {
  cl <- n_workers
}

monthly_afd_l <-
  pblapply(X = seq_along(years), cl = cl, FUN = function(i) {
    op_this_year <- grep(x = overpass_files, pattern = years[i], value = TRUE)
    
    months <- regmatches(op_this_year, 
                         regexpr(text = op_this_year,
                                 pattern = paste0("(?<=", years[i], "-)(\\d+)"),
                                 perl = TRUE))
    
    unique_months <- unique(months)
    
    year_afd <- data.table::fread(afd_files[i], colClasses = c(acq_time = "character"))
    year_afd[, `:=`(acq_month = str_pad(string = lubridate::month(acq_dttme), width = 2, side = "left", pad = "0"),
                    acq_year = as.character(lubridate::year(acq_dttme)))]
    year_afd <- year_afd[confidence >= 10 & type == 0]
    
    year_month_afd <- year_afd[, .(sum_frp = sum(frp), mean_frp = mean(frp), n = .N), by = .(cell_id_lc, lc, x_lc, y_lc, acq_month, acq_year, dn_detect)]
    
    year_month_afd_op_l <- vector(mode = "list", length = length(unique_months))
    
    for(j in seq_along(unique_months)) {
      op_day <- 
        op_this_year[grepl(x = op_this_year, pattern = paste0(years[i], "-", unique_months[j])) & grepl(x = op_this_year, pattern = "day")] %>% 
        terra::rast() %>% 
        terra::shift(dx = -0.25, dy = 0.25) %>% 
        setNames("op") %>% 
        as.data.frame(xy = TRUE, cells = TRUE) %>% 
        dplyr::rename(x_lc = "x", y_lc = "y") %>% 
        dplyr::mutate(acq_year = years[i], acq_month = unique_months[j]) %>% 
        dplyr::rename(cell_id_lc = "cell") %>% 
        as.data.table()
      
      op_day[, dn_detect := "day"]
      
      op_night <- 
        op_this_year[grepl(x = op_this_year, pattern = paste0(years[i], "-", unique_months[j])) & grepl(x = op_this_year, pattern = "night")] %>% 
        terra::rast() %>% 
        terra::shift(dx = -0.25, dy = 0.25) %>% 
        setNames("op") %>% 
        as.data.frame(xy = TRUE, cells = TRUE) %>% 
        dplyr::rename(x_lc = "x", y_lc = "y") %>% 
        dplyr::mutate(acq_year = years[i], acq_month = unique_months[j]) %>% 
        dplyr::rename(cell_id_lc = "cell") %>%
        as.data.table()
      
      op_night[, dn_detect := "night"]
      
      # Combine day and night overpasses for this month/year combo into one data.table
      op <- rbind(op_day, op_night)
      # op <- op_day[op_night, on = c("cell_id_lc", "x_lc", "y_lc", "acq_year", "acq_month")] 
      this_year_month_afd <- year_month_afd[acq_month == unique_months[j]]
      
      year_month_afd_op_l[[j]] <- op[this_year_month_afd, on = c("cell_id_lc", "x_lc", "y_lc", "acq_month", "acq_year", "dn_detect")]
      
    }
    
    year_month_afd_op <- data.table::rbindlist(year_month_afd_op_l)
    
    return(year_month_afd_op)
    
  })

parallel::stopCluster(cl)

monthly_afd <- data.table::rbindlist(monthly_afd_l)

data.table::fwrite(x = monthly_afd, file = "data/out/mcd14ml_n-frp_month-lc-xy-daynight-summary.csv")

system2(command = "aws", args = "s3 cp data/out/mcd14ml_n-frp_month-lc-xy-daynight-summary.csv s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/")
