# global frp per detection summaries 
library(terra)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyverse)
library(pbapply)

n_workers <- parallel::detectCores()

system2(command = "aws", args = "s3 sync s3://earthlab-mkoontz/MODIS-overpass-counts_0.25_analysis-ready/year-month/ data/out/modis-overpass-corrections/MODIS-overpass-counts_0.25_analysis-ready/year-month/")
system2(command = "aws",
        args = "s3 sync s3://earthlab-mkoontz/mcd14ml_analysis-ready data/out/mcd14ml_analysis-ready")

q90 <- function(x, ...) quantile(x, .9, na.rm = TRUE) %>% as.numeric()

lut_kop <-c("Equatorial", "Arid", "Temperate", "Boreal")
names(lut_kop) <-c(1,2,3,4)

# overpass corrections
overpass_files <- list.files(path = "data/out/modis-overpass-corrections/MODIS-overpass-counts_0.25_analysis-ready/year-month/", full.names = TRUE)

afd_files <- list.files(path = "data/out/mcd14ml_analysis-ready", pattern = ".csv", full.names = TRUE)

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
    
    year_month_afd_gl <- year_afd[, .(q90_frp = q90(frp)), 
                               by = .(acq_month, acq_year, dn_detect)] %>%
      mutate(scale = "global",
             lc = "global")
    
    year_month_afd_lc <- year_afd[, .(q90_frp = q90(frp)), 
                               by = .(lc, acq_month, acq_year, dn_detect)]%>% 
      mutate(scale = "landcoverXkoppen",
             lc = as.character(lc)) 
    
    year_month_afd_k <- year_afd[, .(q90_frp = q90(frp)), 
                                  by = .(str_sub(lc,1,1), acq_month, acq_year, dn_detect)]%>% 
      mutate(scale = "koppen") %>%
      dplyr::rename(lc = str_sub) %>%
      mutate(lc = lut_kop[lc])
    
    year_month_afd<- bind_rows(year_month_afd_gl,year_month_afd_lc) %>%
      bind_rows(year_month_afd_k)
    
    return(year_month_afd)
    
  })

parallel::stopCluster(cl)

monthly_afd <- data.table::rbindlist(monthly_afd_l, fill = TRUE) %>%
  mutate(date = as.Date(paste(acq_year, acq_month, "01", sep = "-")))

data.table::fwrite(x = monthly_afd, file = "data/out/mcd14ml_q90-frp_month-lc-kop-daynight-summary.csv")

system2(command = "aws", args = "s3 cp data/out/mcd14ml_q90-frp_month-lc-kop-daynight-summary.csv s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/")

# basic plots

ggplot(monthly_afd %>% filter(scale == "global"), 
       aes(x=date, y= q90_frp, color = dn_detect)) +
  geom_line() +
  geom_smooth()

ggplot(monthly_afd %>% filter(scale == "koppen") %>%
         na.omit(),
       aes(x=date, y=q90_frp, color = dn_detect)) +
  geom_line() +
  facet_wrap(~lc) +
  geom_smooth()
 
