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
                               by = .(acq_month, acq_year, dn_detect)]
    
    year_month_afd_lc <- year_afd[, .(q90_frp = q90(frp)), 
                               by = .(str_c("lc_", lc), acq_month, acq_year, dn_detect)]%>% 
      na.omit() %>%
      pivot_wider(names_from = lc, values_from = c(q90_frp))
    
    year_month_afd_k <- year_afd[, .(q90_frp = q90(frp)), 
                                  by = .(str_c("koppen_",str_sub(lc,1,1)), acq_month, acq_year, dn_detect)]%>% 
      na.omit() %>%
      pivot_wider(names_from = str_sub, values_from = c(q90_frp))
    
    year_month_afd<- left_join(year_month_afd_gl,year_month_afd_lc,
                               by=c("acq_month", "acq_year", "dn_detect")) %>%
      left_join(year_month_afd_k,
                by=c("acq_month", "acq_year", "dn_detect"))
    
    return(year_month_afd)
    
  })

parallel::stopCluster(cl)

monthly_afd <- data.table::rbindlist(monthly_afd_l, fill = TRUE)

data.table::fwrite(x = monthly_afd, file = "data/out/mcd14ml_q90-frp_month-lc-kop-daynight-summary.csv")

system2(command = "aws", args = "s3 cp data/out/mcd14ml_q90-frp_month-lc-kop-daynight-summary.csv s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/")

# basic plots

ggplot(monthly_afd, aes(x=as.Date(paste(acq_year, acq_month, "01", sep="-")), y= q90_frp, color = dn_detect)) +
  geom_line() +
  geom_smooth()

monthly_afd %>% 
  dplyr::select(acq_month, acq_year, dn_detect, `1`,`2`,`3`,`4`) %>%
  mutate(date = as.Date(paste(acq_year, acq_month, "01", sep="-"))) %>%
  pivot_longer(cols = c(`1`, `2`,`3`,`4`), names_to = "koppen", values_to = "q90_frp",
               names_repair = "minimal") %>%
  ggplot(aes(x=date, y=q90_frp)) +
  geom_line() +
  facet_wrap(~koppen) +
  geom_smooth(method = "lm")
 
