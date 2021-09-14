# global frp per detection summaries 
library(terra)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyverse)
library(pbapply)
library(mblm)

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
    # year_afd <- year_afd[str_sub(lc,2,3) != "12",]
    year_afd[, `:=`(acq_month = str_pad(string = lubridate::month(acq_dttme), width = 2, side = "left", pad = "0"),
                    acq_year = as.character(lubridate::year(acq_dttme)))]
    year_afd <- year_afd[confidence >= 10 & type == 0]
    
    
    year_month_afd_gl <- year_afd[, .(q90_frp = q90(frp),
                                      median_frp = median(frp, na.rm=TRUE)), 
                                  
                               by = .(acq_month, acq_year, dn_detect)] %>%
      mutate(scale = "global",
             lc = "global")
    
    year_month_afd_lc <- year_afd[, .(q90_frp = q90(frp), 
                                      median_frp = median(frp, na.rm=TRUE)), 
                               by = .(lc, acq_month, acq_year, dn_detect)]%>% 
      mutate(scale = "landcoverXkoppen",
             lc = as.character(lc)) 
    
    year_month_afd_k <- year_afd[, .(q90_frp = q90(frp), 
                                     median_frp = median(frp, na.rm=TRUE)), 
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

data.table::fwrite(x = monthly_afd, file = "data/out/mcd14ml_q90-frp_month-lc-kop-daynight-summary_croplands-excluded.csv")

system2(command = "aws", args = "s3 cp data/out/mcd14ml_q90-frp_month-lc-kop-daynight-summary_croplands-excluded.csv s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/")

# by cell 
monthly_afd_cell <-
  pblapply(X = seq_along(years), cl = cl, FUN = function(i) {
    
    op_this_year <- grep(x = overpass_files, pattern = years[i], value = TRUE)
    
    months <- regmatches(op_this_year, 
                         regexpr(text = op_this_year,
                                 pattern = paste0("(?<=", years[i], "-)(\\d+)"),
                                 perl = TRUE))
    
    unique_months <- unique(months)
    
    year_afd <- data.table::fread(afd_files[i], colClasses = c(acq_time = "character"))
    # year_afd <- year_afd[str_sub(lc,2,3) != "12",]
    year_afd[, `:=`(acq_month = str_pad(string = lubridate::month(acq_dttme), width = 2, side = "left", pad = "0"),
                    acq_year = as.character(lubridate::year(acq_dttme)))]
    year_afd <- year_afd[confidence >= 10 & type == 0]
    
    
    year_month_afd_gl <- year_afd[, .(q90_frp = q90(frp),
                                      median_frp = median(frp, na.rm=TRUE)), 
                                  
                                  by = .(acq_month, acq_year, dn_detect, cell_id_lc, x_lc, y_lc)]

    
    return(year_month_afd_cell)
    
  })

parallel::stopCluster(cl)

# yearly agg

yearly_afd_l <-
  pblapply(X = seq_along(years), cl = cl, FUN = function(i) {
    
    months <- regmatches(op_this_year, 
                         regexpr(text = op_this_year,
                                 pattern = paste0("(?<=", years[i], "-)(\\d+)"),
                                 perl = TRUE))
    
    unique_months <- unique(months)
    
    year_afd <- data.table::fread(afd_files[i], colClasses = c(acq_time = "character"))
    year_afd <- year_afd[str_sub(lc,2,3) != "12",]
    year_afd[, `:=`(acq_month = str_pad(string = lubridate::month(acq_dttme), width = 2, side = "left", pad = "0"),
                    acq_year = as.character(lubridate::year(acq_dttme)))]
    year_afd <- year_afd[confidence >= 10 & type == 0]
    
    
    year_afd_gl <- year_afd[, .(q90_frp = q90(frp),
                                      median_frp = median(frp, na.rm=TRUE)), 
                                  
                                  by = .(acq_year, dn_detect)] %>%
      mutate(scale = "global",
             lc = "global")
    
    year_afd_lc <- year_afd[, .(q90_frp = q90(frp), 
                                      median_frp = median(frp, na.rm=TRUE)), 
                                  by = .(lc, acq_year, dn_detect)]%>% 
      mutate(scale = "landcoverXkoppen",
             lc = as.character(lc)) 
    
    year_afd_k <- year_afd[, .(q90_frp = q90(frp), 
                                     median_frp = median(frp, na.rm=TRUE)), 
                                 by = .(str_sub(lc,1,1), acq_year, dn_detect)]%>% 
      mutate(scale = "koppen") %>%
      dplyr::rename(lc = str_sub) %>%
      mutate(lc = lut_kop[lc])
    
    year_afd<- bind_rows(year_afd_gl,year_afd_lc) %>%
      bind_rows(year_afd_k)
    
    return(year_afd)
    
  })

yearly_afd <- data.table::rbindlist(yearly_afd_l, fill = TRUE) %>%
  mutate(date = as.Date(paste(acq_year, "01", "01", sep = "-")))

data.table::fwrite(x = monthly_afd, file = "data/out/mcd14ml_q90-frp_yearly-lc-kop-daynight-summary_croplands-excluded.csv")

system2(command = "aws", args = "s3 cp data/out/mcd14ml_q90-frp_yearly-lc-kop-daynight-summary_croplands-excluded.csv s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/")


# basic plots ================

ggplot(monthly_afd %>% filter(scale == "global"), 
       aes(x=date, y = median_frp, color = dn_detect)) +
  geom_line() +
  geom_smooth()

ggplot(monthly_afd %>% filter(scale == "koppen") %>%
         na.omit(),
       aes(x=date, y = q90_frp, color = dn_detect)) +
  geom_line() +
  facet_wrap(~lc) +
  geom_smooth(method = "lm")


ggplot(yearly_afd %>% filter(scale == "global"), 
       aes(x=date, y = median_frp, color = dn_detect)) +
  geom_line() +
  geom_smooth()

ggplot(yearly_afd %>% filter(scale == "koppen") %>%
         na.omit(),
       aes(x=date, y = q90_frp, color = dn_detect)) +
  geom_line() +
  facet_wrap(~lc) +
  geom_smooth(method = "lm")

# formal analysis monthly ========================
library(mblm)
library(ggplottimeseries)

trends_n <- list()
sums_n <- list()
sn <- list()
for(i in 1:4){

  resdf <- monthly_afd %>% 
    filter(scale == "koppen",
           acq_year > 2002,
           dn_detect == "night",
           lc == lut_kop[i]%>% unname) 
  
  resdt<- dts1(x = pull(resdf, date),
         y = pull(resdf, q90_frp),
         z = 12, type = "additive") %>%
    mutate(seasonal_removed = observation-seasonal)%>%
    mutate(time = as.numeric(date)) 
  
  res <- resdt %>%
    mblm(seasonal_removed~time, data=.)
  
  trends_n[[i]] <- res
  sn[[i]] <- summary(res)
  sums_n[[i]]<- tidy(res) %>%
    mutate(koppen = lut_kop[i] %>% unname(),
           dn = "night",
           estimate_year = estimate * 365.25,
           pred_03 = predict(res)[1] %>% unname, 
           pred_20 = predict(res)[216] %>% unname)

}
trends_d <- list()
sums_d <- list()
for(i in 1:4){
  resdf <- monthly_afd %>% 
    filter(scale == "koppen",
           acq_year > 2002,
           dn_detect == "day",
           lc == lut_kop[i]%>% unname) 
  
  resdt<- dts1(x = pull(resdf, date),
               y = pull(resdf, q90_frp),
               z = 12, type = "additive") %>%
    mutate(seasonal_removed = observation-seasonal)%>%
    mutate(time = as.numeric(date)) 
  
  res <- resdt %>%
    mblm(seasonal_removed~time, data=.)
  
  trends_d[[i]] <- res
  sums_d[[i]]<- tidy(res) %>%
    mutate(koppen = lut_kop[i] %>% unname(),
           dn = "day",
           estimate_year = estimate * 365.25,
           pred_03 = predict(res)[1] %>% unname, 
           pred_20 = predict(res)[216] %>% unname)
  
}
trends_g <- list()
sums_g <- list()
for(i in 1:2){
  
  resdf <- monthly_afd %>% 
    filter(scale == "global",
           acq_year > 2002,
           dn_detect ==  c("day","night")[i]) 
  
  resdt<- dts1(x = pull(resdf, date),
               y = pull(resdf, q90_frp),
               z = 12, type = "additive") %>%
    mutate(seasonal_removed = observation-seasonal)%>%
    mutate(time = as.numeric(date)) 
  
  res <- resdt %>%
    mblm(seasonal_removed~time, data=.)
  
  trends_g[[i]] <- res
  sums_g[[i]]<- tidy(res) %>%
    mutate(koppen = "global",
           dn =  c("day","night")[i],
           estimate_year = estimate * 365.25,
           pred_03 = predict(res)[1] %>% unname, 
           pred_20 = predict(res)[216] %>% unname)
  
}
frpq90_m<-bind_rows(sums_d,sums_n, sums_g) %>%
  filter(term == "time") %>%
  mutate(sig = ifelse(p.value<0.05, "*",""))

write_csv(frpq90_m, "data/out/frp_q90_global+koppen_trends_croplands_included.csv")
# formal analysis yearly ========================
library(mblm)

trends_n <- list()
sums_n <- list()
sn <- list()
for(i in 1:4){
  res <- yearly_afd %>% 
    filter(scale == "koppen",
           dn_detect == "night",
           acq_year > 2002,
           lc == lut_kop[i]%>% unname) %>%
    mutate(time = as.numeric(date)) %>%
    na.omit() %>%
    mblm(q90_frp~time, data=.)
  
  trends_n[[i]] <- res
  sn[[i]] <- summary(res)
  sums_n[[i]]<- tidy(res) %>%
    mutate(koppen = lut_kop[i] %>% unname(),
           dn = "night",
           estimate_year = estimate * 365.25)
  
}
trends_d <- list()
sums_d <- list()
for(i in 1:4){
  res <- yearly_afd %>% 
    filter(scale == "koppen",
           dn_detect == "day",
           acq_year > 2002,
           lc == lut_kop[i]%>% unname) %>%
    mutate(time = as.numeric(date)) %>%
    na.omit() %>%
    mblm(q90_frp~time, data=.)
  
  trends_d[[i]] <- res
  sums_d[[i]]<- tidy(res) %>%
    mutate(koppen = lut_kop[i] %>% unname(),
           dn = "day",
           estimate_year = estimate * 365.25)
  
}
trends_g <- list()
sums_g <- list()
for(i in 1:2){
  res <- yearly_afd %>% 
    filter(scale == "global",
           acq_year > 2002,
           dn_detect == c("day","night")[i]) %>%
    mutate(time = as.numeric(date)) %>%
    na.omit() %>%
    mblm(q90_frp~time, data=.)
  
  trends_g[[i]] <- res
  sums_g[[i]]<- tidy(res) %>%
    mutate(koppen = "global",
           dn =  c("day","night")[i],
           estimate_year = estimate * 365.25)
  
}
bind_rows(sums_d,sums_n, sums_g) %>%
  filter(term == "time")%>%
  mutate(sig = ifelse(p.value<0.05, "*", ""))


# landcover
trends_n <- list()
sums_n <- list()
sn <- list()
for(i in 1:4){
  res <- monthly_afd %>% 
    filter(scale == "koppen",
           dn_detect == "night",
           lc == lut_kop[i]%>% unname) %>%
    mutate(time = as.numeric(date)) %>%
    na.omit() %>%
    mblm(median_frp~time, data=.)
  
  trends_n[[i]] <- res
  sn[[i]] <- summary(res)
  sums_n[[i]]<- tidy(res) %>%
    mutate(koppen = lut_kop[i] %>% unname(),
           dn = "night")
  
}
trends_d <- list()
sums_d <- list()
for(i in 1:4){
  res <- monthly_afd %>% 
    filter(scale == "koppen",
           dn_detect == "day",
           lc == lut_kop[i]%>% unname) %>%
    mutate(time = as.numeric(date)) %>%
    na.omit() %>%
    mblm(median_frp~time, data=.)
  
  trends_d[[i]] <- res
  sums_d[[i]]<- tidy(res) %>%
    mutate(koppen = lut_kop[i] %>% unname(),
           dn = "day")
  
}
bind_rows(sums_d,sums_n)

ggplot(monthly_afd %>% 
         filter(scale == "landcoverXkoppen",
                str_sub(lc, 1,1) != "5",
                lc != "100", lc != "101",
                str_sub(lc,2,3)!= "15") %>%
         na.omit(),
       aes(x=date, y = q90_frp, color = dn_detect)) +
  geom_line() +
  facet_wrap(~lc, scales = "free") 

# cell by cell analysis ========================================================
library(mblm)
library(vroom)
library(tidyverse)
library(foreach)
library(doParallel)

# brute forcing it on an aws instance - lots of ram needed
all_years <- list.files("data/out/mcd14ml_analysis-ready", pattern = "csv", full.names = TRUE) %>%
  vroom()%>%
  filter(dn_detect == "night",
         confidence >= 10, 
         type == 0) %>%
  dplyr::select(acq_dttme, frp, cell_id_lc, x_lc, y_lc) %>%
  mutate(timestep = as.numeric(acq_dttme)) 

doParallel::registerDoParallel(detectCores()-1)
cells <- unique(all_years$cell_id_lc)
result <- foreach(i = cells,
                  .combine = bind_rows) %dopar% {
  d<-filter(all_years,cell_id_lc == i)
  
  mod<- mblm(frp ~ timestep, data = d) 
  s<- summary(mod)
  df<- d[1,] %>%
    dplyr::select(-acq_dttme, -frp)
  df$estimate <- s$coefficients[2,1]
  df$mad <- s$coefficients[2,2]
  df$pval <- s$coefficients[2,4]
  
  return(df)
}


save(result,file = "data/out/parallelized-trends-by-cell.csv")

system2(command = "aws", args = "s3 cp data/out/parallelized-trends-by-cell.csv s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/")
