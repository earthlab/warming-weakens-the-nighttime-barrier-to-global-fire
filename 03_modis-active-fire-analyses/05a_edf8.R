# script for making edf8


# setup ==========

library(sf)
library(cowplot)
library(raster)
library(tidyverse)
library(lubridate)
library(stars)
library(mblm)
library(terra)
library(doParallel)
library(foreach)
library(mgcv)
library(ggthemes)
library(viridis)
library(ggpubr)
library(broom)

posneg_cols <- c("Positive"="#CF6630", "Negative"="#07484D") # colours.cafe palette 582
mask_col<- #DDCEBF
  daynight_cols <- c("#B2182B","#2166AC") # red is #B2182B
dir.create("out")

# thresholds ===================================================================
thresholds <- read_csv("data/out/zero-goes-af-vpd-thresholds-with-landcover-codes.csv")
burnable_lcs<- pull(thresholds, lc_name)
# functions ====================================================================
parallel_theilsen <- function(stack_df, zero_to_na=FALSE, pb =TRUE,
                              minimum_sample = 10, workers = 1){
  require(doParallel)
  require(foreach)
  cells<-unique(stack_df$cell)
  
  
  registerDoParallel(workers)
  result<-foreach(i = cells, .combine = bind_rows)%dopar%{
    
    dd<-filter(stack_df, cell==i) 
    
    if(zero_to_na) dd<-dd %>% filter(value>0)
    
    if(nrow(dd)>=minimum_sample & sum(dd$value)>0){
      mod<-mblm(value~timestep, data = dd, repeated =TRUE)
      sum<-summary(mod)
      df<-tibble(cell = dd$cell[1],
                 x = dd$x[1], 
                 y = dd$y[1],
                 p = sum$coefficients[2,4],
                 beta = sum$coefficients[2,1],
                 n = nrow(dd))
      if(pb) system(paste("echo", df[1,2], "p=", round(df[1,4],2),"b=", round(df[1,5],2)))
      return(df)
    }
  }
}

parallel_theilsen_lc <- function(stack_df, zero_to_na=FALSE, pb =TRUE,
                                 minimum_sample = 10, workers = 1){
  require(doParallel)
  require(foreach)
  cells<-unique(stack_df$cell)
  
  
  registerDoParallel(workers)
  result<-foreach(i = cells, .combine = bind_rows)%dopar%{
    
    dd<-filter(stack_df, cell==i) %>%
      replace_na(list(value = 0))
    
    if(zero_to_na) dd<-dd %>% filter(value>0)
    
    if(nrow(dd)>=minimum_sample & sum(dd$value)>0){
      mod<-mblm(value~timestep, data = dd, repeated =TRUE)
      sum<-summary(mod)
      df<-tibble(cell = dd$cell[1],
                 p = sum$coefficients[2,4],
                 beta = sum$coefficients[2,1],
                 n = nrow(dd))
      if(pb) system(paste("echo", df[1,1], "p=", round(df[1,2],2),"b=", round(df[1,3],2)))
      return(df)
    }
  }
}



# ANNUAL ts trends =============================================================

wide_df<-read_csv("data/out/mcd14ml-global-trend-by-year_wide_croplands-excluded.csv") %>%
  mutate(percent_n_night = prop_n_night*100)
long_df<-read_csv("data/out/mcd14ml-global-trend-by-year_croplands-excluded.csv") 


day_afd_ts <- long_df %>%
  filter(dn_detect == "day")%>%
  mblm(n_per_op_per_Mkm2~acq_year, data=.);summary(day_afd_ts)


night_afd_ts<-long_df %>%
  filter(dn_detect == "night")%>%
  mblm(n_per_op_per_Mkm2~acq_year, data=.);summary(night_afd_ts)

nf_ts <-mblm(percent_n_night~acq_year, data=wide_df);summary(nf_ts)

frp_ts <- long_df %>%
  filter(dn_detect == "night") %>%
  mblm(mean_frp_per_detection~acq_year, data=.);summary(frp_ts)

day_frp_ts <- long_df %>%
  filter(dn_detect == "day") %>%
  mblm(mean_frp_per_detection~acq_year, data=.);summary(day_frp_ts)

preds_afd <- predict(day_afd_ts, interval = "confidence") %>%
  as.data.frame() %>%
  mutate(dn_detect = "day",
         acq_year = 2003:2020) %>%
  bind_rows(predict(night_afd_ts,  interval = "confidence") %>%
              as.data.frame() %>%
              mutate(dn_detect = "night",
                     acq_year= 2003:2020))

preds_nf <- predict(nf_ts, interval = "confidence") %>%
  as.data.frame() %>%
  mutate(acq_year = 2003:2020)

preds_frp <- predict(day_frp_ts, interval = "confidence") %>%
  as.data.frame() %>%
  mutate(dn_detect = "day",
         acq_year = 2003:2020) %>%
  rbind(.,predict(frp_ts,  interval = "confidence") %>%
          as.data.frame() %>%
          mutate(dn_detect = "night",
                 acq_year = 2003:2020)) 

p_afd <- ggplot(long_df, aes(x = acq_year, y = n_per_op_per_Mkm2, color = dn_detect)) +
  geom_line(alpha=0.75) +
  geom_line(data = preds_afd, aes(y=fit), lwd=1)+
  geom_line(data = preds_afd, aes(y=upr), lty=2)+
  geom_line(data = preds_afd, aes(y=lwr), lty=2)+
  scale_color_manual(values=daynight_cols)+
  theme_clean() +
  facet_wrap(~dn_detect, scales = "free")+
  ggtitle("Active Fire Detections per Overpass per Mkm2")+
  theme(text = element_text(family="DejaVuSans"),
        legend.position = "none",
        axis.title = element_blank())

p_np <- ggplot(wide_df, aes(x=acq_year, y=percent_n_night)) +
  geom_line(alpha=0.75) +
  geom_line(data = preds_nf, aes(y=fit), lwd=1)+
  geom_line(data = preds_nf, aes(y=upr), lty=2)+
  geom_line(data = preds_nf, aes(y=lwr), lty=2)+
  theme_clean() +
  ggtitle("Percent Nighttime Detections") +
  theme(axis.title = element_blank(),
        text = element_text(family="DejaVuSans"))

p_frp <- ggplot(long_df, aes(x = acq_year, y = mean_frp_per_detection, color = dn_detect)) +
  geom_line(alpha=0.75) +
  geom_line(data = preds_frp, aes(y=fit), lwd=1)+
  geom_line(data = preds_frp, aes(y=upr), lty=2)+
  geom_line(data = preds_frp, aes(y=lwr), lty=2)+
  scale_color_manual(values=daynight_cols)+
  facet_wrap(~dn_detect, scales = "free")+
  theme_clean() +
  ggtitle("Fire Radiative Power per Detection (MW)")+
  xlab("Date (Monthly Increments)")+
  theme(text = element_text(family="DejaVuSans"),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.title = element_blank(),
        axis.title.y = element_blank())

p_3pan_yearly<-ggarrange(p_np,p_afd,  p_frp, nrow=3, labels = c("a", "",""))
ggsave(p_3pan_yearly,filename = "figs/EDF8a_global_trends_line_plots_year_croplands-excluded.png",
       height =12, width = 6)
# MONTHLY GLOBAL ===============================================================
wide_df<-read_csv("data/out/mcd14ml-global-trend-by-month_wide_croplands-excluded.csv") %>%
  mutate(percent_n_night = prop_n_night*100)%>%
  dplyr::mutate(time = as.numeric(difftime(time1 = year_month, time2 = min(year_month), units = "days")))

long_df <- read_csv("data/out/mcd14ml-global-trend-by-month_croplands-excluded.csv") 


day_afd_ts <- long_df %>%
  filter(dn_detect == "day")%>%
  dplyr::mutate(time = as.numeric(difftime(time1 = year_month, 
                                           time2 = min(year_month), 
                                           units = "days"))) %>%
  mblm(n_per_op_per_Mkm2~time, data=.);summary(day_afd_ts)


night_afd_ts<-long_df %>%
  filter(dn_detect == "night")%>%
  dplyr::mutate(time = as.numeric(difftime(time1 = year_month, 
                                           time2 = min(year_month), 
                                           units = "days"))) %>%
  mblm(n_per_op_per_Mkm2~time, data=.);summary(night_afd_ts)

nf_ts <-mblm(percent_n_night~time, data=wide_df);summary(nf_ts)

frp_ts <- long_df %>%
  filter(dn_detect == "night") %>%
  dplyr::mutate(time = as.numeric(difftime(time1 = year_month, 
                                           time2 = min(year_month), 
                                           units = "days"))) %>%
  mblm(mean_frp_per_detection~time, data=.);summary(frp_ts)

day_frp_ts <- long_df %>%
  filter(dn_detect == "day") %>%
  dplyr::mutate(time = as.numeric(difftime(time1 = year_month, 
                                           time2 = min(year_month), 
                                           units = "days"))) %>%
  mblm(mean_frp_per_detection~time, data=.);summary(day_frp_ts)


global_row<-bind_rows(tidy(day_afd_ts)%>% mutate(variable = "day_afd_per_op_per_Mkm2"), 
                      tidy(night_afd_ts)%>% mutate(variable = "night_afd_per_op_per_Mkm2"), 
                      tidy(nf_ts)%>% mutate(variable = "percent_afd_night"), 
                      tidy(frp_ts)%>% mutate(variable = "mean_frp_per_night_detection"),
                      tidy(day_frp_ts)%>% mutate(variable = "mean_frp_per_day_detection")) %>%
  filter(term == "time")%>%
  mutate(sig = ifelse(p.value<0.05, "*", ""),
         bs = paste(round(estimate*365.25,2),sig))  %>%
  pivot_wider(id_cols = term, names_from = "variable", values_from = "bs") %>%
  dplyr::rename(cell=term)

global_row %>%
  write_csv("out/global_trends_pretty_year_croplands-excluded.csv")

global_row_simple <- bind_rows(tidy(day_afd_ts)%>% mutate(variable = "day_afd_per_op_per_Mkm2"), 
                               tidy(night_afd_ts)%>% mutate(variable = "night_afd_per_op_per_Mkm2"), 
                               tidy(nf_ts)%>% mutate(variable = "percent_afd_night"), 
                               tidy(frp_ts)%>% mutate(variable = "mean_frp_per_night_detection"),
                               tidy(day_frp_ts)%>% mutate(variable = "mean_frp_per_day_detection")) %>%
  filter(term == "time")%>%
  mutate(sig = ifelse(p.value<0.05, "*", ""),
         sign = ifelse(estimate>0, "+", "-"),
         sign_sig = ifelse(p.value<0.05, sign, sig))  %>%
  dplyr::select(-sign, -sig,-p.value, -estimate) %>%
  pivot_wider(id_cols = term, names_from = "variable", values_from = "sign_sig") %>%
  dplyr::rename(cell=term)

global_row_simple %>%
  write_csv("out/global_trends_simple_year_croplands-excluded.csv")

# global predictions
global_preds <- tibble("day_afd_per_ovp"=predict(day_afd_ts), 
                       "night_afd_per_ovp"=   predict(night_afd_ts), 
                       "percent_n_afd"=  predict(nf_ts), 
                       "night_frp_MW_per_afd"= predict(frp_ts)) %>%
  slice(c(1,216)) %>%
  mutate(date = c("January 2003", "December 2020"))

write_csv(global_preds, "out/global_prediction_bookends_croplands-excluded.csv")
xxxx<-predict(nf_ts, interval = "confidence")
(xxxx[216,1]-xxxx[12,1])/18

# MONTHLY by Koppen ============================================================

lut_kop<- c("Equatorial", "Arid", "Temperate", "Boreal", "Polar")
names(lut_kop) <- c(1,2,3,4,5)

# ts =====
bind_rows(
  koppen_percent_n_night <- read_csv("data/out/mcd14ml-trend-by-month-koppen_wide_croplands-excluded.csv") %>%
    mutate(value = prop_n_night*100)%>%
    dplyr::mutate(timestep = as.numeric(difftime(time1 = year_month, 
                                                 time2 = min(year_month), 
                                                 units = "days")),
                  cell = lut_kop[koppen])%>%
    parallel_theilsen_lc() %>%
    mutate(variable = "percent_afd_night")
  ,
  koppen_day_afd <- read_csv("data/out/mcd14ml-trend-by-month-koppen_croplands-excluded.csv")%>%
    filter(dn_detect == "day")%>%
    dplyr::mutate(timestep = as.numeric(difftime(time1 = year_month, 
                                                 time2 = min(year_month), 
                                                 units = "days")),
                  cell = lut_kop[koppen]) %>%
    dplyr::rename(value = n_per_op_per_Mkm2)%>%
    parallel_theilsen_lc() %>%
    mutate(variable = "day_afd_per_op_per_Mkm2")
  ,
  koppen_night_afd <- read_csv("data/out/mcd14ml-trend-by-month-koppen_croplands-excluded.csv")%>%
    filter(dn_detect == "night")%>%
    dplyr::mutate(timestep = as.numeric(difftime(time1 = year_month, 
                                                 time2 = min(year_month), 
                                                 units = "days")),
                  cell = lut_kop[koppen]) %>%
    dplyr::rename(value = n_per_op_per_Mkm2)%>%
    parallel_theilsen_lc() %>%
    mutate(variable = "night_afd_per_op_per_Mkm2")
  ,
  koppen_night_frp <- read_csv("data/out/mcd14ml-trend-by-month-koppen_croplands-excluded.csv")%>%
    filter(dn_detect == "night")%>%
    dplyr::mutate(timestep = as.numeric(difftime(time1 = year_month, 
                                                 time2 = min(year_month), 
                                                 units = "days")),
                  cell = lut_kop[koppen]) %>%
    dplyr::rename(value = mean_frp_per_detection)%>%
    parallel_theilsen_lc() %>%
    mutate(variable = "mean_frp_per_night_detection")
  ,
  koppen_day_frp <- read_csv("data/out/mcd14ml-trend-by-month-koppen_croplands-excluded.csv")%>%
    filter(dn_detect == "day")%>%
    dplyr::mutate(timestep = as.numeric(difftime(time1 = year_month, 
                                                 time2 = min(year_month), 
                                                 units = "days")),
                  cell = lut_kop[koppen]) %>%
    dplyr::rename(value = mean_frp_per_detection)%>%
    parallel_theilsen_lc() %>%
    mutate(variable = "mean_frp_per_day_detection")
)-> koppen_trends

write_csv(koppen_trends, "out/koppen_trends_raw_croplands-excluded.csv")

koppen_trends %>%
  dplyr::select(-n) %>%
  mutate(sig = ifelse(p<0.05, "*", ""),
         sign = ifelse(beta>0, "+", "-"),
         sign_sig = ifelse(p<0.05, sign, sig))  %>%
  dplyr::select(-sign, -sig,-p, -beta) %>%
  pivot_wider(id_cols = cell, names_from = "variable", values_from = "sign_sig") %>%
  bind_rows(global_row_simple)%>%
  mutate(cell=replace(cell, cell=="time", "Global")) %>%
  dplyr::rename(Koppen = cell) %>%
  write_csv("out/koppen_trends_pretty_croplands-excluded.csv")

koppen_trends %>%
  dplyr::select(-n) %>%
  mutate(sig = ifelse(p<0.05, "*", ""),
         bs = paste(round(beta*365.25,2),sig))  %>%
  pivot_wider(id_cols = cell, names_from = "variable", values_from = "bs") %>%
  bind_rows(global_row)%>%
  mutate(cell=replace(cell, cell=="time", "Global")) %>%
  dplyr::rename(Koppen = cell) %>%
  write_csv("out/koppen_trends_pretty_year_croplands-excluded.csv")

koppen_trends %>%
  dplyr::select(-n) %>%
  mutate(sig = ifelse(p<0.05, "*", ""),
         bs = paste(round(beta,5),sig))  %>%
  pivot_wider(id_cols = cell, names_from = "variable", values_from = "bs") %>%
  write_csv("out/koppen_trends_pretty_day_croplands-excluded.csv")


# MONTHLY global line plots ====================================================

wide_df<-read_csv("data/out/mcd14ml-global-trend-by-month_wide_croplands-excluded.csv") %>%
  mutate(percent_n_night = prop_n_night*100)%>%
  dplyr::mutate(time = as.numeric(difftime(time1 = year_month, time2 = min(year_month), units = "days")))

long_df <- read_csv("data/out/mcd14ml-global-trend-by-month_croplands-excluded.csv") 


preds_afd <- predict(day_afd_ts, interval = "confidence") %>%
  as.data.frame() %>%
  mutate(dn_detect = "day") %>%
  rbind(.,predict(night_afd_ts,  interval = "confidence") %>%
          as.data.frame() %>%
          mutate(dn_detect = "night")
  ) %>%
  mutate(year_month = pull(long_df%>% arrange(dn_detect), year_month))

preds_nf <- predict(nf_ts, interval = "confidence") %>%
  as.data.frame() %>%
  mutate(year_month = pull(wide_df, year_month))

preds_frp <- predict(day_frp_ts, interval = "confidence") %>%
  as.data.frame() %>%
  mutate(dn_detect = "day") %>%
  rbind(.,predict(frp_ts,  interval = "confidence") %>%
          as.data.frame() %>%
          mutate(dn_detect = "night")
  ) %>%
  mutate(year_month = pull(long_df%>% arrange(dn_detect), year_month))

p_afd <- ggplot(long_df, aes(x = year_month, y = n_per_op_per_Mkm2, color = dn_detect)) +
  geom_line(alpha=0.75) +
  geom_line(data = preds_afd, aes(y=fit), lwd=1)+
  geom_line(data = preds_afd, aes(y=upr), lty=2)+
  geom_line(data = preds_afd, aes(y=lwr), lty=2)+
  scale_color_manual(values=daynight_cols)+
  theme_clean() +
  facet_wrap(~dn_detect, scales = "free")+
  ggtitle("Active Fire Detections per Overpass per Mkm2")+
  theme(text = element_text(family="DejaVuSans"),
        legend.position = "none",
        axis.title = element_blank())

p_np <- ggplot(wide_df, aes(x=year_month, y=percent_n_night)) +
  geom_line(alpha=0.75) +
  geom_line(data = preds_nf, aes(y=fit), lwd=1)+
  geom_line(data = preds_nf, aes(y=upr), lty=2)+
  geom_line(data = preds_nf, aes(y=lwr), lty=2)+
  theme_clean() +
  ggtitle("Percent Nighttime Detections") +
  theme(axis.title = element_blank(),
        text = element_text(family="DejaVuSans"))

p_frp <- ggplot(long_df, aes(x = year_month, y = mean_frp_per_detection, color = dn_detect)) +
  geom_line(alpha=0.75) +
  geom_line(data = preds_frp, aes(y=fit), lwd=1)+
  geom_line(data = preds_frp, aes(y=upr), lty=2)+
  geom_line(data = preds_frp, aes(y=lwr), lty=2)+
  scale_color_manual(values=daynight_cols)+
  theme_clean() +
  facet_wrap(~dn_detect, scales = "free")+
  ggtitle("Fire Radiative Power per Detection (MW)")+
  xlab("Date (Monthly Increments)")+
  theme(text = element_text(family="DejaVuSans"),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.title = element_blank(),
        axis.title.y = element_blank())

ggarrange( p_np,p_afd, p_frp, nrow=3, labels = c("a", "","")) +
  ggsave(filename = "figs/EDF8a_global_trends_line_plots_croplands-excluded.png",
         height =12, width = 6)

# MONTHLY koppen line plots ====================================================


wide_df_k<-read_csv("data/out/mcd14ml-trend-by-month-koppen_wide_croplands-excluded.csv") %>%
  mutate(percent_n_night = prop_n_night*100)%>%
  dplyr::mutate(time = as.numeric(difftime(time1 = year_month, time2 = min(year_month), units = "days"))) %>%
  mutate(koppen = lut_kop[koppen] %>% factor(levels = c("Equatorial", "Arid", "Temperate", "Boreal")))

long_df_k <- read_csv("data/out/mcd14ml-trend-by-month-koppen_croplands-excluded.csv")  %>%
  mutate(koppen = lut_kop[koppen] %>% factor(levels = c("Equatorial", "Arid", "Temperate", "Boreal")))

p_k_afd <- ggplot(long_df_k, aes(x = year_month, y = n_per_op_per_Mkm2, color = dn_detect)) +
  geom_line(alpha=0.75) +
  geom_smooth(method="lm")+
  scale_color_manual(values=daynight_cols)+
  facet_wrap(~koppen, nrow=1, ncol=4, scales = "free")+
  theme_clean() +
  ggtitle("Active Fire Detections per Overpass per Mkm2")+
  theme(text = element_text(family="DejaVuSans"),
        legend.position = "none",
        axis.title = element_blank())

p_k_np <- ggplot(wide_df_k, aes(x=year_month, y=percent_n_night)) +
  geom_line(alpha=0.75) +
  theme_clean() +
  facet_wrap(~koppen, nrow=1, ncol=4, scales = "free")+
  geom_smooth(method="lm")+
  ggtitle("Percent of Detections that Occurred at Night") +
  theme(text = element_text(family="DejaVuSans"),
        axis.title = element_blank())

p_k_frp <- ggplot(long_df_k, aes(x = year_month, y = mean_frp_per_detection, color = dn_detect)) +
  geom_line(alpha=0.75) + 
  facet_wrap(~koppen, nrow=1, ncol=4, scales = "free")+
  geom_smooth(method="lm")+
  scale_color_manual(values=daynight_cols)+
  theme_clean() +
  ggtitle("Fire Radiative Power per Detection (MW)")+
  xlab("Date (Monthly Increments)")+
  theme(text = element_text(family="DejaVuSans"),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.title = element_blank(),
        axis.title.y = element_blank())

ggarrange(p_k_afd, p_k_np, p_k_frp, nrow=3, labels = c("b", "", "")) +
  ggsave(filename = "figs/EDF8b_global_trends_by_koppen_line_plots_croplands-excluded.png",
         height =12, width =12)

# time series decomposition GLOBAL, only seasonal removed ======================
library(forecast)
# devtools:: install_github("brisneve/ggplottimeseries")
library(ggplottimeseries)

wide_df<-read_csv("data/out/mcd14ml-global-trend-by-month_wide_croplands-excluded.csv") %>%
  mutate(percent_n_night = prop_n_night*100)%>%
  dplyr::mutate(time = as.numeric(difftime(time1 = year_month, time2 = min(year_month), units = "days")))

long_df <- read_csv("data/out/mcd14ml-global-trend-by-month_croplands-excluded.csv") 

p_nf_sr <- dts1(pull(wide_df, year_month),
                pull(wide_df, percent_n_night),
                12, type = "additive") %>%
  mutate(seasonal_removed = observation-seasonal,
         timestep = wide_df$time)

ts_trend_nf <- mblm(seasonal_removed ~ timestep, p_nf_sr%>% na.omit(), repeated = TRUE); summary(ts_trend_nf)


p_nf<-ggplot(p_nf_sr %>% na.omit(),aes(x=date, y=seasonal_removed)) +
  geom_line(alpha=0.5) +
  geom_line(aes(y=predict(ts_trend_nf)), lwd=1) +
  geom_line(aes(y=predict(ts_trend_nf, interval = "confidence")[,"upr"]), lty =2) +
  geom_line(aes(y=predict(ts_trend_nf, interval = "confidence")[,"lwr"]), lty =2) +
  ylab("Percent\nNighttime Detections") +
  theme_classic() +
  theme(axis.title.x = element_blank())


df_afd_day <- dts1(pull(long_df%>% filter(dn_detect == "night"), year_month),
                   pull(long_df%>% filter(dn_detect == "night"), n_per_op_per_Mkm2),
                   12, type = "additive")%>%
  mutate(seasonal_removed = observation-seasonal,
         timestep = 1:nrow(.),
         var = "Night") 

ts_trend_day <- mblm(seasonal_removed ~ timestep, df_afd_day %>% na.omit(), repeated = TRUE); summary(ts_trend_day)


df_afd_night <- dts1(pull(long_df%>% filter(dn_detect == "day"), year_month),
                     pull(long_df%>% filter(dn_detect == "day"), n_per_op_per_Mkm2),
                     12, type = "additive") %>%
  mutate(seasonal_removed = observation-seasonal,
         timestep = 1:nrow(.),
         var = "Day") 

ts_trend_night <- mblm(seasonal_removed ~ timestep, df_afd_night %>% na.omit(), repeated = TRUE); summary(ts_trend_night)

afd_preds <- predict(ts_trend_night, interval = "confidence") %>%
  as.data.frame()%>%
  mutate(var = "Night") %>%
  bind_rows(predict(ts_trend_day, interval = "confidence") %>%
              as.data.frame()%>%
              mutate(var = "Day"))

p_afd<- df_afd_night %>%
  na.omit() %>%
  bind_rows(df_afd_day %>% na.omit()) %>%
  ggplot(aes(x=date, y=seasonal_removed, color = var)) +
  geom_line(alpha=0.5) +
  geom_line(aes(y=afd_preds$fit), lwd=1) +
  geom_line(aes(y=afd_preds$upr), lty =2) +
  geom_line(aes(y=afd_preds$lwr), lty =2) +
  facet_wrap(~var, scales = 'free')+
  ylab("Active Fire Detections\nper Overpass per MKm2") +
  theme_classic() +
  scale_color_manual(values = daynight_cols)+
  theme(legend.position ="none",
        legend.justification = c(0,0.5),
        legend.title = element_blank(),
        axis.title.x = element_blank())

df_frp_night <- dts1(pull(long_df%>% filter(dn_detect == "night"), year_month),
                     pull(long_df%>% filter(dn_detect == "night"), mean_frp_per_detection),
                     12, type = "additive") %>%
  mutate(seasonal_removed = observation-seasonal,
         timestep = 1:nrow(.),
         var = "Night") 

ts_frp_trend_night <- mblm(seasonal_removed ~ timestep, df_frp_night %>% na.omit(), repeated = TRUE); summary(ts_frp_trend_night)


df_frp_day <- dts1(pull(long_df%>% filter(dn_detect == "day"), year_month),
                   pull(long_df%>% filter(dn_detect == "day"), mean_frp_per_detection),
                   12, type = "additive")  %>%
  mutate(seasonal_removed = observation-seasonal,
         timestep = 1:nrow(.),
         var = "Day")

ts_frp_trend_day <- mblm(seasonal_removed ~ timestep, df_frp_day %>% na.omit(), repeated = TRUE); summary(ts_frp_trend_day)

frp_preds <- predict(ts_frp_trend_day, interval = "confidence") %>%
  as.data.frame()%>%
  mutate(var = "Day") %>%
  bind_rows(predict(ts_frp_trend_night, interval = "confidence") %>%
              as.data.frame()%>%
              mutate(var = "Night"))

p_frp<- bind_rows(df_frp_day%>% na.omit(), 
                  df_frp_night%>% na.omit)%>%
  ggplot(aes(x=date, y=seasonal_removed, color = var)) +
  geom_line(alpha = 0.5) +
  geom_line(aes(y=frp_preds$fit), lwd=1) +
  geom_line(aes(y=frp_preds$upr), lty =2) +
  geom_line(aes(y=frp_preds$lwr), lty =2) +
  ylab("Fire Radiative Power\nMW per Detection") +
  xlab("Date")+
  theme_classic() +
  scale_color_manual(values = daynight_cols)+
  facet_wrap(~var, scales = "free") +
  theme(legend.position = "none",
        legend.justification = c(0,0.5))


p_deco_3pan<-ggarrange( p_nf,p_afd,p_frp, nrow=3, ncol=1)
ggsave(p_deco_3pan, filename = "figs/ts_decompose_croplands-excluded.png", width = 8, height =12)

# time series decomposition GLOBAL, seasonal and random removed ======================

wide_df<-read_csv("data/out/mcd14ml-global-trend-by-month_wide_croplands-excluded.csv") %>%
  mutate(percent_n_night = prop_n_night*100)%>%
  dplyr::mutate(time = as.numeric(difftime(time1 = year_month, time2 = min(year_month), units = "days")))

long_df <- read_csv("data/out/mcd14ml-global-trend-by-month_croplands-excluded.csv") 

p_nf_sr <- dts1(pull(wide_df, year_month),
                pull(wide_df, percent_n_night),
                12, type = "additive") %>%
  mutate(timestep = wide_df$time)

ts_trend_nf <- mblm(trend ~ timestep, p_nf_sr%>% na.omit(), repeated = TRUE); summary(ts_trend_nf)


p_nf<-ggplot(p_nf_sr %>% na.omit(),aes(x=date, y=trend)) +
  geom_line(alpha=0.5) +
  geom_line(aes(y=predict(ts_trend_nf)), lwd=1) +
  geom_line(aes(y=predict(ts_trend_nf, interval = "confidence")[,"upr"]), lty =2) +
  geom_line(aes(y=predict(ts_trend_nf, interval = "confidence")[,"lwr"]), lty =2) +
  ylab("Percent\nNighttime Detections") +
  theme_classic() +
  theme(axis.title.x = element_blank())


df_afd_day <- dts1(pull(long_df%>% filter(dn_detect == "night"), year_month),
                   pull(long_df%>% filter(dn_detect == "night"), n_per_op_per_Mkm2),
                   12, type = "additive")%>%
  mutate(timestep = 1:nrow(.),
         var = "Night") 

ts_trend_day <- mblm(trend ~ timestep, df_afd_day %>% na.omit(), repeated = TRUE)

df_afd_night <- dts1(pull(long_df%>% filter(dn_detect == "day"), year_month),
                     pull(long_df%>% filter(dn_detect == "day"), n_per_op_per_Mkm2),
                     12, type = "additive") %>%
  mutate(timestep = 1:nrow(.),
         var = "Day") 

ts_trend_night <- mblm(trend ~ timestep, df_afd_night %>% na.omit(), repeated = TRUE); summary(ts_trend_night)

afd_preds <- predict(ts_trend_night, interval = "confidence") %>%
  as.data.frame()%>%
  mutate(var = "Night") %>%
  bind_rows(predict(ts_trend_day, interval = "confidence") %>%
              as.data.frame()%>%
              mutate(var = "Day"))

p_afd<- df_afd_night %>%
  na.omit() %>%
  bind_rows(df_afd_day %>% na.omit()) %>%
  ggplot(aes(x=date, y=trend, color = var)) +
  geom_line(alpha=0.5) +
  geom_line(aes(y=afd_preds$fit), lwd=1) +
  geom_line(aes(y=afd_preds$upr), lty =2) +
  geom_line(aes(y=afd_preds$lwr), lty =2) +
  facet_wrap(~var, scales = 'free')+
  ylab("Active Fire Detections\nper Overpass per MKm2") +
  theme_classic() +
  scale_color_manual(values = daynight_cols)+
  theme(legend.position ="none",
        legend.justification = c(0,0.5),
        legend.title = element_blank(),
        axis.title.x = element_blank())

df_frp_night <- dts1(pull(long_df%>% filter(dn_detect == "night"), year_month),
                     pull(long_df%>% filter(dn_detect == "night"), mean_frp_per_detection),
                     12, type = "additive") %>%
  mutate(seasonal_removed = observation-seasonal,
         timestep = 1:nrow(.),
         var = "Night") 

ts_frp_trend_night <- mblm(trend ~ timestep, df_frp_night %>% na.omit(), repeated = TRUE); summary(ts_frp_trend_night)


df_frp_day <- dts1(pull(long_df%>% filter(dn_detect == "day"), year_month),
                   pull(long_df%>% filter(dn_detect == "day"), mean_frp_per_detection),
                   12, type = "additive")  %>%
  mutate(seasonal_removed = observation-seasonal,
         timestep = 1:nrow(.),
         var = "Day")

ts_frp_trend_day <- mblm(trend ~ timestep, df_frp_day %>% na.omit(), repeated = TRUE); summary(ts_frp_trend_day)

frp_preds <- predict(ts_frp_trend_day, interval = "confidence") %>%
  as.data.frame()%>%
  mutate(var = "Day") %>%
  bind_rows(predict(ts_frp_trend_night, interval = "confidence") %>%
              as.data.frame()%>%
              mutate(var = "Night"))

p_frp<- bind_rows(df_frp_day%>% na.omit(), 
                  df_frp_night%>% na.omit)%>%
  ggplot(aes(x=date, y=trend, color = var)) +
  geom_line(alpha = 0.5) +
  geom_line(aes(y=frp_preds$fit), lwd=1) +
  geom_line(aes(y=frp_preds$upr), lty =2) +
  geom_line(aes(y=frp_preds$lwr), lty =2) +
  ylab("Fire Radiative Power\nMW per Detection") +
  xlab("Date")+
  theme_classic() +
  scale_color_manual(values = daynight_cols)+
  facet_wrap(~var, scales = "free") +
  theme(legend.position = "none",
        legend.justification = c(0,0.5))


p_deco_3pan_t<-ggarrange( p_nf,p_afd,p_frp, nrow=3, ncol=1)
ggsave(p_deco_3pan_t, filename = "figs/ts_decompose_trend_croplands-excluded.png", width = 8, height =12)






# tsdeco KOPPEN =====================
wide_df_k<-read_csv("data/out/mcd14ml-trend-by-month-koppen_wide_croplands-excluded.csv") %>%
  mutate(percent_n_night = prop_n_night*100)%>%
  dplyr::mutate(time = as.numeric(difftime(time1 = year_month, time2 = min(year_month), units = "days"))) %>%
  mutate(koppen = lut_kop[koppen] %>% factor(levels = c("Equatorial", "Arid", "Temperate", "Boreal")))

long_df_k <- read_csv("data/out/mcd14ml-trend-by-month-koppen_croplands-excluded.csv")  %>%
  mutate(koppen = lut_kop[koppen] %>% factor(levels = c("Equatorial", "Arid", "Temperate", "Boreal")))

ks<- c("Equatorial", "Arid", "Temperate", "Boreal")

# night fraction ==============
p_nfk <- list()
ts_trends_nf <- list()
for(i in 1:4){
  
  dk <- filter(wide_df_k, koppen == ks[i])
  
  p_nf_sr <- dts1(pull(dk, year_month),
                  pull(dk, percent_n_night),
                  12, type = "additive") %>%
    mutate(seasonal_removed = observation-seasonal,
           timestep = dk$time)%>% 
    na.omit()

  ts_trends_nf<- mblm(seasonal_removed ~ timestep, p_nf_sr, 
                      repeated = TRUE) %>%
    predict(interval = "confidence")
  
  
  p_nfk[[i]] <- ggplot(cbind(p_nf_sr, ts_trends_nf), aes(x=date, y=seasonal_removed)) +
    geom_line(alpha=0.5) +
    geom_line(aes(y=fit), lwd=1) +
    geom_line(aes(y=upr), lty =2) +
    geom_line(aes(y=lwr), lty =2) +
    ggtitle(ks[i])+
    theme_classic() +
    theme(axis.title.x = element_blank())
  
  if(i == 1){p_nfk[[i]] <- p_nfk[[i]] + ylab("Percent Nighttime Detections")}else{
    p_nfk[[i]] <- p_nfk[[i]] + theme(axis.title.y = element_blank())
  } 
  
}

p_nf_kop_deco <- ggarrange(plotlist = p_nfk, nrow = 1, ncol =4)

# night frp ===============
p_nfrpk <- list()
ts_trends_nf <- list()
for(i in 1:4){
  dk <- filter(long_df_k, koppen == ks[i] & dn_detect == "night")
  
  df_frp_night <- dts1(pull(dk, year_month),
                       pull(dk, mean_frp_per_detection),
                       12, type = "additive") %>%
    mutate(seasonal_removed = observation-seasonal,
           timestep = 1:nrow(.))  %>%
    na.omit()

  ts_frp_trend_night <- mblm(seasonal_removed ~ timestep, df_frp_night, repeated = TRUE) %>%
    predict(interval = "confidence")
  
  p_nfrpk[[i]] <- ggplot(cbind(df_frp_night, ts_frp_trend_night),
                         aes(x=date, y=seasonal_removed)) +
    geom_line(alpha=0.5, color = daynight_cols[2]) +
    geom_line(aes(y=fit), lwd=1, color = daynight_cols[2]) +
    geom_line(aes(y=upr), lty =2, color = daynight_cols[2]) +
    geom_line(aes(y=lwr), lty =2, color = daynight_cols[2]) +
    theme_classic() +
    theme(axis.title.x = element_blank())
  
  if(i == 1){p_nfrpk[[i]] <- p_nfrpk[[i]] + ylab("Nighttime FRP\nper detection (MW)")}else{
    p_nfrpk[[i]] <- p_nfrpk[[i]] + theme(axis.title.y = element_blank())
  }}

p_nfrp_kop_deco <- ggarrange(plotlist = p_nfrpk, nrow = 1, ncol =4)

# day frp ===============
p_dfrpk <- list()
ts <- list()
for(i in 1:4){
  dk <- filter(long_df_k, koppen == ks[i] & dn_detect == "day")
  
  df <- dts1(pull(dk, year_month),
                       pull(dk, mean_frp_per_detection),
                       12, type = "additive") %>%
    mutate(seasonal_removed = observation-seasonal,
           timestep = 1:nrow(.))  %>%
    na.omit()
  
  ts <- mblm(seasonal_removed ~ timestep, df, repeated = TRUE) %>%
    predict(interval = "confidence")
  
  p_dfrpk[[i]] <- ggplot(cbind(df, ts),
                         aes(x=date, y=seasonal_removed)) +
    geom_line(alpha=0.5, color = daynight_cols[1]) +
    geom_line(aes(y=fit), lwd=1, color = daynight_cols[1]) +
    geom_line(aes(y=upr), lty =2, color = daynight_cols[1]) +
    geom_line(aes(y=lwr), lty =2, color = daynight_cols[1]) +
    theme_classic() +
    theme(axis.title.x = element_blank())
  
  if(i == 1){p_dfrpk[[i]] <- p_dfrpk[[i]] + ylab("Daytime FRP\nper detection (MW)")}else{
    p_dfrpk[[i]] <- p_dfrpk[[i]] + theme(axis.title.y = element_blank())
  }}

p_dfrp_kop_deco <- ggarrange(plotlist = p_dfrpk, nrow = 1, ncol =4)

# night afd ===============
p_nafdk <- list()
ts <- list()
for(i in 1:4){
  dk <- filter(long_df_k, koppen == ks[i] & dn_detect == "night")
  
  df <- dts1(pull(dk, year_month),
                       pull(dk, n_per_op_per_Mkm2),
                       12, type = "additive") %>%
    mutate(seasonal_removed = observation-seasonal,
           timestep = 1:nrow(.))  %>%
    na.omit()
  
  ts <- mblm(seasonal_removed ~ timestep, df, repeated = TRUE) %>%
    predict(interval = "confidence")
  
  p_nafdk[[i]] <- ggplot(cbind(df, ts),
                         aes(x=date, y=seasonal_removed)) +
    geom_line(alpha=0.5, color = daynight_cols[2]) +
    geom_line(aes(y=fit), lwd=1, color = daynight_cols[2]) +
    geom_line(aes(y=upr), lty =2, color = daynight_cols[2]) +
    geom_line(aes(y=lwr), lty =2, color = daynight_cols[2]) +
    theme_classic() +
    theme(axis.title.x = element_blank())
  
  if(i == 1){p_nafdk[[i]] <- p_nafdk[[i]] + ylab("Nighttime Active Fire\nDetections per Mkm2")}else{
    p_nafdk[[i]] <- p_nafdk[[i]] + theme(axis.title.y = element_blank())
  }}

p_nafd_kop_deco <- ggarrange(plotlist = p_nafdk, nrow = 1, ncol =4)

# day afd ===============
p_dafdk <- list()
ts <- list()
for(i in 1:4){
  dk <- filter(long_df_k, koppen == ks[i] & dn_detect == "day")
  
  df <- dts1(pull(dk, year_month),
             pull(dk, n_per_op_per_Mkm2),
             12, type = "additive") %>%
    mutate(seasonal_removed = observation-seasonal,
           timestep = 1:nrow(.))  %>%
    na.omit()
  
  ts <- mblm(seasonal_removed ~ timestep, df, repeated = TRUE) %>%
    predict(interval = "confidence")
  
  p_dafdk[[i]] <- ggplot(cbind(df, ts),
                         aes(x=date, y=seasonal_removed)) +
    geom_line(alpha=0.5, color = daynight_cols[1]) +
    geom_line(aes(y=fit), lwd=1, color = daynight_cols[1]) +
    geom_line(aes(y=upr), lty =2, color = daynight_cols[1]) +
    geom_line(aes(y=lwr), lty =2, color = daynight_cols[1]) +
    theme_classic() +
    theme(axis.title.x = element_blank())
  
  if(i == 1){p_dafdk[[i]] <- p_dafdk[[i]] + ylab("Daytime Active Fire\nDetections per Mkm2")}else{
    p_dafdk[[i]] <- p_dafdk[[i]] + theme(axis.title.y = element_blank())
  }}

p_dafd_kop_deco <- ggarrange(plotlist = p_dafdk, nrow = 1, ncol =4)

ggsave(ggarrange(p_nf_kop_deco, p_dafd_kop_deco, 
                 p_nafd_kop_deco, p_dfrp_kop_deco,
                 p_nfrp_kop_deco, ncol=1, nrow=5),
       filename = "figs/koppen_sr_trends-croplands-excluded.png",
       width = 8, height = 10)
