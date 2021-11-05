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
library(forecast)
# devtools:: install_github("brisneve/ggplottimeseries")
library(ggplottimeseries)

posneg_cols <- c("Positive"="#CF6630", "Negative"="#07484D") # colours.cafe palette 582
mask_col<- #DDCEBF
  daynight_cols <- c("#B2182B","#2166AC") # red is #B2182B
dir.create("out")

lut_kop<- c("Equatorial", "Arid", "Temperate", "Boreal", "Polar")
names(lut_kop) <- c(1,2,3,4,5)

# thresholds ===================================================================
thresholds <- read_csv("in/zero-goes-af-vpd-thresholds-with-landcover-codes.csv")
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




# tsdeco KOPPEN =====================

wide_df<-read_csv("data/out/mcd14ml-global-trend-by-month_wide.csv") %>%
  mutate(percent_n_night = prop_n_night*100)%>%
  dplyr::mutate(time = as.numeric(difftime(time1 = year_month, time2 = min(year_month), units = "days")))

long_df <- read_csv("data/out/mcd14ml-global-trend-by-month.csv") 

wide_df_k<-read_csv("data/out/mcd14ml-trend-by-month-koppen_wide.csv") %>%
  mutate(percent_n_night = prop_n_night*100)%>%
  dplyr::mutate(time = as.numeric(difftime(time1 = year_month, time2 = min(year_month), units = "days"))) %>%
  mutate(koppen = lut_kop[koppen] %>% factor(levels = c("Equatorial", "Arid", "Temperate", "Boreal"))) %>%
  bind_rows(wide_df %>% mutate(koppen = "Global")) %>%
  mutate(koppen = as.factor(koppen)%>% 
           fct_relevel(c("Global", "Boreal", "Temperate", "Arid", "Equatorial")))

long_df_k <- read_csv("data/out/mcd14ml-trend-by-month-koppen.csv")  %>%
  mutate(koppen = lut_kop[koppen] %>% factor(levels = c("Equatorial", "Arid", "Temperate", "Boreal"))) %>%
  bind_rows(long_df %>% mutate(koppen = "Global")) %>%
  mutate(koppen = as.factor(koppen)%>% 
           fct_relevel(c("Global", "Boreal", "Temperate", "Arid", "Equatorial")))

ks<- c("Global", "Boreal", "Temperate", "Arid", "Equatorial")

ktp <- read_csv("out/koppen_trends_pretty_year_sr.csv") %>%
  mutate(Koppen = str_to_title(Koppen))

# night fraction ==============
p_nfk <- list()
ts_trends_nf <- list()
output_data_nf <- list()
for(i in 1:5){
  
  dk <- filter(wide_df_k, koppen == ks[i])
  
  p_nf_sr <- dts1(pull(dk, year_month),
                  pull(dk, percent_n_night),
                  12, type = "additive") %>%
    mutate(seasonal_removed = observation-seasonal,
           timestep = dk$time)%>% 
    na.omit()

  ts_trends_nf<- mblm(trend ~ timestep, p_nf_sr, 
                      repeated = TRUE) %>%
    predict(interval = "confidence")
  
  sig <- ktp %>%
    dplyr::filter(Koppen == ks[i]) %>%
    dplyr::select(percent_afd_night) %>%
    pull() %>%
    str_detect("\\*")
  
  output_data_nf[[i]]<- cbind(p_nf_sr, ts_trends_nf) %>%
    mutate(koppen = ks[i],
           variable = "Percent Nighttime Detections",
           sig=sig)
  
  p_nfk[[i]] <- ggplot(cbind(p_nf_sr, ts_trends_nf), aes(x=date, y=trend)) +
    geom_line(alpha=0.5) +
    ggtitle(ks[i])+
    theme_classic() +
    theme(text = element_text(family="Arial"),
          axis.title.x = element_blank())
  
  if(sig){
    p_nfk[[i]] <-p_nfk[[i]]+
    geom_line(aes(y=fit), lwd=1) +
    geom_line(aes(y=upr), lty =2) +
    geom_line(aes(y=lwr), lty =2) }
  
  if(i == 1){p_nfk[[i]] <- p_nfk[[i]] + ylab("Percent Nighttime\nDetections")}else{
    p_nfk[[i]] <- p_nfk[[i]] + theme(axis.title.y = element_blank())
  } 
  
}

p_nf_kop_deco <- ggarrange(plotlist = p_nfk, nrow = 1, ncol =5,
                           widths = c(1.25,1,1,1,1),
                           labels = c("c","", "", "", "", ""))

# night frp ===============
p_nfrpk <- list()
ts_trends_nf <- list()
output_data_nfrp <- list()
for(i in 1:5){
  dk <- filter(long_df_k, koppen == ks[i] & dn_detect == "night")
  
  df_frp_night <- dts1(pull(dk, year_month),
                       pull(dk, mean_frp_per_detection),
                       12, type = "additive") %>%
    mutate(seasonal_removed = observation-seasonal,
           timestep = 1:nrow(.))  %>%
    na.omit()

  mod<-mblm(trend ~ timestep, df_frp_night, repeated = TRUE)
  
  ts_frp_trend_night <- mod %>%
    predict(interval = "confidence")
  
  sig <- ktp %>%
    dplyr::filter(Koppen == ks[i]) %>%
    dplyr::select(mean_frp_per_night_detection) %>%
    pull() %>%
    str_detect("\\*")
  
  output_data_nfrp[[i]]<- cbind(df_frp_night, ts_frp_trend_night) %>%
    mutate(koppen = ks[i],
           variable = "Night FRP per detection (MW)",
           sig=sig)
  
  p_nfrpk[[i]] <- ggplot(cbind(df_frp_night, ts_frp_trend_night),
                         aes(x=date, y=trend)) +
    geom_line(alpha=0.5, color = daynight_cols[2]) +
    theme_classic() +
    ggtitle(ks[i])+
    theme(text = element_text(family="Arial"),
          axis.title.x = element_blank())
  
  if(sig){
    p_nfrpk[[i]] <- p_nfrpk[[i]]+
      geom_line(aes(y=fit), lwd=1, color = daynight_cols[2]) +
      geom_line(aes(y=upr), lty =2, color = daynight_cols[2]) +
      geom_line(aes(y=lwr), lty =2, color = daynight_cols[2]) }
  
  if(i == 1){p_nfrpk[[i]] <- p_nfrpk[[i]] + ylab("Nighttime FRP\nper detection (MW)")}else{
    p_nfrpk[[i]] <- p_nfrpk[[i]] + theme(axis.title.y = element_blank())
  }}

p_nfrp_kop_deco <- ggarrange(plotlist = p_nfrpk, nrow = 1, ncol =5, widths = c(1.25,1,1,1,1), labels = c("a","", "", "", "", ""))

# day frp ===============
p_dfrpk <- list()
ts <- list()
output_data_dfrp <- list()
for(i in 1:5){
  dk <- filter(long_df_k, koppen == ks[i] & dn_detect == "day")
  
  df <- dts1(pull(dk, year_month),
                       pull(dk, mean_frp_per_detection),
                       12, type = "additive") %>%
    mutate(seasonal_removed = observation-seasonal,
           timestep = 1:nrow(.))  %>%
    na.omit()
  
  ts <- mblm(trend ~ timestep, df, repeated = TRUE) %>%
    predict(interval = "confidence")
  
  sig <- ktp %>%
    dplyr::filter(Koppen == ks[i]) %>%
    dplyr::select(mean_frp_per_day_detection) %>%
    pull() %>%
    str_detect("\\*")
  
  output_data_dfrp[[i]]<- cbind(df, ts) %>%
    mutate(koppen = ks[i],
           variable = "Day FRP per detection (MW)",
           sig=sig)
  
  p_dfrpk[[i]] <- ggplot(cbind(df, ts),
                         aes(x=date, y=trend)) +
    geom_line(alpha=0.5, color = daynight_cols[1]) +
    theme_classic() +
    theme(text = element_text(family="Arial"),
          axis.title.x = element_blank())
  
  if(sig){
    p_dfrpk[[i]] <- p_dfrpk[[i]] +
      geom_line(aes(y=fit), lwd=1, color = daynight_cols[1]) +
      geom_line(aes(y=upr), lty =2, color = daynight_cols[1]) +
      geom_line(aes(y=lwr), lty =2, color = daynight_cols[1])
  }
  
  if(i == 1){p_dfrpk[[i]] <- p_dfrpk[[i]] + ylab("Daytime FRP\nper detection (MW)")}else{
    p_dfrpk[[i]] <- p_dfrpk[[i]] + theme(axis.title.y = element_blank())
  }}

p_dfrp_kop_deco <- ggarrange(plotlist = p_dfrpk, nrow = 1, ncol =5, widths = c(1.25,1,1,1,1))

# night afd ===============
p_nafdk <- list()
ts <- list()
output_data_nafd<-list()
for(i in 1:5){
  dk <- filter(long_df_k, koppen == ks[i] & dn_detect == "night")
  
  df <- dts1(pull(dk, year_month),
                       pull(dk, n_per_op_per_Mkm2),
                       12, type = "additive") %>%
    mutate(seasonal_removed = observation-seasonal,
           timestep = 1:nrow(.))  %>%
    na.omit()
  
  ts <- mblm(trend ~ timestep, df, repeated = TRUE) %>%
    predict(interval = "confidence")
  
  sig <- ktp %>%
    dplyr::filter(Koppen == ks[i]) %>%
    dplyr::select(night_afd_per_op_per_Mkm2) %>%
    pull() %>%
    str_detect("\\*")
  
  output_data_nafd[[i]] <-cbind(df, ts)%>%
    mutate(koppen = ks[i],
           variable = "Night Active fire detections",
           sig=sig)
  
  p_nafdk[[i]] <- ggplot(cbind(df, ts),
                         aes(x=date, y=trend)) +
    geom_line(alpha=0.5, color = daynight_cols[2])  +
    theme_classic() +
    theme(text = element_text(family="Arial"),
          axis.title.x = element_blank())
  
  if(sig){
    p_nafdk[[i]] <-  p_nafdk[[i]] +
      geom_line(aes(y=fit), lwd=1, color = daynight_cols[2]) +
      geom_line(aes(y=upr), lty =2, color = daynight_cols[2]) +
      geom_line(aes(y=lwr), lty =2, color = daynight_cols[2])
  }
  
  if(i == 1){p_nafdk[[i]] <- p_nafdk[[i]] + ylab("Nighttime Active Fire\nDetections per Mkm2")}else{
    p_nafdk[[i]] <- p_nafdk[[i]] + theme(axis.title.y = element_blank())
  }}

p_nafd_kop_deco <- ggarrange(plotlist = p_nafdk, nrow = 1, ncol =5, widths = c(1.25,1,1,1,1))

# day afd ===============
p_dafdk <- list()
ts <- list()
output_data_dafd <- list()

for(i in 1:5){
  dk <- filter(long_df_k, koppen == ks[i] & dn_detect == "day")
  
  df <- dts1(pull(dk, year_month),
             pull(dk, n_per_op_per_Mkm2),
             12, type = "additive") %>%
    mutate(seasonal_removed = observation-seasonal,
           timestep = 1:nrow(.))  %>%
    na.omit()
  
  ts <- mblm(trend ~ timestep, df, repeated = TRUE) %>%
    predict(interval = "confidence")
  
  sig <- ktp %>%
    dplyr::filter(Koppen == ks[i]) %>%
    dplyr::select(day_afd_per_op_per_Mkm2) %>%
    pull() %>%
    str_detect("\\*")
  
  output_data_dafd[[i]] <-cbind(df, ts)%>%
    mutate(koppen = ks[i],
           variable = "Day Active fire detections",
           sig=sig)
  
  p_dafdk[[i]] <- ggplot(cbind(df, ts),
                         aes(x=date, y=trend)) +
    geom_line(alpha=0.5, color = daynight_cols[1]) +
    theme_classic() +
    theme(text = element_text(family="Arial"),
          axis.title.x = element_blank())
  
  if(sig){
    p_dafdk[[i]] <- p_dafdk[[i]] +
      geom_line(aes(y=fit), lwd=1, color = daynight_cols[1]) +
      geom_line(aes(y=upr), lty =2, color = daynight_cols[1]) +
      geom_line(aes(y=lwr), lty =2, color = daynight_cols[1])
  }
  
  if(i == 1){p_dafdk[[i]] <- p_dafdk[[i]] + ylab("Daytime Active Fire\nDetections per Mkm2")}else{
    p_dafdk[[i]] <- p_dafdk[[i]] + theme(axis.title.y = element_blank())
  }}

p_dafd_kop_deco <- ggarrange(plotlist = p_dafdk, nrow = 1, ncol =5, widths = c(1.25,1,1,1,1))

# full plots =====================

ggsave(ggarrange(p_nfrp_kop_deco,
                 p_dfrp_kop_deco,  ncol=1, nrow=2),
       filename = "figs/EDF8a_koppen_frp_smoothed_trends-croplands-included.png",
       width = 10, height = 5)

ggsave(ggarrange(p_nf_kop_deco,
                 p_dafd_kop_deco, 
                 p_nafd_kop_deco,  ncol=1, nrow=3),
       filename = "figs/EDF8c_koppen_afd_smoothed_trends-croplands-included.png",
       width = 10, height = 7)

# whole thing, divided by climatology trend ====================================
system2(command = "aws", args = "s3 cp s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-global-trend-matched-to-climatology-by-month.csv data/out/mcd14ml-global-trend-matched-to-climatology-by-month.csv")

system2(command = "aws", args = "s3 cp s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-trend-matched-to-climatology-by-month-koppen.csv data/out/mcd14ml-trend-matched-to-climatology-by-month-koppen.csv")

long_df_c <- read_csv("data/out/mcd14ml-global-trend-matched-to-climatology-by-month.csv")%>%
  dplyr::mutate(time = as.numeric(difftime(time1 = year_month, time2 = min(year_month), units = "days")))

long_df_ck <- read_csv("data/out/mcd14ml-trend-matched-to-climatology-by-month-koppen.csv")%>%
  dplyr::mutate(time = as.numeric(difftime(time1 = year_month, time2 = min(year_month), units = "days"))) %>%
  mutate(koppen = lut_kop[koppen] %>% factor(levels = c("Equatorial", "Arid", "Temperate", "Boreal"))) %>%
  bind_rows(long_df_c %>% mutate(koppen = "Global")) %>%
  mutate(koppen = as.factor(koppen)%>% 
           fct_relevel(c("Global", "Boreal", "Temperate", "Arid", "Equatorial")))

ks<- c("Global", "Boreal", "Temperate", "Arid", "Equatorial")

# need to create the new seasonal-adjusted trends divided by climatology
# ktp <- read_csv("out/koppen_trends_pretty_year_sr.csv") %>%
#   mutate(Koppen = str_to_title(Koppen))

# night frp ====================================================================
p_nfrpk <- list()
ts_trends_nf <- list()
output_data_clim<-list()

c_trends <- c("increase in flammable night hours", 
              "decrease in flammable night hours")
counter <- 1

c_trend_cols <- c(daynight_cols[2], "springgreen4")
c_trend_cols <- c("chocolate4", "springgreen4")


for(j in 1:2){
  for(i in 1:5){
  dk <- filter(long_df_ck, koppen == ks[i] & dn_detect == "night" & big_trend == c_trends[j])

  # doing the ts decomposition and modelling
  df_frp_night <- dts1(pull(dk, year_month),
                       pull(dk, mean_frp_per_detection),
                       12, type = "additive") %>%
    mutate(seasonal_removed = observation-seasonal,
           timestep = 1:nrow(.))  %>%
    na.omit()
  
  mod<-mblm(trend ~ timestep, df_frp_night, repeated = TRUE)
  
  ts_frp_trend_night <- mod %>%
    predict(interval = "confidence")
  
  sig <- ifelse(summary(mod)$coefficients[2,4] < 0.01, TRUE, FALSE)
  
  output_data_clim[[counter]] <-cbind(df_frp_night, ts_frp_trend_night)%>%
    mutate(koppen = ks[i],
           variable = c_trends[j],
           sig=sig)
  
  # making the plot
  p_nfrpk[[counter]] <- ggplot(cbind(df_frp_night, ts_frp_trend_night),
                         aes(x=date, y=trend)) +
    geom_line(alpha=0.5, color = c_trend_cols[j]
              ) +
    theme_classic() +
    theme(text = element_text(family="Arial"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=15),
          axis.text = element_text(size=15))+
    scale_x_date(expand=c(0,0), breaks = "5 years",date_labels = "%Y") +
    scale_y_continuous(labels = scales::label_number(accuracy=1))

  if(j == 1){
    p_nfrpk[[counter]] <- p_nfrpk[[counter]]+
      ggtitle(ks[i]) +
      theme(plot.title = element_text(size=20))

  }
  
  if(sig){
    p_nfrpk[[counter]] <- p_nfrpk[[counter]]+
      geom_line(aes(y=fit), lwd=1.5, color = c_trend_cols[j]) +
      geom_line(aes(y=upr), lwd=1, lty =2, color = c_trend_cols[j]) +
      geom_line(aes(y=lwr), lwd=1, lty =2, color = c_trend_cols[j]) }
  

  if(i == 1){
    p_nfrpk[[counter]] <- p_nfrpk[[counter]] + 
      ylab(paste0("Night FRP per detection (MW)"))
  }else{
    p_nfrpk[[counter]] <- p_nfrpk[[counter]] + 
      theme(axis.title.y = element_blank())
  }
  
  counter <- counter +1
}}

df_legend_c <- tibble(x=c(1,2), y=c(1,2), 
                    `Climatology Trend`= c("Increase in Flammable Hours at Night",
                            "Decrease in Flammable Hours at Night"))  %>%
  ggplot(aes(x=x,y=y,fill=`Climatology Trend`)) +
  geom_raster() +
  theme_transparent()+
  theme(legend.title = element_text(size=25),
        legend.text = element_text(size=25),
        legend.direction = "horizontal")+
  scale_fill_manual(values = c("Increase in Flammable Hours at Night" = c_trend_cols[1],
                               "Decrease in Flammable Hours at Night" = c_trend_cols[2])) 

ctrend_leg<-get_legend(df_legend_c)

p_nfrp_kop_deco <- ggarrange(plotlist = p_nfrpk, nrow = 2, ncol =5, align = "v",
                             widths = c(1.05,1,1,1,1,
                                        1.05,1,1,1,1), 
                             labels = c("b","", "", "", "", "","", "", "", "", ""),
                             font.label = list(size=20))

p_final <- cowplot::ggdraw(xlim=c(0,18), ylim=c(0,9)) +
  draw_plot(p_nfrp_kop_deco,x = 0,y=1, width = 18, height=8) +
  draw_plot(ctrend_leg, x=9,y=0, height =1)

ggsave(p_final, filename="figs/EDF8b_global_trends_divided_by_climatology_trends.png", 
       height = 9, width = 18, bg = "white")


# edf8, from one data set

edf8a<- ggplot(bind_rows(output_data_nfrp,output_data_dfrp)%>%
                 mutate(fit = ifelse(sig, fit, NA),
                        upr = ifelse(sig, upr, NA),
                        lwr = ifelse(sig, lwr, NA)), 
               aes(x=date, y=trend, color = variable)) + 
  facet_grid(variable ~ koppen, scales="free") +
  geom_line(alpha=0.5) +
  geom_line(aes(y = fit)) +
  geom_line(aes(y=upr),lty=2)+
  geom_line(aes(y=lwr),lty=2)+
  scale_color_manual(values = c(daynight_cols[1], daynight_cols[2]))+
  theme_classic() +
  theme(panel.border = element_rect(size=.5, fill="transparent"),
        axis.title = element_blank(),
        text = element_text(size=7)) +
  guides(color="none")

ggsave(edf8a, 
       filename = "figs/EDF8a_global_trends_in_frp.pdf",
       width=183, units = "mm", dpi=300, height = 100)

edf8b <- ggplot(bind_rows(output_data_clim) %>%
                  mutate(fit = ifelse(sig, fit, NA),
                         upr = ifelse(sig, upr, NA),
                         lwr = ifelse(sig, lwr, NA)), 
                aes(x=date, y=trend, color = variable)) + 
  facet_grid(variable ~ koppen, scales="free") +
  geom_line(alpha=0.5) +
  geom_line(aes(y = fit)) +
  geom_line(aes(y=upr),lty=2)+
  geom_line(aes(y=lwr),lty=2)+
  scale_color_manual(values = c(c_trend_cols[2], c_trend_cols[1]))+
  theme_classic() +
  theme(panel.border = element_rect(size=.5, fill="transparent"),
        axis.title = element_blank(),
        text = element_text(size=7)) +
  guides(color="none")

ggsave(edf8b, 
       filename = "figs/EDF8_global_trends_divided_by_climatology_trends.pdf",
       width=183, units = "mm", dpi=300, height = 100)

edf8c_df <- bind_rows(output_data_nafd,output_data_dafd, output_data_nf)%>%
  mutate(fit = ifelse(sig, fit, NA),
         upr = ifelse(sig, upr, NA),
         lwr = ifelse(sig, lwr, NA),
         koppen = fct_relevel(koppen, 
                              c("Global", "Boreal", 
                                "Temperate", "Arid", "Equatorial")),
         variable = fct_relevel(variable, c("Day Active fire detections",
                                            "Night Active fire detections",
                                            "Percent Nighttime Detections")),
         variable = fct_recode(variable,  "Night"="Night Active fire detections",
                               "Day"="Day Active fire detections",
                               "Percent at Night" = "Percent Nighttime Detections"))

edf8c1<- ggplot(edf8c_df%>% filter(variable == "Day"), 
               aes(x=date, y=trend, color = variable)) + 
  facet_wrap( ~ koppen,scales="free_y", nrow=1) +
  geom_line(alpha=0.5) +
  geom_line(aes(y = fit)) +
  geom_line(aes(y=upr),lty=2)+
  geom_line(aes(y=lwr),lty=2)+
  scale_color_manual(values = c(daynight_cols[1], daynight_cols[2], "black"))+
  theme_classic() +
  scale_y_continuous()+
  ylab("Day")+
  theme(panel.border = element_rect(size=.5, fill="transparent"),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x =  element_blank(), 
        strip.background = element_blank(),
        text = element_text(size=7)) +
  guides(color="none")

edf8c2<- ggplot(edf8c_df%>% filter(variable == "Night"), 
                aes(x=date, y=trend, color = variable)) + 
  facet_wrap( ~ koppen,scales="free_y", nrow=1) +
  geom_line(alpha=0.5) +
  geom_line(aes(y = fit)) +
  geom_line(aes(y=upr),lty=2)+
  geom_line(aes(y=lwr),lty=2)+
  scale_color_manual(values = c(daynight_cols[2], "black"))+
  theme_classic() +
  scale_y_continuous()+
  ylab("Night")+
  theme(panel.border = element_rect(size=.5, fill="transparent"),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x =  element_blank(), 
        strip.background = element_blank(),
        strip.text=element_blank(),
        text = element_text(size=7)) +
  guides(color="none")

edf8c3<- ggplot(edf8c_df%>% filter(variable == "Percent at Night"), 
                aes(x=date, y=trend, color = variable)) + 
  facet_wrap( ~ koppen,scales="free_y", nrow=1) +
  geom_line(alpha=0.5) +
  geom_line(aes(y = fit)) +
  geom_line(aes(y=upr),lty=2)+
  geom_line(aes(y=lwr),lty=2)+
  scale_color_manual(values = c( "black"))+
  theme_classic() +
  scale_y_continuous()+
  ylab("Percent at Night")+
  theme(panel.border = element_rect(size=.5, fill="transparent"),
        axis.title = element_blank(),
        axis.title.y.right = element_text(),
        # axis.ticks.x =  element_blank(), 
        strip.background = element_blank(),
        strip.text=element_blank(),
        text = element_text(size=7)) +
  guides(color="none")


ggsave(ggdraw(annotate_figure(ggarrange(edf8c1, edf8c2, edf8c3, nrow=3), 
                              left=text_grob("Overpass-Adjusted Active Fire Detections", rot=90))) +
         draw_plot_label("c", x=0, y=1, size=12), 
       filename = "figs/EDF8c_global_trends_in_afd.pdf",
       width=183, units = "mm", dpi=300, height = 120)

ggsave(ggdraw(annotate_figure(ggarrange(edf8c1, edf8c2, edf8c3, nrow=3), 
                              left=text_grob("Overpass-Adjusted Active Fire Detections", rot=90))) +
         draw_plot_label("c", x=0, y=1, size=12), 
       filename = "figs/EDF8c_global_trends_in_afd.png",
       width=183, units = "mm", dpi=300, height = 120)

ggsave(ggarrange(edf8a, edf8b, edf8c, labels = c("a", "b", "c"), 
                 nrow=3, ncol=1, heights = c(2,2,3)),
       filename = "figs/edf8_all_one.pdf", width = 183, height=247, dpi=300, units="mm", bg="white")

edf8_ab_df<-bind_rows(#output_data_nafd,output_data_dafd, output_data_nf,
                   output_data_clim, output_data_dfrp, output_data_nfrp)%>%
  mutate(fit = ifelse(sig, fit, NA),
         upr = ifelse(sig, upr, NA),
         lwr = ifelse(sig, lwr, NA),
         koppen = fct_relevel(koppen, 
                              c("Global", "Boreal", 
                                "Temperate", "Arid", "Equatorial")),
         variable = fct_relevel(variable, c("Night FRP per detection (MW)",
                                            "Day FRP per detection (MW)",
                                            "increase in flammable night hours",
                                            "decrease in flammable night hours")),
         variable = fct_recode(variable,  "Night"="Night FRP per detection (MW)",
                                            "Day"="Day FRP per detection (MW)",
                                            "More Flammable Night Hours"="increase in flammable night hours",
                                            "Fewer Flammable Night Hours"="decrease in flammable night hours"))

edf8_ab <- ggplot(edf8_ab_df, 
               aes(x=date, y=trend, color = variable)) + 
  facet_grid(variable ~ koppen, scales="free") +
  geom_line(alpha=0.5) +
  geom_line(aes(y = fit)) +
  geom_line(aes(y=upr),lty=2)+
  geom_line(aes(y=lwr),lty=2)+
  scale_color_manual(values = c(daynight_cols[2], daynight_cols[1],c_trend_cols[1], c_trend_cols[2]))+
  theme_classic() +
  ylab("Fire Radiative Power per Detection (MW)") +
  theme(panel.border = element_rect(size=.5, fill="transparent"),
        axis.title.x= element_blank(),
        strip.background = element_blank(),
        text = element_text(size=7),
        axis.title.y = element_text(size=7)) +
  guides(color="none")

ggsave(ggdraw(edf8_ab) +
         cowplot::draw_plot_label("a", x=0.06, y=.95, size = 12)+
         cowplot::draw_plot_label("b", x=0.06, y=.48, size=12),
       filename = "figs/edf8_ab.pdf", 
       width = 183, height=150, dpi=300, units="mm", bg="white")

ggsave(ggdraw(edf8_ab) +
         cowplot::draw_plot_label("a", x=0.06, y=.95, size = 12)+
         cowplot::draw_plot_label("b", x=0.06, y=.48, size=12),
       filename = "figs/edf8_ab.png", 
       width = 183, height=150, dpi=300, units="mm", bg="white")


ggsave(ggarrange(edf8_ab, annotate_figure(
  ggarrange(edf8c1, edf8c2, edf8c3, nrow=3), 
    right=text_grob("         Day                                       Night                                        Percent at Night",
      size = 6, rot=270, vjust=1.8),
    left=text_grob("Overpass-Adjusted Active Fire Detections",size = 7, rot=90)),
  nrow=2, heights = c(5.5,4),font.label = list(size=8), labels = c("a", "b")), filename = "figs/edf8_all_one.jpg",
       width = 183, height=230, dpi=600, units="mm", bg="white")

ggsave(ggarrange(edf8_ab, annotate_figure(
  ggarrange(edf8c1, edf8c2, edf8c3, nrow=3), 
  right=text_grob("         Day                                       Night                                        Percent at Night",
                  size = 6, rot=270, vjust=1.8),
  left=text_grob("Overpass-Adjusted Active Fire Detections",size = 7, rot=90)),
  nrow=2, heights = c(5.5,4),font.label = list(size=8) ,labels = c("a", "b")), filename = "figs/edf8_all_one.png",
  width = 183, height=230, dpi=600, units="mm", bg="white")

# possible panels for research brief

prb<-edf8_ab_df %>%
  filter(variable != "Night" & variable != "Day" & koppen == "Global") %>%
  mutate(variable = forcats::fct_relevel(variable,
                                         "More Flammable Night Hours", 
                                         "Fewer Flammable Night Hours"))%>%
  ggplot(aes(x=date, y=trend, color = variable)) + 
    facet_wrap(~variable, nrow=1) +
    geom_line(alpha=0.5, lwd=0.25, key_glyph = "rect") +
    geom_line(aes(y = fit), lwd=0.25, key_glyph = "rect") +
    geom_line(aes(y=upr),lty=2, lwd=0.25, key_glyph = "rect")+
    geom_line(aes(y=lwr),lty=2, lwd=0.25, key_glyph = "rect")+
    scale_color_manual(name = "Climatology Trend", values = c(c_trend_cols[1], c_trend_cols[2]))+
    theme_classic() +
    ylab("Night FRP\n(MW/detection)") +
    # guides(color= "none")+
    theme(panel.border = element_rect(size=.75, fill="transparent"),
          axis.title.x= element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          # strip.text = element_text(margin = margin(0.3,0,0.3,0), size=5),
          text = element_text(size=5),
          axis.title.y = element_text(size=5))

ggsave(prb, filename = "figs/rb_2pan_wlegend.pdf", 
       units="mm", height = 30, width = 115, bg="white", dpi=600)


source_data_for_nature <- bind_rows(edf8_ab_df, edf8c_df)
write_csv(source_data_for_nature, "figs/source_data/edf8.csv")
