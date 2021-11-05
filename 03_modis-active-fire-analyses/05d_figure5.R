# modeling overpass count-adjusted active fire data

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
library(ggplottimeseries)
posneg_cols <- c("Positive"="darkorange1", "Negative"="#07484D") # colours.cafe palette 582
posneg_cols <- c("Positive"="darkorange1", "Negative"="aquamarine4") # colours.cafe palette 582

mask_col<- #DDCEBF
  daynight_cols <- c("#B2182B","#2166AC") # red is #B2182B
dir.create("out")

# thresholds ===================================================================
thresholds <- read_csv("in/zero-goes-af-vpd-thresholds-with-landcover-codes.csv")
burnable_lcs<- pull(thresholds, lc_name)
# lc koppen setup ==============================================================
lut_kop <- c("Equatorial", "Arid", "Temperate", "Boreal", "Polar")
names(lut_kop) <- c(1,2,3,4,5)

lut_lc<-c( "Evergreen Needleleaf Forests",
           "Evergreen Broadleaf Forests",
           "Deciduous Needleleaf Forests",
           "Deciduous Broadleaf Forests",
           "Mixed Forests",
           "Closed Shrublands",
           "Open Shrublands",
           "Woody Savannas",
           "Savannas",
           "Grasslands",
           "Permanent Wetlands",
           "Croplands",
           "Urban and Built-up Lands",
           "Cropland Natural Vegetation Mosaics",
           "Permanent Snow and Ice",
           "Barren",
           "Water Bodies")
names(lut_lc) <- str_pad(1:17, width = 2, side = "left",pad = "0")

# landcover
template <- list.files("data/annual_adjusted_counts", 
                       pattern = "day",
                       full.names = TRUE) %>%
  terra::rast()

lck_shifted <- terra::rast("in/lc_koppen_2010_mode.tif") %>%
  terra::aggregate(4, fun = "modal") %>% 
  terra::shift(dx = -179.5, dy = -0.5) %>%
  terra::crop(template) # getting rid of an extra polar row

# creating the burnable land mask ==============================================
lck_s <- terra::rast("in/lck_shifted.tif")
lck_s[lck_s==100] <- NA
lck_s[lck_s==200] <- NA
lck_s[lck_s==300] <- NA
lck_s[lck_s==400] <- NA
lck_s[lck_s==500] <- NA
brnbl<-thresholds %>% 
  dplyr::select(lc_kop=lc_name) %>% 
  mutate(Burnable = "yes")

lck_df<-as.data.frame(lck_shifted, xy=TRUE)%>%
  mutate(Koppen = lut_kop[str_sub(lc_koppen_2010_mode,1,1)],
         Landcover = lut_lc[str_sub(lc_koppen_2010_mode,2,3)],
         lc_kop = paste(Koppen, Landcover),
         croplands = str_detect(Landcover, "Crop"),
         croplands = ifelse(croplands == TRUE, "Croplands", "Not Croplands")) %>%
  left_join(brnbl) %>%
  replace_na(list(Burnable = "no")) %>%
  mutate(x = ifelse(x< 0, x+360, x)-181)

# night fraction with inset frp trends =========================================
system2(command = "aws", args = "s3 cp s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-global-trend-matched-to-climatology-by-month.csv data/out/mcd14ml-global-trend-matched-to-climatology-by-month.csv")
system2(command = "aws", args = "s3 cp s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/parallelized-trends-by-cell.csv data/out/parallelized-trends-by-cell.Rda")
system2(command = "aws", args = "s3 cp s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-global-trend-matched-to-climatology-by-year.csv data/out/mcd14ml-global-trend-matched-to-climatology-by-year.csv")

long_df<-read_csv("data/out/mcd14ml-global-trend-matched-to-climatology-by-month.csv") 
long_dfy<-read_csv("data/out/mcd14ml-global-trend-matched-to-climatology-by-year.csv") 

load("data/out/parallelized-trends-by-cell.Rda")
long_df_c <- read_csv("data/out/mcd14ml-global-trend-matched-to-climatology-by-month.csv")

# writing out source data for final figures
write_csv(result, "figs/source_data/fig_5_main.csv")
write_csv(long_dfy %>%
            filter(big_trend == "increase in flammable night hours", dn_detect == "night"), 
          "figs/source_data/fig_5_inset.csv")

# creating the map portion of the figure
p_nfrp_single <-ggplot(result %>% 
                         filter(pval < 0.05,n>180) %>%
                         mutate(`Trend in Nighttime FRP ` = ifelse(estimate >0, "Positive", "Negative"))) +
  geom_sf(data = st_read("world.gpkg"), lwd=0.25, fill="white")+
  geom_raster(data = lck_df%>%
                filter(Burnable == "yes"), 
              aes(x=x,y=y), fill = "grey90")+
  geom_raster(aes(x=longitude,y=latitude,fill=`Trend in Nighttime FRP `)) +
  scale_fill_manual(values = posneg_cols)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(-57.625, 78.125))+
  theme_void()+
  # ylim(c(-52.625, 75.125)) + 
  theme(text = element_text(size=5),
        plot.caption = element_text(hjust=0.5, size=rel(1.2)),
        legend.position = "bottom",
        legend.text = element_text(size = 30),
        legend.title = element_text(size=30),
        panel.background = element_rect(color = "black", size=2))

# creating the inset portion of the figure

p_inset2 <- long_dfy %>%
  filter(big_trend == "increase in flammable night hours", dn_detect == "night") %>%
  ggplot(aes(x=acq_year, y=mean_frp_per_detection))+
  geom_line(lwd=1, color=daynight_cols[2]) +
  geom_smooth(method = "lm", se=F, color=daynight_cols[2], lwd=2) +
  theme_void() +
  labs(title ="Night FRP (MW/detection)" , subtitle = ""#,
       #caption =  "Where Flammable Hours Increased at Night"
       )+
  scale_y_continuous(name = "", breaks = c(30,35,40))+ 
  theme(text = element_text(size=5),
        axis.line=element_blank(),
        axis.text.y.left = element_text(size=20),
        axis.text.x.bottom = element_text(size=20), 
        axis.ticks = element_blank(),
        plot.caption = element_text(size=17),
        legend.position = c(0.01,.65),
        legend.justification = c(0,1),
        legend.text = element_text(size=15),
        legend.title = element_blank(),
        plot.title = element_text(size=25),
        plot.subtitle = element_text( size=18),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(color = "black", 
                                        size=1, 
                                        fill="transparent"))+
  scale_x_continuous(breaks = c(2003, 2020), 
                     labels = c("   2003", "2020   "),
                     name="")

# putting it all together
p_fig5<-ggdraw() +
  draw_plot(p_nfrp_single) +
  draw_plot(p_inset2,
            x = 0.02,
            y = 0.15,
            width = 0.25,
            height = 0.35)

# writing it out!
ggsave(p_fig5, filename = "figs/fig5_percent_night_w_inset_unagged2.png", 
       height=9, width=20, bg="white")

ggsave(p_fig5, 
       filename = "figs/fig5_percent_night_w_inset_unagged2.pdf", 
       height=100, 
       width=183, 
       units="mm", 
       bg="white", 
       dpi=300)

ggsave(p_inset2, filename="figs/fig5_inset.pdf", width = 6, height=4)



