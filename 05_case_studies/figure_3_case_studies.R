# Night fires figure 3

# setup ========================================================================
library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(raster)
library(ggsn)
library(stars)
library(ggpubr)
library(scales)
library(ggnewscale)
library(ggthemes)
library(lubridate)
library(cowplot)
theme_set(theme_void())

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

shade_cols<-c("#f0dbdb","#c0e9f6") # first pink ("#f0dbdb"), then skyblue
# thresholds ====================================================================
thresholds<-read_csv("fig_3_data/updated-goes-af-vpd-thresholds.csv")
# thresholds[1:20,]

# landcover classification =====================================================
classification_table <- data.frame(
  name = c("Evergreen Needleleaf Forests", "Evergreen Broadleaf Forests",
           "Deciduous Needleleaf Forests","Deciduous Broadleaf Forests",
           "Mixed Forests","Closed Shrublands",
           "Open Shrublands","Woody Savannas",
           "Savannas","Grasslands",
           "Permanent Wetlands","Croplands",
           "Urban and Built-up","Cropland/Natural  Vegetation  Mosaics",
           "Permanent Snow and Ice","Barren",
           "Water Bodies", "Unclassified"),
  value = c(1:17,255)
)


lut_lc <-  c("1"="Evergreen Needleleaf Forests", "2"= "Evergreen Broadleaf Forests",
             "3"= "Deciduous Needleleaf Forests","4"= "Deciduous Broadleaf Forests",
             "5"= "Mixed Forests","6"= "Closed Shrublands",
             "7"= "Open Shrublands","8"= "Woody Savannas",
             "9"=  "Savannas","10"= "Grasslands",
             "11"= "Permanent Wetlands","12"= "Croplands",
             "13"=  "Urban and Built-up",
             "14"= "Cropland/Natural  Vegetation  Mosaics",
             "15"=  "Permanent Snow and Ice","16"= "Barren",
             "17"=  "Water Bodies", "255"= "Unclassified")

lut_colors <- c("Evergreen Needleleaf Forests" = "#006400",
                "Evergreen Broadleaf Forests" = "#228B22",
                "Deciduous Needleleaf Forests"= "#458B00",
                "Deciduous Broadleaf Forests" = "#008B45",
                "Mixed Forests"= "#3CB371",
                "Closed Shrublands" = "#6E8B3D",
                "Open Shrublands" = "#9ACD32",
                "Woody Savannas" = "#6B8E23",
                "Savannas" = "#8B8B00",
                "Grasslands" = "#CDC673",
                "Permanent Wetlands" = "#00868B",
                "Croplands" = "#EE9572",
                "Urban and Built-up" = "grey10",
                "Cropland/Natural  Vegetation  Mosaics" = "#EE8262",
                "Permanent Snow and Ice" = "#FFFFFF",
                "Barren" = "#DEB887",
                "Water Bodies" = "#87CEEB",
                "Unclassified" = "#BEBEBE"
                )

lut_lc_simple <-  c("Evergreen Needleleaf Forests" = "Forests",
                    "Evergreen Broadleaf Forests" = "Forests",
                    "Deciduous Needleleaf Forests"= "Forests",
                    "Deciduous Broadleaf Forests" = "Forests",
                    "Mixed Forests"= "Forests",
                    "Closed Shrublands" = "Grasslands, Shrublands\nCroplands, Savannas",
                    "Open Shrublands" = "Grasslands, Shrublands\nCroplands, Savannas",
                    "Woody Savannas" = "Grasslands, Shrublands\nCroplands, Savannas",
                    "Savannas" = "Grasslands, Shrublands\nCroplands, Savannas",
                    "Grasslands" = "Grasslands, Shrublands\nCroplands, Savannas",
                    "Permanent Wetlands" = "Grasslands, Shrublands\nCroplands, Savannas",
                    "Croplands" = "Grasslands, Shrublands\nCroplands, Savannas",
                    "Urban and Built-up Lands" = "Urban and Built-up",
                    "Cropland/Natural  Vegetation  Mosaics" = "Grasslands, Shrublands\nCroplands, Savannas",
                    "Permanent Snow and Ice" ="Grasslands, Shrublands\nCroplands, Savannas",
                    "Barren" = "Grasslands, Shrublands\nCroplands, Savannas",
                    "Water Bodies" = "Water Bodies",
                    "Unclassified" = "Grasslands, Shrublands\nCroplands, Savannas"
)

# RColorBrewer::brewer.pal(9,"Set1")
# [1] "#E41A1C" "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00" "#FFFF33" "#A65628" "#F781BF" "#999999"

lut_colors_simple <- c("Forests" = "#228B22",
                       "Grasslands, Shrublands\nCroplands, Savannas" = "#CDC673",
                "Urban and Built-up" = "grey10",
                "Water Bodies" = "#87CEEB"
)
# from colorbrewer
lut_colors_simple <- c("Forests" = "#4DAF4A",
                       "Grasslands, Shrublands\nCroplands, Savannas" = "#FFFF33",
                      
                       "Urban and Built-up" = "grey10",
                       "Water Bodies" = "#87CEEB"
)

df_colors <- read_csv("fig_3_data/landcover-colors-2021.csv") %>%
  mutate(lc_name= replace(lc_name, lc_name == "Urban and Built-up Lands",
                          "Urban and Built-up"))

lut_colors <- df_colors %>%
  dplyr::select(color) %>%
  pull
names(lut_colors) <- df_colors$lc_name
lut_colors[17] <- "#87CEEB"
names(lut_colors)[17] <- "Water Bodies"
lut_colors[18] <- "#BEBEBE"
names(lut_colors)[18] <- "Unclassified"




daynight_cols <- c("#2166AC","#B2182B") # red is #B2182B
daynight_cols <- c("#377EB8","#E41A1C") # red is #E41A1C

snowy_lc_file <- "fig_3_data/MCD12Q1_tifs/h29v12.tif"
tubbs_lc_file <- "fig_3_data/MCD12Q1_tifs/h08v05.tif"

modis_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

# snowy complex ================================================================

snowy <- st_read("fig_3_data/snowy_complex.gpkg") %>%
  mutate(daynight = factor(daynight, levels = c("night", "day")))%>%
  mutate(acq_date = as.Date(acq_date)) %>%
  filter(acq_date > as.Date("2019-12-28"),
         acq_date < as.Date("2020-01-6"))
daterange_s <- snowy$acq_date %>% as.Date() %>% range()
bbox <- st_bbox(snowy)%>% as.numeric()

snowy_modis <- read_csv("fig_3_data/snowy_modis_afd.csv")%>%
  dplyr::mutate(acq_hour = as.numeric(substr(acq_time, start = 1, stop = 2)),
                acq_min = as.numeric(substr(acq_time, start = 3, stop = 4)),
                acq_datetime = ymd_hm(paste0(acq_date, " ", acq_hour, ":", acq_min)),
                acq_year = year(acq_datetime),
                acq_month = month(acq_datetime),
                acq_day = day(acq_datetime),
                solar_offset = longitude / 15,
                hemisphere = ifelse(latitude >= 0, yes = "Northern hemisphere", no = "Southern hemisphere"),
                acq_datetime_local = acq_datetime + as.duration(solar_offset * 60 * 60),
                local_doy = lubridate::yday(acq_datetime_local),
                local_hour_decmin = ((acq_hour) + (acq_min / 60) + solar_offset + 24) %% 24,
                local_solar_hour_decmin_round = round(local_hour_decmin),
                local_solar_hour_decmin_round0.5 = round(local_hour_decmin * 2) / 2,
                h = (local_hour_decmin - 12) * 15 * pi / 180,
                phi = latitude * pi / 180,
                delta = -asin(0.39779 * cos(pi / 180 * (0.98565 * (local_doy + 10) + 360 / pi * 0.0167 * sin(pi / 180 * (0.98565 * (local_doy - 2)))))),
                solar_elev_ang = (asin(sin(phi)*sin(delta) + cos(phi)*cos(delta)*cos(h))) * 180 / pi,
                daynight = ifelse(solar_elev_ang > 0, yes = "day", no = "night")) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  sf::st_transform(crs = modis_crs)%>%
  dplyr::mutate(daynight = factor(daynight, levels = c("night", "day")))

snowy_modis_detections <- snowy_modis %>%
    mutate(acq_datetime = round_date(ymd_hms(acq_datetime), unit = "hour")) %>%
    group_by(acq_datetime) %>%
    summarise(value = n(),
              daynight= first(daynight)) %>%
    ungroup() %>%
    mutate(variable = "MODIS Detections",
           lty = 0,
           label_y = max(value)*0.85) %>%
    dplyr::select(date = acq_datetime, variable, value,lty, label_y, daynight) %>%
    st_set_geometry(NULL) %>%
    filter(date>daterange_s[1], date < daterange_s[2])
  
snowy_n <- snowy_modis %>%
  st_set_geometry(NULL)%>%
  mutate(acq_datetime = round_date(ymd_hms(acq_datetime), unit = "hour")) %>%
  group_by(acq_datetime) %>%
  summarise(value = n(),
            daynight = first(daynight)) %>%
  ungroup() %>%
  mutate(variable = "Detections",
         lty = 0,
         lty_main = 0) %>%
  mutate(label_y = max(value)*0.85) %>%
  dplyr::select(datetime = acq_datetime, variable, value,lty, label_y, daynight, lty_main) 

snowy_clim <- read_csv("fig_3_data/snowyave.csv",col_names = F) %>%
  dplyr::rename(day=X1, hour=X2, value = X3)%>% 
  mutate(year = ifelse(day>300,2019,2020),
         month = ifelse(day>300,"12","01"),
         value = value/10) %>%
  mutate(day = replace(day, day > 300, day-334),
         day = str_pad(day, 2, "left", "0"),
         date = paste0(year, "-",month,"-",day) %>% as.Date(),
         datetime = lubridate::ymd_hms(paste0(date," ", hour, ":00:00")),
         variable = "VPD (kPa)",
         lty=2,
         lty_main = 1) %>%
  mutate(label_y = max(value)*0.85) %>%
  filter(date > as.Date("2019-12-28"),
         date < as.Date("2020-01-6"))%>%
  dplyr::mutate(latitude = first(snowy$latitude),
                longitude = first(snowy$longitude),
                acq_datetime = datetime,
                acq_hour = hour(acq_datetime),
                acq_min = minute(acq_datetime),
                acq_year = year(acq_datetime),
                acq_month = month(acq_datetime),
                acq_day = day(acq_datetime),
                solar_offset = longitude / 15,
                hemisphere = ifelse(latitude >= 0, yes = "Northern hemisphere", no = "Southern hemisphere"),
                acq_datetime_local = acq_datetime + as.duration(solar_offset * 60 * 60),
                local_doy = lubridate::yday(acq_datetime_local),
                local_hour_decmin = ((acq_hour) + (acq_min / 60) + solar_offset + 24) %% 24,
                local_solar_hour_decmin_round = round(local_hour_decmin),
                local_solar_hour_decmin_round0.5 = round(local_hour_decmin * 2) / 2,
                h = (local_hour_decmin - 12) * 15 * pi / 180,
                phi = latitude * pi / 180,
                delta = -asin(0.39779 * cos(pi / 180 * (0.98565 * (local_doy + 10) + 360 / pi * 0.0167 * sin(pi / 180 * (0.98565 * (local_doy - 2)))))),
                solar_elev_ang = (asin(sin(phi)*sin(delta) + cos(phi)*cos(delta)*cos(h))) * 180 / pi,
                daynight = ifelse(solar_elev_ang > 0, yes = "day", no = "night")) %>%
  dplyr::select(datetime, variable, value,lty, label_y,daynight, lty_main) %>%
  rbind(snowy_n)

lc_s <- snowy_modis %>%
  mutate(lc=raster::extract(x=raster(snowy_lc_file), y=.)) %>%
  mutate(lc_name = lut_lc[lc]) %>%
  group_by(lc_name) %>%
  summarise(percent_lc = round((n()/nrow(.))*100)) %>%
  ungroup() %>%
  st_set_geometry(NULL)

stats_s <- snowy_modis %>%
  st_set_geometry(NULL) %>%
  group_by(daynight) %>%
  summarise(percent_dn = round((n()/nrow(.))*100,2)) %>%
  ungroup() 

if(!file.exists("fig_3_data/snowy_area.Rda")){
snowy_area <- st_read("fig_3_data/background/world_borders/ne_50m_admin_0_countries.shp") %>%
  filter(NAME_EN == "Australia") %>%
  st_crop(snowy %>% st_buffer(1))
  save(snowy_area, file = "fig_3_data/snowy_area.Rda")}else{
    load("fig_3_data/snowy_area.Rda")
  }


bbox_s <- snowy %>%
  st_bbox()%>%
  as.numeric()


australia <- st_read("fig_3_data/background/world_borders/ne_50m_admin_0_countries.shp") %>%
  filter(NAME_EN == "Australia")


if(!file.exists("fig_3_data/fishnet.RDS")){
  fishnet <- st_bbox(snowy)%>% 
    st_as_sfc() %>%
    st_buffer(0.5) %>%
    st_make_grid(cellsize = 0.01)%>%
    as("Spatial")%>%
    exactextractr::exact_extract(x=raster(snowy_lc_file), y=., 
                   fun = "mode") 
  saveRDS(fishnet, "fig_3_data/fishnet.RDS")
  }else{
  fishnet <- readRDS("fig_3_data/fishnet.RDS")
}

if(!file.exists("fig_3_data/fishnet_lc.RDS")){
  fishnet_lc <- st_bbox(snowy) %>%
    st_as_sfc() %>%
    st_buffer(0.5) %>%
    st_make_grid(cellsize = 0.01) %>%
    st_as_sf() %>%
    mutate(lc= fishnet,
           classes = lut_lc[lc])
  saveRDS(fishnet_lc, "fig_3_data/fishnet_lc.RDS")
}else{
    fishnet_lc<-readRDS("fig_3_data/fishnet_lc.RDS")
  }


locator_box <- st_bbox(snowy_modis) %>% st_as_sfc()

# tubbs ==========================================================

tubbs_viirs <- st_read("fig_3_data/tubbs.gpkg") %>%
  mutate(daynight = factor(daynight, levels = c("night", "day")))
daterange_t <- tubbs_viirs$acq_date %>% as.Date() %>% range()

tubbs_modis <- read_csv("fig_3_data/tubbs_modis_afd.csv")%>%
  dplyr::mutate(acq_hour = as.numeric(substr(acq_time, start = 1, stop = 2)),
                acq_min = as.numeric(substr(acq_time, start = 3, stop = 4)),
                acq_datetime = ymd_hm(paste0(acq_date, " ", acq_hour, ":", acq_min)),
                acq_year = year(acq_datetime),
                acq_month = month(acq_datetime),
                acq_day = day(acq_datetime),
                solar_offset = longitude / 15,
                hemisphere = ifelse(latitude >= 0, yes = "Northern hemisphere", no = "Southern hemisphere"),
                acq_datetime_local = acq_datetime + as.duration(solar_offset * 60 * 60),
                local_doy = lubridate::yday(acq_datetime_local),
                local_hour_decmin = ((acq_hour) + (acq_min / 60) + solar_offset + 24) %% 24,
                local_solar_hour_decmin_round = round(local_hour_decmin),
                local_solar_hour_decmin_round0.5 = round(local_hour_decmin * 2) / 2,
                h = (local_hour_decmin - 12) * 15 * pi / 180,
                phi = latitude * pi / 180,
                delta = -asin(0.39779 * cos(pi / 180 * (0.98565 * (local_doy + 10) + 360 / pi * 0.0167 * sin(pi / 180 * (0.98565 * (local_doy - 2)))))),
                solar_elev_ang = (asin(sin(phi)*sin(delta) + cos(phi)*cos(delta)*cos(h))) * 180 / pi,
                daynight = ifelse(solar_elev_ang > 0, yes = "day", no = "night")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = modis_crs)%>%
  mutate(daynight = factor(daynight, levels = c("night", "day")))

tubbs_modis_detections<- tubbs_modis %>%
  mutate(acq_datetime = round_date(ymd_hms(acq_datetime), unit = "hour")) %>%
  group_by(acq_datetime) %>%
  summarise(value = n(),
            daynight= first(daynight)) %>%
  ungroup() %>%
  mutate(name = "MODIS Detections",
         lty = 0,
         label_y = max(value)*0.85) %>%
  dplyr::select(datetime_utc = acq_datetime, name, value,lty, label_y, daynight) %>%
  st_set_geometry(NULL) %>%
  filter(datetime_utc>daterange_t[1], datetime_utc < daterange_t[2])

tubbs_fired_p <- st_read("fig_3_data/tubbs_fired.gpkg")

lct <- tubbs_viirs %>%
  mutate(lc=raster::extract(x=raster(tubbs_lc_file), y=.)) %>%
  mutate(lc_name = lut_lc[lc]) %>%
  group_by(lc_name) %>%
  summarise(percent_lc = round((n()/nrow(.))*100)) %>%
  ungroup() %>%
  st_set_geometry(NULL)

statst <- tubbs_modis %>%
  st_set_geometry(NULL) %>%
  group_by(daynight) %>%
  summarise(percent_dn = round((n()/nrow(.))*100,2)) %>%
  ungroup() 


tubbs_area <- st_read("fig_3_data/CUS/CUS.shp") %>%
  st_transform(crs=st_crs(tubbs_viirs)) %>%
  st_crop(tubbs_viirs %>% st_buffer(1))

usa <- st_read("fig_3_data/CUS/CUS.shp") %>%
  st_transform(crs=st_crs(tubbs_viirs)) %>%
  filter(STUSPS == "CA")

bbox <- st_bbox(tubbs_viirs)%>% as.numeric()

locator_box <- st_bbox(tubbs_area) %>% st_as_sfc()

if(!file.exists("fig_3_data/fishnett.RDS")){
  fishnet_t <- st_bbox(tubbs_viirs)%>% 
    st_as_sfc() %>%
    st_buffer(0.5) %>%
    st_make_grid(cellsize = 0.01)%>%
    as("Spatial")%>%
    raster::extract(x=raster(tubbs_lc_file), y=., 
                    fun = function(x,...) getmode(x), 
                    method="simple") 
  saveRDS(fishnet, "fig_3_data/fishnett.RDS")
}else{
  fishnet_t <- readRDS("fig_3_data/fishnett.RDS")
}

if(!file.exists("fig_3_data/fishnet_lct.RDS")){
  fishnet_lc_t <- st_bbox(tubbs_viirs) %>%
    st_as_sfc() %>%
    st_buffer(0.5) %>%
    st_make_grid(cellsize = 0.01) %>%
    st_as_sf() %>%
    mutate(lc= fishnet,
           classes = lut_lc[lc])
  saveRDS(fishnet_lc_t, "fig_3_data/fishnet_lct.RDS")
}else{
  fishnet_lc_t<-readRDS("fig_3_data/fishnet_lct.RDS")
}


# snowy plots ==================================================================
if(!file.exists("fig_3_data/fishnet_snowwwy.Rda")){
lc_snowwwy <- fishnet_lc %>%
  group_by(classes) %>%
  summarise() %>%
  ungroup()
  save(lc_snowwwy, file="fig_3_data/fishnet_snowwwy.Rda")}else{
    load("fig_3_data/fishnet_snowwwy.Rda")}

main_plot_s <- ggplot() +
  geom_sf(data = lc_snowwwy, 
          aes(fill=lut_lc_simple[classes]),
          alpha=0.25,
          color = "transparent")+
  scale_fill_manual(values = lut_colors_simple, name = "Landcover Classes")+
  geom_sf(data = snowy_modis, 
          aes(color=daynight), show.legend = "point", size=0.5) +
  scale_color_manual(values = daynight_cols)+
  xlim(c(bbox_s[c(1,3)])) +
  ylim(c(bbox_s[c(2,4)])) + 
  ggsn::scalebar(data = snowy, location = "topleft", model = "WGS84",
                 dist = 30, dist_unit = "km",transform = TRUE,
                 st.size = 3, st.dist = 0.05) +
  ggtitle(paste("   Snowy Complex. December 29, 2019 - January 5, 2020")) +
  guides(fill="none", color="none")+
  theme(panel.border = element_rect(color="black", fill=NA),
        plot.title = element_text(hjust = 1),
        panel.background = element_rect(fill = "transparent", color = "black", size=1),
        text = element_text(size = 7))

locator_plot_s <- ggplot(australia) +  
  theme(panel.border = element_rect(color="black", fill=NA),
        plot.background = element_rect(fill="white"))+
  geom_sf(fill="white", color="grey") +
  geom_point(x= 149.6,y=-37.5, size=2, color = "black", shape=8) +
  ylim(c(-43, -11))+
  xlim(c(114,153))

inset_s <- ggplot(snowy_clim %>% 
                    filter(variable == "VPD (kPa)"), 
                  aes(x=datetime, y=value)) +
  geom_bar(stat = "identity", aes(fill = daynight,color = daynight,y=5), show.legend=F) +
  scale_fill_manual(values = shade_cols)+
  new_scale("fill")+
  scale_color_manual(values = shade_cols)+
  new_scale("color")+
  geom_line(color = "grey30", lwd=0.35) +
  geom_bar(data = snowy_modis_detections,stat = "identity", width = 20000,
           aes(x=date, y=value, fill=daynight))+
  geom_abline(aes(slope = 0,intercept = 0.6), lty=2)+
  scale_linetype_manual(values = c(0,2))+
  scale_y_continuous(position = "right", labels = label_number_si(), expand = c(0.02, 0.02))+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d", expand = c(0.02, 0.02))+
  facet_wrap(~variable, scales = "free_y", 
             nrow = 1#, strip.position = "left"
             ) +
  scale_fill_manual(values = daynight_cols)+
  xlab("Date") +
  theme_classic() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent", color = "black", size=1),
        panel.spacing.y = unit(4, "mm"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 7))

snowy_cow <- ggdraw() +
  draw_plot(main_plot_s) +
  draw_plot(locator_plot_s, x=0.8337,y=.670,width=.2,height=.2) +
  draw_plot_label("a", x=0, y=.96, size=8, fontface = "bold")

# tubbs plots ==================================================================

if(!file.exists("fig_3_data/lc_tubbbs.Rda")){
  lc_tubbbs <- fishnet_lc_t %>%
    group_by(classes) %>%
    summarise() %>%
    ungroup()
  save(lc_tubbbs, file = "fig_3_data/lc_tubbbs.Rda")
  }else{load("fig_3_data/lc_tubbbs.Rda")}

main_plot_t <- ggplot() +
  geom_sf(data = lc_tubbbs,
          aes(fill=lut_lc_simple[classes]),
          alpha = 0.25,
          color = "transparent"
          )+
  geom_sf(data = tubbs_fired_p, fill = "transparent",lwd=.5) +
  geom_sf(data = tubbs_modis, 
          aes(color=daynight), size=3,show.legend = "point") +
  scale_alpha_manual(values = c(0.9,0.5))+
  scale_color_manual(values = daynight_cols)+
  scale_fill_manual(values = lut_colors_simple)+
  xlim(c(bbox[c(1,3)])) +
  ylim(c(bbox[c(2,4)])) +
  ggsn::scalebar(data = tubbs_viirs, location = "topleft", model = "WGS84",
                 st.dist = 0.04, st.size= 3,
                 dist = 3, dist_unit = "km",transform = TRUE) +
  guides(fill = "none") +
  ggtitle("Tubbs Fire. October 9-15, 2017") +
  theme(legend.position = "none",
        legend.justification = c(1,0),
        legend.title = element_blank(),
        plot.title = element_text(hjust=1),
        text = element_text(size = 7),
        panel.background = element_rect(fill = "transparent", color = "black", size=1),
        panel.border = element_rect(color="black", fill=NA))

inset_t_data <- read_csv("fig_3_data/tubbs-hourly.csv") %>% 
  mutate(lty = ifelse(name == "VPD (kPa)",2,0),
         name = str_replace_all(name, "Active Fire detections", "Detections")) %>%
  group_by(name) %>%
  mutate(label_y = max(value)*0.85,
         daynight = NA,
         lty_main = 1,
         shade_y = max(value)) %>%
  ungroup()%>%
  dplyr::mutate(latitude = 38,
                longitude = -122,
                acq_hour = hour(datetime_utc),
                acq_min = lubridate::minute(datetime_utc),
                acq_datetime = datetime_utc,
                acq_year = year(acq_datetime),
                acq_month = month(acq_datetime),
                acq_day = day(acq_datetime),
                solar_offset = longitude / 15,
                hemisphere = ifelse(latitude >= 0, yes = "Northern hemisphere", no = "Southern hemisphere"),
                acq_datetime_local = acq_datetime + as.duration(solar_offset * 60 * 60),
                local_doy = lubridate::yday(acq_datetime_local),
                local_hour_decmin = ((acq_hour) + (acq_min / 60) + solar_offset + 24) %% 24,
                local_solar_hour_decmin_round = round(local_hour_decmin),
                local_solar_hour_decmin_round0.5 = round(local_hour_decmin * 2) / 2,
                h = (local_hour_decmin - 12) * 15 * pi / 180,
                phi = latitude * pi / 180,
                delta = -asin(0.39779 * cos(pi / 180 * (0.98565 * (local_doy + 10) + 360 / pi * 0.0167 * sin(pi / 180 * (0.98565 * (local_doy - 2)))))),
                solar_elev_ang = (asin(sin(phi)*sin(delta) + cos(phi)*cos(delta)*cos(h))) * 180 / pi,
                daynight = ifelse(solar_elev_ang > 0, yes = "day", no = "night")) %>%
  dplyr::select(datetime_utc, name, value, lty, label_y, daynight, lty_main, shade_y)%>%
  rbind(tubbs_modis_detections %>% mutate(lty_main = 0, shade_y=0)) %>%
  filter(datetime_utc > as.Date("2017-10-08") &
           datetime_utc < as.Date("2017-10-17"))

inset_t<- ggplot(inset_t_data, aes(x=datetime_utc, y=value)) +
  geom_bar(stat = "identity", aes(fill = daynight,y=shade_y, color = daynight),
           show.legend=F) +
  scale_fill_manual(values = shade_cols)+
  scale_color_manual(values = shade_cols)+
  new_scale("fill")+
  new_scale("color")+
    geom_line(color = "grey30", aes(lty = as.factor(lty_main)), lwd=0.35) +
    geom_bar(data = tubbs_modis_detections,
             stat="identity",width = 25000,
             aes(y = value, fill=daynight), 
             color = "transparent")+
    xlab("Date") +
    scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d", 
                     expand = c(0.02, 0.02) )+
    geom_hline(aes(yintercept = 0.34, lty=as.factor(lty)))+
    scale_y_continuous(position = "right", 
                       labels = label_number_si(), expand = c(0.02, 0.02))+
    scale_linetype_manual(values=c(0,1,2)) +
    scale_fill_manual(values =daynight_cols,)+
    facet_wrap(~name, ncol = 1, scales = "free_y", 
               nrow = 3, strip.position = "top") +
    theme_classic() +
    theme(legend.position = "none",
          panel.background = element_rect(color = "black", 
                                          fill = "transparent",
                                          size = 1),
          plot.background = element_rect(color="transparent", fill="transparent"),
          text = element_text(size = 7),
          axis.title = element_blank(),
          axis.ticks = element_blank())

locator_plot_t <- ggplot(usa) +  
  theme(panel.border = element_rect(color="black", fill=NA),
        plot.background = element_rect(fill="white"))+
  geom_sf(fill="white", color="grey") +
  geom_sf(data=st_centroid(locator_box), color = "black", size=2, shape=8)

tubbs_cow<-ggdraw() +
  draw_plot(main_plot_t) +
  draw_plot(locator_plot_t, .75,.10,.2,.2) +
  draw_plot_label("b", x=0, y=0.96, size=8, fontface = "bold")


# insets and legends =====================================================
leg_dn <- get_legend(ggplot()+
                    geom_bar(data = snowy_n,stat = "identity", width = 20000,
                             aes(x=datetime, y=value, fill=daynight))+
                      theme(text = element_text(size = 7))+
                    scale_fill_manual(values=daynight_cols,
                                      name= "Modis Active\nFire Detections"))

leg_lc <- get_legend(fishnet_lc %>%
                       st_set_geometry(NULL) %>%
                       group_by(classes) %>%
                       summarise(x = rnorm(1)) %>%
                       ungroup() %>%
                       na.omit() %>%
                       mutate(simple_classes = lut_lc_simple[classes]) %>%
                       ggplot() +
                        geom_raster(aes(fill=simple_classes, x=x,y=x), alpha=0.25) +
                     scale_fill_manual(values = lut_colors_simple)+
                     guides(fill=guide_legend(ncol=2, title = "Landcover Classes"))+
                       theme(text = element_text(size = 7)))

insets_ls <- ggarrange(leg_dn, inset_s,
                       nrow=2, ncol=1,heights = c(1,4),
                       label.x = 0.08,
                       label.y = 0.95)

insets_ts <- ggarrange(inset_t, insets_ls, 
                       nrow=1, ncol=2,
                       labels = c( "c. Tubbs", ""),
                       label.x = c(.85, .39), 
                       label.y = .97, hjust="right", vjust="top")

# final plot ====================
finalfig <- ggdraw(xlim = c(0, 3.503937), ylim = c(2,9.724409)) +
  draw_plot(snowy_cow, x = 0, y = 8.2, width =3.5, height = 1.6) +
  draw_plot(inset_s, x = 0, y= 7.25, width = 3.5, height = 1) +
  draw_plot(tubbs_cow, x = 0, y = 4.5, width = 2, height = 2.77) +
  draw_plot(inset_t, x = 2, y = 4.4, width = 1.5, height = 2.699) +
  draw_plot(leg_dn, x=0, y=3.7) +
  draw_plot(leg_lc,x=1.2, y=3.15, width = 2, height = 2)

ggsave(finalfig, 
       filename = "figs/Fig3_case_studies.pdf",
       bg = "white",
       units ="mm",
       dpi = 600,
       height = 247, width =89)
ggsave(finalfig, 
       filename = "figs/Fig3_case_studies.png",
       bg = "white",
       units ="mm",
       dpi = 300,
       height = 247, width =89)

