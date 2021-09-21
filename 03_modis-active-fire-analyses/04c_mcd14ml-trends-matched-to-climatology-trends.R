
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(terra)

dir.create("figs", showWarnings = FALSE)

# get burnable landcovers
lc_lookup_burnable <- read.csv("data/out/zero-goes-af-vpd-thresholds-with-landcover-codes.csv")

lc_area <-
  read.csv("data/out/area-per-lc.csv") %>%
  dplyr::right_join(lc_lookup_burnable, by = c(lc = "koppen_modis_code")) %>%
  dplyr::select(lc, area_km2, koppen_orig_modis_name)

# get the area and landcover of each pixel
pixel_area <-
  data.table::fread("data/out/area-per-lc-pixel.csv")

# get climatology trends
# note the data ending in "2003" means the trend is for the period of 2003 to 2020 (the same as the MODIS period of record)
# other trends are from 1979 to 2020
climatology_trend <- 
  setNames(terra::rotate(terra::rast("data/out/climatology/nighthours_trend.nc")), "nighthours_trend") %>% 
  c(setNames(terra::rotate(terra::rast("data/out/climatology/nighthourstrend20032020.nc")), "nighthours_trend20032020")) %>% 
  c(setNames(terra::rotate(terra::rast("data/out/climatology/nighthours_climo19912020.nc")), "nighthours")) %>% 
  c(setNames(terra::rotate(terra::rast("data/out/climatology/nights_trend.nc")), "nights_trend")) %>% 
  # c(setNames(terra::rotate(terra::rast("data/out/climatology/nights_trend2003.nc")), "nights_trend2003")) %>%  # commented out because we don't have a refresh on this
  c(terra::rotate(terra::rast("data/out/climatology/nights_climo19912020.nc"))) %>% 
  as.data.frame(xy = TRUE) %>% 
  dplyr::rename(x_lc = "x", y_lc = "y") %>% 
  as.data.table() %>% 
  mutate(nighthours_rel_trend = nighthours_trend / nighthours,
         nights_rel_trend = nights_trend / nights,
         nighthours_rel_trend2003 = nighthours_trend20032020 / nighthours)
         # nights_rel_trend2003 = nights_trend2003 / nights) # we don't have a refresh on the nights trend since 2003

# join the climatology trend data with the pixel area and landcover data
climatology_trend <- pixel_area[climatology_trend, on = c("x_lc", "y_lc")]
# climatology_trend[, big_trend := ifelse(nighthours_trend >= 0, yes = "increase in flammable night hours", no = "decrease in flammable night hours")]
climatology_trend[, big_trend := ifelse(nighthours_trend20032020 >= 0, yes = "increase in flammable night hours", no = "decrease in flammable night hours")]

# add in the koppen class and modis landcover as a new column
climatology_trend[, `:=`(koppen = substr(x = lc, start = 1, stop = 1),
                         modis = substr(x = lc, start = 2, stop = 3))]

# subset out pixels in non-burnable landcovers [this step is redundant, as only burnable pixels are included in climatology trends data files]
climatology_trend <- climatology_trend[lc %in% lc_lookup_burnable$koppen_modis_code, ]

# Area of positive/negative trends in flammable nighttime hours in global aggregation
big_trend_areas <- climatology_trend[, .(area_Mkm2 = sum(area_m2) / 1e12), by = .(big_trend)]

# Area of positive/negative trends in flammable nighttime hours by koppen class
big_trend_areas_koppen <- climatology_trend[, .(area_Mkm2 = sum(area_m2) / 1e12), by = .(big_trend, koppen)]

# 21.1% of pixels show a decrease in number of flammable nighttime hours
ecdf(climatology_trend$nighthours_trend)(0)
# 20.2% of pixels show a relative decrease in flammable nighttime hours (trend in nighttime flammable hours / climatological avg num. of nighttime flammable hours) [that is, excluding zeroes]
ecdf(climatology_trend$nighthours_rel_trend)(0)
# about 50% of pixels (49.5) show a relative trend of flammable night hours of less than 0.25
ecdf(climatology_trend$nighthours_rel_trend)(0.25)

ggplot(climatology_trend, aes(x = nighthours_rel_trend)) + geom_histogram()
ggplot(climatology_trend, aes(x = nighthours_trend)) + geom_histogram()

# read the active fire data from MODIS that has already been aggregated by month and pixel
afd <- data.table::fread(input = "data/out/mcd14ml_n-frp_month-lc-xy-daynight-summary.csv")

# Account for sampling effort (MODIS overpass frequency) in the detections
afd[, n_per_op := n / op]

# Subset out any instances where there weren't any overpasses (at least for how we've determined overpasses)
# but with some detections
afd <- afd[!(op == 0 & n > 0), ]

# Subset out pixels in the non-burnable landcovers
afd <- afd[lc %in% lc_lookup_burnable$koppen_modis_code, ]

# Join active fire data with climatology trends data
afd <- climatology_trend[afd, on = c("cell_id_lc", "x_lc", "y_lc", "lc")]

#### Begin aggregations ####
## Global
afd_global_summary <-
  afd[, .(total_n_per_op = sum(n_per_op), total_n = sum(n), n_per_op_per_px = mean(n_per_op),
          total_frp = sum(sum_frp),
          n_px = .N,
          n_op = sum(op)), 
      by = .(acq_year, acq_month, dn_detect, big_trend)] %>% 
  dplyr::mutate(mean_frp_per_detection = total_frp / total_n,
                mean_frp_per_px = total_frp / n_px) %>% 
  dplyr::mutate(year_month = lubridate::ymd(paste0(acq_year, "-", acq_month, "-", "15"))) %>% 
  tidyr::complete(year_month, nesting(dn_detect), fill = list(n_per_op = 0, frp_per_op = 0)) %>%
  dplyr::filter(acq_year >= 2003 & !is.na(big_trend)) %>% 
  dplyr::left_join(big_trend_areas, by = "big_trend") %>% 
  dplyr::mutate(n_per_op_per_Mkm2 = (total_n_per_op / area_Mkm2))

# wide version to get proportion night burning
afd_global_summary_wide <-
  afd_global_summary %>% 
  tidyr::pivot_wider(names_from = "dn_detect", values_from = "total_n_per_op", id_cols = c("year_month", "acq_year", "acq_month", "big_trend")) %>% 
  dplyr::mutate(prop_n_night = night  / (night + day))

### begin aggregations by Koppen class ###

afd_koppen_summary <-
  afd[, .(total_n_per_op = sum(n_per_op), total_n = sum(n), n_per_op_per_px = mean(n_per_op),
          total_frp = sum(sum_frp),
          n_px = .N,
          n_op = sum(op)), 
      by = .(acq_year, acq_month, koppen, dn_detect, big_trend)] %>% 
  dplyr::mutate(mean_frp_per_detection = total_frp / total_n,
                mean_frp_per_px = total_frp / n_px) %>% 
  dplyr::mutate(year_month = lubridate::ymd(paste0(acq_year, "-", acq_month, "-", "15"))) %>% 
  tidyr::complete(year_month, nesting(koppen, dn_detect), fill = list(n_per_op = 0, frp_per_op = 0)) %>% 
  dplyr::filter(acq_year >= 2003 & !is.na(big_trend)) %>% 
  dplyr::left_join(big_trend_areas_koppen, by = c("big_trend", "koppen")) %>% 
  dplyr::mutate(n_per_op_per_Mkm2 = (total_n_per_op / area_Mkm2)) 
  
# wide version to get proportion nighttime burning
afd_koppen_summary_wide <-
  afd_koppen_summary %>% 
  tidyr::pivot_wider(names_from = "dn_detect", values_from = "total_n_per_op", id_cols = c("koppen", "year_month", "acq_year", "acq_month", "big_trend")) %>% 
  dplyr::mutate(prop_n_night = night  / (night + day))


### Write to disk and upload
write.csv(afd_global_summary, "data/out/mcd14ml-global-trend-matched-to-climatology-by-month.csv")
system2(command = "aws", args = "s3 cp data/out/mcd14ml-global-trend-matched-to-climatology-by-month.csv s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-global-trend-matched-to-climatology-by-month.csv")

write.csv(afd_global_summary_wide, "data/out/mcd14ml-global-trend-matched-to-climatology-by-month_wide.csv")
system2(command = "aws", args = "s3 cp data/out/mcd14ml-global-trend-matched-to-climatology-by-month_wide.csv s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-global-trend-matched-to-climatology-by-month_wide.csv")


write.csv(afd_koppen_summary, "data/out/mcd14ml-trend-matched-to-climatology-by-month-koppen.csv")
system2(command = "aws", args = "s3 cp data/out/mcd14ml-trend-matched-to-climatology-by-month-koppen.csv s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-trend-matched-to-climatology-by-month-koppen.csv")

write.csv(afd_koppen_summary_wide, "data/out/mcd14ml-trend-matched-to-climatology-by-month-koppen_wide.csv")
system2(command = "aws", args = "s3 cp data/out/mcd14ml-trend-matched-to-climatology-by-month-koppen_wide.csv s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-trend-matched-to-climatology-by-month-koppen_wide.csv")

### Trends since 2003

write.csv(afd_global_summary, "data/out/mcd14ml-global-trend-matched-to-climatology2003-by-month.csv")
system2(command = "aws", args = "s3 cp data/out/mcd14ml-global-trend-matched-to-climatology2003-by-month.csv s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-global-trend-matched-to-climatology2003-by-month.csv")

write.csv(afd_global_summary_wide, "data/out/mcd14ml-global-trend-matched-to-climatology2003-by-month_wide.csv")
system2(command = "aws", args = "s3 cp data/out/mcd14ml-global-trend-matched-to-climatology2003-by-month_wide.csv s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-global-trend-matched-to-climatology2003-by-month_wide.csv")


write.csv(afd_koppen_summary, "data/out/mcd14ml-trend-matched-to-climatology2003-by-month-koppen.csv")
system2(command = "aws", args = "s3 cp data/out/mcd14ml-trend-matched-to-climatology2003-by-month-koppen.csv s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-trend-matched-to-climatology2003-by-month-koppen.csv")

write.csv(afd_koppen_summary_wide, "data/out/mcd14ml-trend-matched-to-climatology2003-by-month-koppen_wide.csv")
system2(command = "aws", args = "s3 cp data/out/mcd14ml-trend-matched-to-climatology2003-by-month-koppen_wide.csv s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-trend-matched-to-climatology2003-by-month-koppen_wide.csv")

### How to download files
# Trends since 1979
system2(command = "aws", args = "s3 cp s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-global-trend-matched-to-climatology-by-month.csv data/out/mcd14ml-global-trend-matched-to-climatology-by-month.csv")
system2(command = "aws", args = "s3 cp s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-global-trend-matched-to-climatology-by-month_wide.csv data/out/mcd14ml-global-trend-matched-to-climatology-by-month_wide.csv")
system2(command = "aws", args = "s3 cp s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-trend-matched-to-climatology-by-month-koppen.csv data/out/mcd14ml-trend-matched-to-climatology-by-month-koppen.csv")
system2(command = "aws", args = "s3 cp s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-trend-matched-to-climatology-by-month-koppen_wide.csv data/out/mcd14ml-trend-matched-to-climatology-by-month-koppen_wide.csv")

# Trends since 2003
system2(command = "aws", args = "s3 cp s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-global-trend-matched-to-climatology2003-by-month.csv data/out/mcd14ml-global-trend-matched-to-climatology2003-by-month.csv")
system2(command = "aws", args = "s3 cp s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-global-trend-matched-to-climatology2003-by-month_wide.csv data/out/mcd14ml-global-trend-matched-to-climatology2003-by-month_wide.csv")
system2(command = "aws", args = "s3 cp s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-trend-matched-to-climatology2003-by-month-koppen.csv data/out/mcd14ml-trend-matched-to-climatology2003-by-month-koppen.csv")
system2(command = "aws", args = "s3 cp s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/data/out/mcd14ml-trend-matched-to-climatology2003-by-month-koppen_wide.csv data/out/mcd14ml-trend-matched-to-climatology2003-by-month-koppen_wide.csv")

afd_global_summary
afd_koppen_summary

### Figure drafts ###
global_n_gg <-
  ggplot(afd_global_summary, aes(x = year_month, y = n_per_op_per_Mkm2, color = dn_detect, lty = big_trend)) +
  geom_line() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Date (monthly increments)",
       y = "Number of detections per overpass per million square km",
       color = "Day/night",
       lty = "Climatology trend") +
  facet_wrap(facets = "dn_detect", scales = "free_y") +
  scale_color_manual(values = c("red", "#2166ac"))

global_n_gg

global_frp_gg <-
  ggplot(afd_global_summary, aes(x = year_month, y = mean_frp_per_detection, color = dn_detect, lty = big_trend)) +
  geom_line() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Date (monthly increments)",
       y = "Mean FRP per detection (MW)",
       color = "Day/night",
       lty = "Climatology trend") +
  facet_wrap(facets = "dn_detect", scales = "free_y") +
  scale_color_manual(values = c("red", "#2166ac"))

global_frp_gg

global_frp_gg_linear <-
  ggplot(afd_global_summary, aes(x = year_month, y = mean_frp_per_detection, color = dn_detect, lty = big_trend)) +
  geom_line() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Date (monthly increments)",
       y = "Mean FRP per detection (MW)",
       color = "Day/night",
       lty = "Climatology trend") +
  facet_wrap(facets = "dn_detect", scales = "free_y") +
  scale_color_manual(values = c("red", "#2166ac"))

global_frp_gg_linear



global_prop_n_gg <-
  ggplot(afd_global_summary_wide, aes(x = year_month, y = prop_n_night, lty = big_trend)) +
  geom_line() +
  geom_smooth() +
  labs(x = "Date (monthly increments)",
       y = "Proportion of global MODIS detections at night") +
  theme_bw()

global_prop_n_gg

## Koppen figures

koppen_n_gg <-
  ggplot(afd_koppen_summary, aes(x = year_month, y = n_per_op_per_Mkm2, color = dn_detect, lty = big_trend)) +
  geom_line() + 
  scale_color_manual(values = c("#b2182b", "#2166ac")) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Date (monthly increments)",
       y = "Number of detections per overpass per million square km",
       color = "Day/night") +
  facet_grid(rows = vars(dn_detect), cols = vars(koppen), scales = "free_y")

koppen_n_gg

koppen_frp_gg <-
  ggplot(afd_koppen_summary, aes(x = year_month, y = mean_frp_per_detection, color = dn_detect, lty = big_trend)) +
  geom_line() + 
  scale_color_manual(values = c("#b2182b", "#2166ac")) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Date (monthly increments)",
       y = "Mean FRP per detection (MW)",
       color = "Day/night") +
  facet_grid(rows = vars(dn_detect), cols = vars(koppen), scales = "free_y")
  
koppen_frp_gg
