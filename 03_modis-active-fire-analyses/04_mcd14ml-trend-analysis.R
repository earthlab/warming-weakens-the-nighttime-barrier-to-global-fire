# plot trends in MODIS active fire detections/FRP through time by landcover and day/night

library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(mgcv)

dir.create("figs", showWarnings = FALSE)

# lc_area_per_px <- read.csv("data/out/area-per-lc-pixel.csv")
# lc_lookup <- read.csv("data/out/koppen-modis-landcover-lookup-table.csv")
# lc <-
#   lc_area_per_px %>% 
#   dplyr::left_join(y = lc_lookup, by = c(lc = "koppen_modis_code")) %>% 
#   as.data.table()

lc_lookup_burnable <- read.csv("data/out/zero-goes-af-vpd-thresholds-with-landcover-codes.csv")

lc_area <- 
  read.csv("data/out/area-per-lc.csv") %>% 
  dplyr::right_join(lc_lookup_burnable, by = c(lc = "koppen_modis_code")) %>% 
  dplyr::select(lc, area_km2, koppen_orig_modis_name)

afd <- data.table::fread(input = "data/out/mcd14ml_n-frp_month-lc-xy-daynight-summary.csv")

afd[, n_per_op := n / op]

afd <- afd[!(op == 0 & n > 0), ]

# join active fire data with area information per cell and landcovers
# Only if you need the per-pixel area corrections (we didn't use these)
# afd <- lc[afd, on = c("cell_id_lc", "x_lc", "y_lc", "lc")]

### Summarize the global day/night detections and FRP across MODIS record
# on an annual + monthly basis
afd_global_summary_no_trend <-
  afd[acq_year > 2002, .(total_n_per_op = sum(n_per_op), total_n = sum(n),
                         total_frp = sum(sum_frp)), 
      by = .(dn_detect, acq_year, acq_month)] %>% 
  tidyr::pivot_wider(names_from = "dn_detect", values_from = starts_with("total")) %>% 
  mutate(prop_n_per_op_night = total_n_per_op_night / (total_n_per_op_day + total_n_per_op_night),
         prop_n_night = total_n_night / (total_n_night + total_n_day)) %>% 
  dplyr::select(prop_n_per_op_night, prop_n_night) %>% 
  summarize(mean_prop = mean(prop_n_per_op_night),
            sd_prop = sd(prop_n_per_op_night))

# Just annual basis
afd[acq_year > 2002, .(total_n_per_op = sum(n_per_op), total_n = sum(n),
                       total_frp = sum(sum_frp)), 
    by = .(dn_detect, acq_year)] %>% 
  tidyr::pivot_wider(names_from = "dn_detect", values_from = starts_with("total")) %>% 
  mutate(prop_n_per_op_night = total_n_per_op_night / (total_n_per_op_day + total_n_per_op_night),
         prop_n_night = total_n_night / (total_n_night + total_n_day)) %>% 
  dplyr::select(prop_n_per_op_night, prop_n_night) %>% 
  summarize(mean_prop = mean(prop_n_per_op_night),
            sd_prop = sd(prop_n_per_op_night))

# across the whole record (no standard deviation or range to speak of)
afd[acq_year > 2002, .(total_n_per_op = sum(n_per_op), total_n = sum(n),
                       total_frp = sum(sum_frp)), 
    by = .(dn_detect)] %>% 
  tidyr::pivot_wider(names_from = "dn_detect", values_from = starts_with("total")) %>% 
  mutate(prop_n_per_op_night = total_n_per_op_night / (total_n_per_op_day + total_n_per_op_night),
         prop_n_night = total_n_night / (total_n_night + total_n_day)) %>% 
  dplyr::select(prop_n_per_op_night, prop_n_night) %>% 
  summarize(mean_prop = mean(prop_n_per_op_night),
            sd_prop = sd(prop_n_per_op_night))

### begin global aggregations ###
afd_global_summary <-
  afd[, .(total_n_per_op = sum(n_per_op), total_n = sum(n), n_per_op_per_px = mean(n_per_op),
          total_frp = sum(sum_frp),
          n_px = .N,
          n_op = sum(op)), 
      by = .(acq_year, acq_month, dn_detect)] %>% 
  dplyr::mutate(mean_frp_per_detection = total_frp / total_n,
                mean_frp_per_px = total_frp / n_px) %>% 
  dplyr::mutate(year_month = lubridate::ymd(paste0(acq_year, "-", acq_month, "-", "15"))) %>% 
  tidyr::complete(year_month, nesting(dn_detect), fill = list(n_per_op = 0, frp_per_op = 0)) %>% 
  dplyr::mutate(n_per_op_per_Mkm2 = (total_n_per_op / sum(lc_area$area_km2)) * 1e6) %>% 
  dplyr::filter(acq_year >= 2003)

write.csv(afd_global_summary, "data/out/mcd14ml-global-trend-by-month.csv")

afd_global_summary_wide <-
  afd_global_summary %>% 
  tidyr::pivot_wider(names_from = "dn_detect", values_from = "total_n_per_op", id_cols = c("year_month", "acq_year", "acq_month")) %>% 
  dplyr::mutate(prop_n_night = night  / (night + day))

write.csv(afd_global_summary_wide, "data/out/mcd14ml-global-trend-by-month_wide.csv")

### end global aggregations ###

### begin aggregations by Koppen class ###
lc_area_koppen <-
  lc_area %>% 
  dplyr::mutate(koppen = substr(x = lc, start = 1, stop = 1),
                modis = substr(x = lc, start = 2, stop = 3)) %>% 
  group_by(koppen) %>% 
  dplyr::summarize(area_km2 = sum(area_km2))

afd[, `:=`(koppen = substr(x = lc, start = 1, stop = 1),
           modis = substr(x = lc, start = 2, stop = 3))]

afd_koppen_summary <-
  afd[, .(total_n_per_op = sum(n_per_op), total_n = sum(n), n_per_op_per_px = mean(n_per_op),
          total_frp = sum(sum_frp),
          n_px = .N,
          n_op = sum(op)), 
      by = .(acq_year, acq_month, koppen, dn_detect)] %>% 
  dplyr::left_join(lc_area_koppen) %>% 
  dplyr::filter(!is.na(area_km2)) %>% 
  dplyr::mutate(mean_frp_per_detection = total_frp / total_n,
                mean_frp_per_px = total_frp / n_px) %>% 
  dplyr::mutate(year_month = lubridate::ymd(paste0(acq_year, "-", acq_month, "-", "15"))) %>% 
  tidyr::complete(year_month, nesting(koppen, dn_detect), fill = list(n_per_op = 0, frp_per_op = 0)) %>% 
  dplyr::mutate(n_per_op_per_Mkm2 = (total_n_per_op / area_km2) * 1e6) %>% 
  dplyr::filter(acq_year >= 2003)

afd_koppen_summary

write.csv(afd_koppen_summary, "data/out/mcd14ml-trend-by-month-koppen.csv")

afd_koppen_summary_wide <-
  afd_koppen_summary %>% 
  tidyr::pivot_wider(names_from = "dn_detect", values_from = "total_n_per_op", id_cols = c("koppen", "year_month", "acq_year", "acq_month")) %>% 
  dplyr::mutate(prop_n_night = night  / (night + day))

write.csv(afd_koppen_summary_wide, "data/out/mcd14ml-trend-by-month-koppen_wide.csv")


### end aggregations by Koppen class ###

### begin aggregations by landcover (koppen + MODIS) ###

afd_summary <- 
  afd[, .(total_n_per_op = sum(n_per_op), total_n = sum(n), 
          n_per_op_per_px = mean(n_per_op),
          total_frp = sum(sum_frp), 
          n_px = .N,
          n_op = sum(op)), 
      by = .(acq_year, acq_month, dn_detect, lc)] %>% 
  left_join(lc_area) %>% 
  dplyr::filter(!is.na(area_km2)) %>% 
  dplyr::mutate(mean_frp_per_detection = total_frp / total_n,
                mean_frp_per_px = total_frp / n_px) %>% 
  dplyr::mutate(year_month = lubridate::ymd(paste0(acq_year, "-", acq_month, "-", "15"))) %>% 
  tidyr::complete(year_month, nesting(lc, dn_detect, koppen_orig_modis_name), fill = list(n_per_op = 0, frp_per_op = 0)) %>% 
  dplyr::mutate(n_per_op_per_Mkm2 = (total_n_per_op / area_km2) * 1e6) %>% 
  dplyr::filter(!is.na(koppen_orig_modis_name), acq_year >= 2003) %>% 
  dplyr::mutate(koppen = substr(x = lc, start = 1, stop = 1),
                modis = substr(x = lc, start = 2, stop = 3))

afd_summary

write.csv(afd_summary, "data/out/mcd14ml-trend-by-month-landcover.csv")

afd_landcover_summary_wide <-
  afd_summary %>% 
  tidyr::pivot_wider(names_from = "dn_detect", values_from = "total_n_per_op", id_cols = c("lc", "year_month", "acq_year", "acq_month")) %>% 
  dplyr::mutate(prop_n_night = night  / (night + day))

write.csv(afd_landcover_summary_wide, "data/out/mcd14ml-trend-by-month-landcover_wide.csv")


### end aggregations by landcover (Koppen + MODIS) ###

global_n_gg <-
  ggplot(afd_global_summary, aes(x = year_month, y = n_per_op_per_Mkm2, color = dn_detect)) +
  geom_line() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Date (monthly increments)",
       y = "Number of detections per overpass per million square km",
       color = "Day/night")

global_n_gg

ggsave(filename = "figs/mcd14ml_n-trend_global.png", plot = global_n_gg)

global_frp_gg <-
  ggplot(afd_global_summary, aes(x = year_month, y = mean_frp_per_detection, color = dn_detect)) +
  geom_line() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Date (monthly increments)",
       y = "Mean FRP per detection (MW)",
       color = "Day/night")

global_frp_gg

ggsave(filename = "figs/mcd14ml_frp-trend_global.png", plot = global_frp_gg)

global_prop_n_gg <-
  ggplot(afd_global_summary_wide, aes(x = year_month, y = prop_n_night)) +
  geom_line() +
  geom_smooth() +
  labs(x = "Date (monthly increments)",
       y = "Proportion of global MODIS detections at night") +
  theme_bw()

global_prop_n_gg

ggsave(filename = "figs/mcd14ml_prop-n-trend_global.png", plot = global_prop_n_gg)

# koppen 1 Tropical
tropical_n_gg <-
  ggplot(dplyr::filter(afd_summary, koppen == 1), aes(x = year_month, y = n_per_op_per_Mkm2, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw() +
  labs(x = "Date (monthly increments)",
       y = "Number of detections per overpass per million square km",
       color = "Day/night")

# koppen 2 Arid
arid_n_gg <-
  ggplot(dplyr::filter(afd_summary, koppen == 2), aes(x = year_month, y = n_per_op_per_Mkm2, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw() +
  labs(x = "Date (monthly increments)",
       y = "Number of detections per overpass per million square km",
       color = "Day/night")

# koppen 3 Temperate
temperate_n_gg <- 
  ggplot(dplyr::filter(afd_summary, koppen == 3), aes(x = year_month, y = n_per_op_per_Mkm2, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw() +
  labs(x = "Date (monthly increments)",
       y = "Number of detections per overpass per million square km",
       color = "Day/night")

# koppen 4 Cold
cold_n_gg <-
  ggplot(dplyr::filter(afd_summary, koppen == 4), aes(x = year_month, y = n_per_op_per_Mkm2, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw() +
  labs(x = "Date (monthly increments)",
       y = "Number of detections per overpass per million square km",
       color = "Day/night")

ggsave(filename = "figs/mcd14ml_n-trend_tropical.png", plot = tropical_n_gg)
ggsave(filename = "figs/mcd14ml_n-trend_arid.png", plot = arid_n_gg)
ggsave(filename = "figs/mcd14ml_n-trend_temperate.png", plot = temperate_n_gg)
ggsave(filename = "figs/mcd14ml_n-trend_cold.png", plot = cold_n_gg)

# FRP
# all
# ggplot(afd_summary, aes(x = year_month, y = frp_per_op_per_Mkm2, color = dn_detect)) +
#   geom_line() + 
#   facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
#   geom_smooth() +
#   theme_bw()

# koppen 1 Tropical
tropical_frp_gg <-
  ggplot(dplyr::filter(afd_summary, koppen == 1), aes(x = year_month, y = mean_frp_per_detection, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw() +
  labs(x = "Date (monthly increments)",
       y = "Mean FRP per detection (MW)",
       color = "Day/night")

# koppen 2 Arid
arid_frp_gg <-
  ggplot(dplyr::filter(afd_summary, koppen == 2), aes(x = year_month, y = mean_frp_per_detection, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw() +
  labs(x = "Date (monthly increments)",
       y = "Mean FRP per detection (MW)",
       color = "Day/night")

# koppen 3 Temperate
temperate_frp_gg <-
  ggplot(dplyr::filter(afd_summary, koppen == 3), aes(x = year_month, y = mean_frp_per_detection, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw() +
  labs(x = "Date (monthly increments)",
       y = "Mean FRP per detection (MW)",
       color = "Day/night")

# koppen 4 Cold
cold_frp_gg <-
  ggplot(dplyr::filter(afd_summary, koppen == 4), aes(x = year_month, y = mean_frp_per_detection, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw() +
  labs(x = "Date (monthly increments)",
       y = "Mean FRP per detection (MW)",
       color = "Day/night")

ggsave(filename = "figs/mcd14ml_frp-trend_tropical.png", plot = tropical_frp_gg)
ggsave(filename = "figs/mcd14ml_frp-trend_arid.png", plot = arid_frp_gg)
ggsave(filename = "figs/mcd14ml_frp-trend_temperate.png", plot = temperate_frp_gg)
ggsave(filename = "figs/mcd14ml_frp-trend_cold.png", plot = cold_frp_gg)


### Formal analysis
gam_data <- 
  afd_global_summary_wide %>% 
  dplyr::mutate(time = as.numeric(difftime(time1 = year_month, time2 = min(year_month), units = "days")))

# https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
m1 <- gamm(prop_n_night ~ s(acq_month, bs = "cc", k = 12) + s(time), 
          data = gam_data, 
          correlation = corARMA(form = ~ 1, p = 1))
plot(m1$gam)

summary(m1$gam)

layout(matrix(1:2, ncol = 2))
acf(resid(m1$lme), lag.max = 36, main = "ACF")
pacf(resid(m1$lme), lag.max = 36, main = "pACF")
layout(1)