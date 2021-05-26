# plot trends in MODIS active fire detections/FRP through time by landcover and day/night

library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)

dir.create("figs", showWarnings = FALSE)

lc_area <- read.csv("data/out/area-per-lc-pixel.csv")
lc_lookup <- read.csv("data/out/koppen-modis-landcover-lookup-table.csv")

lc <-
  lc_area %>% 
  dplyr::left_join(y = lc_lookup, by = c(lc = "koppen_modis_code")) %>% 
  as.data.table()

afd <- data.table::fread(input = "data/out/mcd14ml_n-frp_month-lc-xy-daynight-summary.csv")

afd[, `:=`(n_per_op = ifelse(dn_detect == "day", yes = n / op_day, no = n / op_night),
           frp_per_op = ifelse(dn_detect == "day", yes = mean_frp / op_day, no = mean_frp / op_night))]

afd <- afd[!(op_night == 0 & dn_detect == "night"), ][!(op_day == 0 & dn_detect == "day"), ]

# join active fire data with area information per cell

afd <- lc[afd, on = c("cell_id_lc", "x_lc", "y_lc", "lc")]

afd_summary <- 
  afd[, .(total_n_per_op = sum(n_per_op), total_frp_per_op = sum(frp_per_op), 
          n_per_op_per_px = mean(n_per_op), frp_per_op_per_px = mean(frp_per_op),
          n_px = .N), 
      by = .(acq_year, acq_month, dn_detect, lc)] %>% 
  dplyr::left_join(lc) %>% 
  dplyr::mutate(year_month = lubridate::ymd(paste0(acq_year, "-", acq_month, "-", "15"))) %>% 
  tidyr::complete(year_month, nesting(lc, dn_detect, koppen_orig_modis_name), fill = list(n_per_op = 0, frp_per_op = 0)) %>% 
  dplyr::mutate(n_per_op_per_Mkm2 = (total_n_per_op / area_km2) * 1e6,
                frp_per_op_per_Mkm2 = (total_frp_per_op / area_km2) * 1e6) %>% 
  dplyr::filter(!is.na(koppen_orig_modis_name), acq_year >= 2003) %>% 
  dplyr::mutate(koppen = substr(x = lc, start = 1, stop = 1),
                modis = substr(x = lc, start = 2, stop = 3))

afd_summary

afd_global_summary <-
  afd[, .(n_per_op = mean(n_per_op), frp_per_op = mean(frp_per_op)), by = .(acq_year, acq_month, dn_detect)] %>% 
  dplyr::mutate(year_month = lubridate::ymd(paste0(acq_year, "-", acq_month, "-", "15"))) %>% 
  tidyr::complete(year_month, nesting(dn_detect), fill = list(n_per_op = 0, frp_per_op = 0)) %>% 
  dplyr::mutate(n_per_op_per_Mkm2 = (n_per_op / sum(lc$area_km2)) * 1e6,
                frp_per_op_per_Mkm2 = (frp_per_op / sum(lc$area_km2)) * 1e6) %>% 
  dplyr::filter(acq_year >= 2003)

afd_global_summary_wide <-
  afd_global_summary %>% 
  tidyr::pivot_wider(names_from = "dn_detect", values_from = c("n_per_op", "frp_per_op"), id_cols = c("year_month", "acq_year", "acq_month")) %>% 
  dplyr::mutate(prop_n_night = n_per_op_night  / (n_per_op_night + n_per_op_day))

global_n_gg <-
  ggplot(afd_global_summary, aes(x = year_month, y = n_per_op, color = dn_detect)) +
  geom_line() +
  geom_smooth() +
  theme_bw()

ggsave(filename = "figs/mcd14ml_n-trend_global.png", plot = global_n_gg)

global_frp_gg <-
  ggplot(afd_global_summary, aes(x = year_month, y = frp_per_op, color = dn_detect)) +
  geom_line() +
  geom_smooth() +
  theme_bw()

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
# number of detections
# all
# ggplot(afd_summary, aes(x = year_month, y = n_per_op_per_Mkm2, color = dn_detect)) +
#   geom_line() + 
#   facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
#   geom_smooth() +
#   theme_bw()

# koppen 1 Tropical
tropical_n_gg <-
  ggplot(dplyr::filter(afd_summary, koppen == 1), aes(x = year_month, y = n_per_op_per_Mkm2, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw()

# koppen 2 Arid
arid_n_gg <-
  ggplot(dplyr::filter(afd_summary, koppen == 2), aes(x = year_month, y = n_per_op_per_Mkm2, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw()

# koppen 3 Temperate
temperate_n_gg <- 
  ggplot(dplyr::filter(afd_summary, koppen == 3), aes(x = year_month, y = n_per_op_per_Mkm2, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw()

# koppen 4 Cold
cold_n_gg <-
ggplot(dplyr::filter(afd_summary, koppen == 4), aes(x = year_month, y = n_per_op_per_Mkm2, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw()

# koppen 5 Polar
polar_n_gg <-
ggplot(dplyr::filter(afd_summary, koppen == 5), aes(x = year_month, y = n_per_op_per_Mkm2, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw()

ggsave(filename = "figs/mcd14ml_n-trend_tropical.png", plot = tropical_n_gg)
ggsave(filename = "figs/mcd14ml_n-trend_arid.png", plot = arid_n_gg)
ggsave(filename = "figs/mcd14ml_n-trend_temperate.png", plot = temperate_n_gg)
ggsave(filename = "figs/mcd14ml_n-trend_cold.png", plot = cold_n_gg)
ggsave(filename = "figs/mcd14ml_n-trend_polar.png", plot = polar_n_gg)

# FRP
# all
# ggplot(afd_summary, aes(x = year_month, y = frp_per_op_per_Mkm2, color = dn_detect)) +
#   geom_line() + 
#   facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
#   geom_smooth() +
#   theme_bw()

# koppen 1 Tropical
tropical_frp_gg <-
  ggplot(dplyr::filter(afd_summary, koppen == 1), aes(x = year_month, y = frp_per_op_per_Mkm2, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw()

# koppen 2 Arid
arid_frp_gg <-
  ggplot(dplyr::filter(afd_summary, koppen == 2), aes(x = year_month, y = frp_per_op_per_Mkm2, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw()

# koppen 3 Temperate
temperate_frp_gg <-
  ggplot(dplyr::filter(afd_summary, koppen == 3), aes(x = year_month, y = frp_per_op_per_Mkm2, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw()

# koppen 4 Cold
cold_frp_gg <-
  ggplot(dplyr::filter(afd_summary, koppen == 4), aes(x = year_month, y = frp_per_op_per_Mkm2, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw()

# koppen 5 Polar
polar_frp_gg <-
  ggplot(dplyr::filter(afd_summary, koppen == 5), aes(x = year_month, y = frp_per_op_per_Mkm2, color = dn_detect)) +
  geom_line() + 
  facet_wrap(facets = "koppen_orig_modis_name", scales = "free_y") +
  geom_smooth() +
  theme_bw()


ggsave(filename = "figs/mcd14ml_frp-trend_tropical.png", plot = tropical_frp_gg)
ggsave(filename = "figs/mcd14ml_frp-trend_arid.png", plot = arid_frp_gg)
ggsave(filename = "figs/mcd14ml_frp-trend_temperate.png", plot = temperate_frp_gg)
ggsave(filename = "figs/mcd14ml_frp-trend_cold.png", plot = cold_frp_gg)
ggsave(filename = "figs/mcd14ml_frp-trend_polar.png", plot = polar_frp_gg)
