# define the day and nighttime fire season by landcover type

library(dplyr)
library(data.table)
library(cowplot)
library(patchwork)
library(ggtext)
library(ggplot2)
library(purrr)

# table of the first of each month in DOY ---------------------------------

first_of_months <-
  tibble(month = 1:12, name = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), name_abbrv = substr(name, start = 1, stop = 3), days_in_month = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
         doy_first = lag(cumsum(days_in_month) + 1)) %>% 
  dplyr::mutate(doy_first = ifelse(month == 1, yes = 1, no = doy_first),
                doy_last = lead(doy_first) - 1,
                doy_last = ifelse(month == 12, yes = 365, no = doy_last),
                doys_in_month = map2(doy_first, doy_last, seq)) %>% 
  dplyr::mutate(doy_first_leap = ifelse(month > 2, yes = doy_first + 1, no = doy_first),
                doy_last_leap = ifelse(month > 1, yes = doy_last + 1, no = doy_last),
                doys_in_month_leap = map2(doy_first_leap, doy_last_leap, seq))

# read data describing active fire detection count per day of year across the 18 year record
afd_of_interest_lc <- data.table::fread("data/out/seasonality_afd-and-frp-by-day-of-year-landcover.csv")

# Plot
gg_fire_season <-
  ggplot(afd_of_interest_lc, aes(x = doy, 
                                 y = 1e6 * smoothed_detections, 
                                 color = dn_detect, 
                                 alpha = over_threshold_smooth_detections)) +
  geom_line(lwd = 1) +
  theme_bw(base_size = 10) +
  theme(strip.text = element_text(angle = 0, face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "pt")),
        axis.text = element_text(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        text = element_text()) +
  scale_color_manual(values = c("#b2182b", "#2166ac")) +
  # so the order is the same as the Bayes plot (based on vpd threshold)
  facet_wrap(~reorder(landcover_split, vpd_thresh_hpa), nrow = 5) +
  # so the order is descending from highest nighttime detections
  # facet_wrap(facets = vars(landcover_split), nrow = 5) +
  labs(x = "Day of year",
       y = bquote("Expected detections per day per overpass per " ~ Mkm^2),
       color = "Day or night?") +
  guides(alpha = FALSE) +
  scale_x_continuous(breaks = first_of_months$doy_first, labels = first_of_months$name_abbrv) +
  scale_y_log10(labels = scales::comma) +
  geom_vline(xintercept = first_of_months$doy_first) +
  coord_polar() +
  scale_alpha_identity()

gg_fire_season

ggsave(filename = "figs/fire-seasonality-detections_daynight-landcover.png", plot = gg_fire_season, width = 183, height = 4/3 * 183, units = "mm")


# FRP

gg_fire_season_frp <-
  ggplot(afd_of_interest_lc, aes(x = doy, 
                                 y = smoothed_frp, 
                                 color = dn_detect, 
                                 alpha = over_threshold_smooth_frp)) +
  geom_line(lwd = 1) +
  theme_bw(base_size = 10) +
  theme(strip.text = element_text(angle = 0, face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "pt")),
        axis.text = element_text(),
        axis.title.x = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(values = c("#b2182b", "#2166ac")) +
  # so the order is the same as the Bayes plot (based on vpd threshold)
  facet_wrap(~reorder(landcover_split, vpd_thresh_hpa), nrow = 5) +
  # so the order is descending from highest nighttime detections
  # facet_wrap(facets = vars(landcover_split), nrow = 5) +
  labs(x = "Day of year",
       y = "Expected FRP per detection",
       color = "Day or night?") +
  guides(alpha = FALSE) +
  scale_x_continuous(breaks = first_of_months$doy_first, labels = first_of_months$name_abbrv) +
  scale_y_log10(labels = scales::comma) +
  geom_vline(xintercept = first_of_months$doy_first) +
  coord_polar() +
  scale_alpha_identity()

gg_fire_season_frp

ggsave(filename = "figs/fire-seasonality-frp_daynight-landcover.png", plot = gg_fire_season_frp, width = 183, height = 4/3 * 183, units = "mm")

dn_detect_legend <- cowplot::get_legend(gg_fire_season)

# Use only one plot
prow <- cowplot::plot_grid(gg_fire_season + theme(legend.position = "none"), 
                        gg_fire_season_frp + theme(legend.position = "none"), 
                        nrow = 1, labels = c("a", "b"))

p <- cowplot::plot_grid(prow, dn_detect_legend, ncol = 1, rel_heights = c(1, 0.1))
p

ggsave(filename = "figs/fire-seasonality-two-panel_afd-frp_daynight-landcover.png", plot = p, width = 183 * 2, height = 4/3 * 183, units = "mm")
ggsave(filename = "figs/fire-seasonality-two-panel_afd-frp_daynight-landcover.pdf", plot = p, width = 183 * 2, height = 4/3 * 183, units = "mm")


# table summary -----------------------------------------------------------

seasonality_table <-
  afd_of_interest_lc %>% 
  group_by(lc, lc_name, dn_detect) %>% 
  dplyr::summarize(season_length_detections = sum(over_threshold_smooth_detections == alpha_high),
                   season_length_frp = sum(over_threshold_smooth_frp == alpha_high)) %>% 
  tidyr::pivot_wider(names_from = dn_detect, values_from = c("season_length_detections", "season_length_frp")) %>% 
  dplyr::arrange(lc)

readr::write_csv(seasonality_table, path = "tables/fire-seasonality-summary-table.csv")


# by hemisphere also ------------------------------------------------------

afd_of_interest_by_hemisphere_lc <- data.table::fread("data/out/seasonality_afd-and-frp-by-day-of-year-landcover-hemisphere.csv")

gg_fire_season_northern_hemisphere <- 
  ggplot(afd_of_interest_by_hemisphere_lc %>% dplyr::filter(hemisphere == "northern"), aes(x = doy, y = 1e6 * smoothed_detections, color = dn_detect, alpha = over_threshold_smooth_detections)) +
  geom_line() +
  theme_bw(base_size = 10) +
  theme(strip.text = element_text(angle = 0, face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "pt")),
        axis.text = element_text(),
        axis.title.x = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(values = c("red", "black")) +
  # so the order is the same as the Bayes plot (based on vpd threshold)
  facet_wrap(~reorder(landcover_split, vpd_thresh_hpa), nrow = 5) +
  # so the order is descending from highest nighttime detections
  # facet_wrap(facets = vars(landcover_split), ncol = 5) +
  labs(x = "Day of year",
       y = bquote("Mean detections per day per overpass per 1,000" ~ km^2),
       color = "Day or night?",
       title = "Northern hemisphere") +
  guides(alpha = FALSE) +
  scale_x_continuous(breaks = first_of_months$doy_first, labels = first_of_months$name_abbrv) +
  scale_y_log10(labels = scales::comma) +
  geom_vline(xintercept = first_of_months$doy_first) +
  coord_polar() +
  scale_alpha_identity()

gg_fire_season_northern_hemisphere

ggsave(filename = "figures/fire-seasonality_daynight-landcover-northern-hemisphere.png", plot = gg_fire_season_northern_hemisphere, width = 183, height = 4/3 * 183, units = "mm")


gg_fire_season_southern_hemisphere <- 
  ggplot(afd_of_interest_by_hemisphere_lc %>% dplyr::filter(hemisphere == "southern"), aes(x = doy, y = 1e6 * smoothed_detections, color = dn_detect, alpha = over_threshold_smooth_detections)) +
  geom_line() +
  theme_bw(base_size = 10) +
  theme(strip.text = element_text(angle = 0, face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "pt")),
        axis.text = element_text(),
        axis.title.x = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(values = c("red", "black")) +
  # so the order is the same as the Bayes plot (based on vpd threshold)
  facet_wrap(~reorder(landcover_split, vpd_thresh_hpa), nrow = 5) +
  # so the order is descending from highest nighttime detections
  # facet_wrap(facets = vars(landcover_split), ncol = 5, drop = FALSE) +
  labs(x = "Day of year",
       y = bquote("Mean detections per day per overpass per 1,000" ~ km^2),
       color = "Day or night?",
       title = "Southern hemisphere") +
  guides(alpha = FALSE) +
  scale_x_continuous(breaks = first_of_months$doy_first, labels = first_of_months$name_abbrv) +
  scale_y_log10(labels = scales::comma) +
  geom_vline(xintercept = first_of_months$doy_first) +
  coord_polar() +
  scale_alpha_identity()

gg_fire_season_southern_hemisphere

ggsave(filename = "figures/fire-seasonality_daynight-landcover-southern-hemisphere.png", plot = gg_fire_season_southern_hemisphere, width = 183, height = 4/3 * 183, units = "mm")

