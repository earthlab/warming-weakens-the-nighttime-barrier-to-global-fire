# define the day and nighttime fire season by landcover type

library(dplyr)
library(data.table)
library(cowplot)
library(patchwork)
library(ggtext)
library(ggplot2)
library(purrr)
library(tidyr)

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

# Collate plotting data
plotting_data_peak_detections <-
  afd_of_interest_lc %>% 
  filter(over_threshold_smooth_detections == 1) %>%
  tidyr::complete(doy, nesting(dn_detect, lc_name), fill = list(smoothed_detections = NA)) %>% 
  dplyr::mutate(peak = 1)

plotting_data_nonpeak_detections <-
  afd_of_interest_lc %>% 
  filter(over_threshold_smooth_detections != 1) %>%
  tidyr::complete(doy, nesting(dn_detect, lc_name), fill = list(smoothed_detections = NA)) %>% 
  dplyr::mutate(peak = 0)

plotting_data_detections <- 
  rbind(plotting_data_peak_detections, plotting_data_nonpeak_detections) %>% 
  dplyr::mutate(type = "detections")

plotting_data_peak_frp <-
  afd_of_interest_lc %>% 
  filter(over_threshold_smooth_frp == 1) %>%
  tidyr::complete(doy, nesting(dn_detect, lc_name), fill = list(smoothed_frp = NA)) %>% 
  dplyr::mutate(peak = 1)

plotting_data_nonpeak_frp <-
  afd_of_interest_lc %>% 
  filter(over_threshold_smooth_frp != 1) %>%
  tidyr::complete(doy, nesting(dn_detect, lc_name), fill = list(smoothed_frp = NA)) %>% 
  dplyr::mutate(peak = 0)

plotting_data_frp <- 
  rbind(plotting_data_peak_frp, plotting_data_nonpeak_frp) %>% 
  dplyr::mutate(type = "frp")

plotting_data <- rbind(plotting_data_detections, plotting_data_frp)
write.csv(x = plotting_data, file = "figs/source-data/edf2-source-data.csv", row.names = FALSE)

# Build plots

# We determined these limits by first building the gg_fire_season and gg_fire_season_frp faceted
# plots without including the `limits = ylim_detections' or `limits = ylim_frp` constraints in
# the scale_y_log10() call, then tweaking them a bit. Here, we use what we learned by how
# ggplot2 wants to set those limits automatically, then just hard code the tweaks so that all
# the plots have the same limits

# detections_ylim <- 10^ggplot_build(gg_fire_season)$layout$panel_scales_y[[1]]$range$range
# detections_ylim[1] <- 0.01
detections_ylim <- c(0.01, 475.8775)

# frp_ylim <- 10^ggplot_build(gg_fire_season_frp)$layout$panel_scales_y[[1]]$range$range
# frp_ylim[1] <- 15
frp_ylim <- c(15, 160.1649)

# detections first
gg_fire_season <-
  ggplot(filter(plotting_data, peak == 0, type == "detections"), aes(x = doy, 
                                                                     y = 1e6 * smoothed_detections, 
                                                                     color = dn_detect)) +
  geom_line(alpha = 0.25, size = 0.5) +
  geom_line(data = filter(plotting_data, peak == 1, type == "detections"), alpha = 1, size = 0.5) +
  theme_bw(base_size = 5) +
  theme(strip.text = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        axis.title.y.right = element_text(margin = unit(c(t = 0, r = -20, b = 0, l = 10), "pt")),
        axis.text = element_text(),
        axis.title.x = element_blank(),
        text = element_text(),
        legend.position = "bottom") +
  scale_color_manual(values = c("#b2182b", "#2166ac")) +
  # so the order is the same as the Bayes plot (based on vpd threshold)
  facet_wrap(~reorder(lc_name, vpd_thresh_hpa, na.rm = TRUE), nrow = 5, labeller = label_wrap_gen(width = 22), strip.position = "left") +
  # so the order is descending from highest nighttime detections
  # facet_wrap(facets = vars(landcover_split), nrow = 5) +
  labs(x = "Day of year",
       y = bquote("Expected detections per day per overpass per " ~ Mkm^2),
       color = "") +
  guides(alpha = "none") +
  scale_x_continuous(breaks = first_of_months$doy_first, labels = first_of_months$name_abbrv) +
  scale_y_log10(breaks = c(0.01, 0.1, 1.0, 10.0, 100.0),
                limits = detections_ylim,
                labels = scales::label_comma(accuracy = 0.1), 
                position = "right") +
  geom_vline(xintercept = first_of_months$doy_first, size = 0.25) +
  coord_polar() +
  scale_alpha_identity()

# gg_fire_season


# FRP

gg_fire_season_frp <-
  ggplot(filter(plotting_data, peak == 0, type == "frp"), aes(x = doy, 
                                                              y = smoothed_frp, 
                                                              color = dn_detect)) +
  geom_line(alpha = 0.25, size = 0.5) +
  geom_line(data = filter(plotting_data, peak == 1, type == "frp"), alpha = 1, size = 0.5) +
  theme_bw(base_size = 5) +
  theme(strip.text = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        axis.title.y.right = element_text(margin = unit(c(t = 0, r = -20, b = 0, l = 10), "pt")),
        axis.text = element_text(),
        axis.title.x = element_blank(),
        text = element_text(),
        legend.position = "bottom") +
  scale_color_manual(values = c("#b2182b", "#2166ac")) +
  # so the order is the same as the Bayes plot (based on vpd threshold)
  facet_wrap(~reorder(lc_name, vpd_thresh_hpa, na.rm = TRUE), 
             nrow = 5, 
             labeller = label_wrap_gen(width = 22), 
             strip.position = "left") +
  # so the order is descending from highest nighttime detections
  # facet_wrap(facets = vars(landcover_split), nrow = 5) +
  labs(x = "Day of year",
       y = "Expected FRP per detection",
       color = "") +
  guides(alpha = "none") +
  scale_x_continuous(breaks = first_of_months$doy_first, labels = first_of_months$name_abbrv) +
  scale_y_log10(breaks = c(15, 30, 50, 100),
                limits = frp_ylim,
                labels = scales::label_comma(accuracy = 0.1), 
                position = "right") +
  geom_vline(xintercept = first_of_months$doy_first, size = 0.25) +
  coord_polar() +
  scale_alpha_identity()

# gg_fire_season_frp

dn_detect_legend <- cowplot::get_legend(gg_fire_season)

# Combine into two-panel plot
pcol <- cowplot::plot_grid(gg_fire_season + theme(legend.position = "none"), 
                           gg_fire_season_frp + theme(legend.position = "none"), 
                           nrow = 2, labels = c("a", "b"))

# pcol

p <- cowplot::plot_grid(pcol, dn_detect_legend, ncol = 1, rel_heights = c(1, 0.025))

ggsave(filename = "figs/fire-seasonality-two-panel-portrait_afd-frp_daynight-landcover.pdf", plot = p, width = 145, height = 200, units = "mm")

ggsave(filename = "figs/fire-seasonality-two-panel-portrait_afd-frp_daynight-landcover.png", plot = p, width = 145, height = 200, units = "mm", dpi = 300)

ggsave(filename = "figs/fire-seasonality-two-panel-portrait_afd-frp_daynight-landcover.jpg", plot = p, width = 145, height = 200, units = "mm", dpi = 300)



# Michael's solution ------------------------------------------------

plotting_data_michael <-
  plotting_data %>% 
  dplyr::mutate(lc_name = reorder(lc_name, vpd_thresh_hpa, na.rm = TRUE),
                y = ifelse(type == "detections", 
                           yes = 1e6 * smoothed_detections,
                           no = smoothed_frp),
                y_label = ifelse(type == "detections",
                                 yes = "Detections per day\nper overpass per Mkm\U00B2", # use \U00B2 for unicode superscript 2
                                 no = "\nFRP per detection"))

plotting_data_split <-
  plotting_data_michael %>% 
  group_by(lc_name) %>% 
  group_split()

# modified from label_wrap_gen()
my_labeller <- function (width = 25, multi_line = TRUE) 
{
  fun <- function(labels) {
    labels <- label_value(labels, multi_line = multi_line)
    lapply(labels, function(x) {
      x <- strwrap(x, width = width, simplify = FALSE)
      x <- vapply(x, paste, character(1), collapse = "\n")
      if(!grepl(x = x, pattern = "\n")) x <- paste0("\n", x)
      
      return(x)
    })
  }
  structure(fun, class = "labeller")
}

plot_list <- pbapply::pblapply(plotting_data_split, FUN = function(x) {
  
  detections <- dplyr::filter(x, type == "detections")
  frp <- dplyr::filter(x, type == "frp")
  
  this_detections_plot <-
    ggplot(filter(detections, peak == 0), aes(x = doy, 
                                     y = y, 
                                     color = dn_detect)) +
    geom_line(alpha = 0.25, size = 0.5) +
    geom_line(data = filter(detections, peak == 1), alpha = 1, size = 0.5) +
    theme_bw(base_size = 5) +
    theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
          strip.text = element_text(angle = 0),
          strip.background = element_rect(fill = "white"),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 5),
          panel.spacing = margin(0)) +
    scale_color_manual(values = c("#b2182b", "#2166ac")) +
    facet_wrap(facets = vars(y_label)) +
    labs(x = "",
         y = "",
         color = "") +
    guides(alpha = "none") +
    scale_x_continuous(breaks = first_of_months$doy_first, labels = first_of_months$name_abbrv) +
    scale_y_log10(breaks = c(0.01, 0.1, 1.0, 10.0, 100.0), 
                  labels = scales::label_comma(accuracy = 0.1), 
                  limits = detections_ylim) +
    geom_vline(xintercept = first_of_months$doy_first, size = 0.25) +
    coord_polar() +
    scale_alpha_identity()
  
  this_frp_plot <-
    ggplot(filter(frp, peak == 0), aes(x = doy, 
                                              y = y, 
                                              color = dn_detect)) +
    geom_line(alpha = 0.25, size = 0.5) +
    geom_line(data = filter(frp, peak == 1), alpha = 1, size = 0.5) +
    theme_bw(base_size = 5) +
    theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
          strip.text = element_text(angle = 0),
          strip.background = element_rect(fill = "white"),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 5),
          panel.spacing = margin(0)) +
    scale_color_manual(values = c("#b2182b", "#2166ac")) +
    facet_wrap(facets = vars(y_label)) +
    labs(x = "",
         y = "",
         color = "") +
    guides(alpha = "none") +
    scale_x_continuous(breaks = first_of_months$doy_first, labels = first_of_months$name_abbrv) +
    scale_y_log10(breaks = c(15, 30, 50, 100), 
                  labels = scales::label_comma(accuracy = 0.1), 
                  limits = frp_ylim,
                  position = "right") +
    geom_vline(xintercept = first_of_months$doy_first, size = 0.25) +
    coord_polar() +
    scale_alpha_identity()
  
  this_detections_plot
  this_frp_plot
  
  title <- 
    ggdraw() + 
    draw_label(label = my_labeller(width = 30)(unique(x$lc_name)),
               size = 5, hjust = 0.5) +
    theme(plot.margin = margin(0))
  
  pair_plot <- cowplot::plot_grid(this_detections_plot, this_frp_plot, scale = 1.05)
  pair_plot <- cowplot::plot_grid(title, pair_plot, ncol = 1, rel_heights = c(0.25, 1))
  
  return(pair_plot)  
})

dn_detect_legend <- cowplot::get_legend(gg_fire_season + theme(legend.position = "right"))

final_plot_list <- vector(mode = "list", length = 28)
final_plot_list[1:24] <- plot_list[1:24]
final_plot_list[[26]] <- plot_list[[25]]
final_plot_list[[27]] <- dn_detect_legend

mp <- cowplot::plot_grid(plotlist = final_plot_list, ncol = 4, scale = 1.04)

ggsave(filename = "figs/fire-seasonality-two-panel-portrait-editor-suggestion_afd-frp_daynight-landcover.pdf", plot = mp, width = 183, height = 200, units = "mm")

ggsave(filename = "figs/fire-seasonality-two-panel-portrait-editor-suggestion_afd-frp_daynight-landcover.png", plot = mp, width = 183, height = 200, units = "mm", dpi = 300)

ggsave(filename = "figs/fire-seasonality-two-panel-portrait-editor-suggestion_afd-frp_daynight-landcover.jpg", plot = mp, width = 183, height = 200, units = "mm", dpi = 300)





# table summary -----------------------------------------------------------

seasonality_table <-
  afd_of_interest_lc %>% 
  group_by(lc, lc_name, dn_detect) %>% 
  dplyr::summarize(season_length_detections = sum(over_threshold_smooth_detections == alpha_high),
                   season_length_frp = sum(over_threshold_smooth_frp == alpha_high)) %>% 
  tidyr::pivot_wider(names_from = dn_detect, values_from = c("season_length_detections", "season_length_frp")) %>% 
  dplyr::arrange(lc)

readr::write_csv(seasonality_table, path = "tables/fire-seasonality-summary-table.csv")

# Landscape orientation ---------------------------------------------------

gg_fire_season <-
  ggplot(filter(plotting_data, peak == 0, type == "detections"), aes(x = doy, 
                                                                     y = 1e6 * smoothed_detections, 
                                                                     color = dn_detect)) +
  geom_line(alpha = 0.25, size = 0.5) +
  geom_line(data = filter(plotting_data, peak == 1, type == "detections"), alpha = 1, size = 0.5) +
  theme_bw(base_size = 5) +
  theme(strip.text = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        axis.title.y = element_text(margin = unit(c(t = 0, r = 5, b = 0, l = 0), "pt")),
        axis.text = element_text(),
        axis.title.x = element_blank(),
        text = element_text(),
        legend.position = "bottom") +
  scale_color_manual(values = c("#b2182b", "#2166ac")) +
  # so the order is the same as the Bayes plot (based on vpd threshold)
  facet_wrap(~reorder(lc_name, vpd_thresh_hpa, na.rm = TRUE), nrow = 5, labeller = label_wrap_gen(width = 22)) +
  # so the order is descending from highest nighttime detections
  # facet_wrap(facets = vars(landcover_split), nrow = 5) +
  labs(x = "Day of year",
       y = bquote("Expected detections per day per overpass per " ~ Mkm^2),
       color = "") +
  guides(alpha = "none") +
  scale_x_continuous(breaks = first_of_months$doy_first, labels = first_of_months$name_abbrv) +
  scale_y_log10(labels = scales::label_comma(accuracy = 0.1)) +
  geom_vline(xintercept = first_of_months$doy_first, size = 0.25) +
  coord_polar() +
  scale_alpha_identity()

gg_fire_season


gg_fire_season_frp <-
  ggplot(filter(plotting_data, peak == 0, type == "frp"), aes(x = doy, 
                                                              y = smoothed_frp, 
                                                              color = dn_detect)) +
  geom_line(alpha = 0.25, size = 0.5) +
  geom_line(data = filter(plotting_data, peak == 1, type == "frp"), alpha = 1, size = 0.5) +
  theme_bw(base_size = 5) +
  theme(strip.text = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        axis.title.y = element_text(margin = unit(c(t = 0, r = 5, b = 0, l = 0), "pt")),
        axis.text = element_text(),
        axis.title.x = element_blank(),
        text = element_text(),
        legend.position = "bottom") +
  scale_color_manual(values = c("#b2182b", "#2166ac")) +
  # so the order is the same as the Bayes plot (based on vpd threshold)
  facet_wrap(~reorder(lc_name, vpd_thresh_hpa, na.rm = TRUE), nrow = 5, labeller = label_wrap_gen(width = 22)) +
  # so the order is descending from highest nighttime detections
  # facet_wrap(facets = vars(landcover_split), nrow = 5) +
  labs(x = "Day of year",
       y = "Expected FRP per detection",
       color = "") +
  guides(alpha = "none") +
  scale_x_continuous(breaks = first_of_months$doy_first, labels = first_of_months$name_abbrv) +
  scale_y_log10(labels = scales::label_comma(accuracy = 0.1)) +
  geom_vline(xintercept = first_of_months$doy_first, size = 0.25) +
  coord_polar() +
  scale_alpha_identity()

gg_fire_season_frp

dn_detect_legend <- cowplot::get_legend(gg_fire_season)

# Use only one plot
prow <- cowplot::plot_grid(gg_fire_season + theme(legend.position = "none"), 
                           gg_fire_season_frp + theme(legend.position = "none"), 
                           nrow = 1, labels = c("a", "b"))

p <- cowplot::plot_grid(prow, dn_detect_legend, ncol = 1, rel_heights = c(1, 0.05))

ggsave(filename = "figs/fire-seasonality-two-panel_afd-frp_daynight-landcover.pdf", plot = p, width = 183 * (11 / 8.5), height = 150, units = "mm")
ggsave(filename = "figs/fire-seasonality-two-panel_afd-frp_daynight-landcover.png", plot = p, width = 183 * (11 / 8.5), height = 150, units = "mm")

# Write individual panels to disk
ggsave(filename = "figs/fire-seasonality-detections_daynight-landcover.png", plot = gg_fire_season, width = 183, height = 4/3 * 183, units = "mm")
ggsave(filename = "figs/fire-seasonality-frp_daynight-landcover.pdf", plot = gg_fire_season_frp, width = 183, height = 4/3 * 183, units = "mm")

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

