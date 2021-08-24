# Plot the VPD/AF time series for the case study fire
library(tidyverse)
library(patchwork)
library(here)

long_pts <- read_csv(here("data", "out", "case_study_data_2017-2020.csv"))
thresh <- read_csv(here("data", "out", "zero-goes-af-vpd-thresholds-with-landcover-codes.csv"))

last_date_df <- 
  long_pts %>%
  group_by(nid, lc_name) %>%
  filter(n > 0) %>%
  summarize(last_date = max(rounded_datetime) + 60 * 60 * 24 * 1, 
            first_date = min(rounded_datetime) - 60 * 60 * 24 * 1) %>% 
  left_join(thresh) %>% 
  dplyr::select(nid, lc_name, last_date, first_date, vpd_thresh_hpa)

split_events <- 
  long_pts %>%
  dplyr::select(rounded_datetime, lc_name, n, nid, vpd_kPa, event_name,
                dn_detect) %>%
  left_join(last_date_df) %>%
  filter(rounded_datetime <= last_date, 
         rounded_datetime >= first_date) %>%
  pivot_longer(cols = c("n", "vpd_kPa")) %>%
  mutate(value = ifelse(!(name == "n" & value == 0), yes = value, no = NA)) %>% 
  mutate(name = ifelse(name == "n", "AF counts", "VPD (kPa)")) %>% 
  mutate(thresh_vpd_kPa = ifelse(name == "VPD (kPa)", 
                                 yes = vpd_thresh_hpa / 10,
                                 no = NA)) %>% 
  group_by(nid) %>% 
  group_split()

plot_list <- 
  split_events %>%
  lapply(function(x) {
    event_name <- unique(x$event_name)
    
    pct_night <- x %>%
      filter(name == "AF counts", !is.na(value)) %>%
      group_by(dn_detect) %>%
      summarize(n_af = sum(value)) %>%
      ungroup() %>% 
      pivot_wider(names_from = "dn_detect", values_from = "n_af") %>% 
      dplyr::mutate(pct_night = night / (night + day)) %>% 
      pull(pct_night)
    
    title <- paste0(unique(x$event_name), ": ", 100 * round(pct_night, 2), 
                    "% nighttime fire detections")
    
    ggplot(x, aes(rounded_datetime - lubridate::days(1), value, color = dn_detect)) + 
      geom_point(size = .6, alpha= .8) + 
      geom_line(alpha = 0.3, color = "black") +
      geom_hline(aes(yintercept = thresh_vpd_kPa), lty = 2) +
      theme_minimal() + 
      facet_wrap(~name, scales = "free_y", ncol = 1, strip.position = "left") + 
      xlab("") + 
      ylab("") + 
      theme(panel.grid.minor = element_blank(), 
            legend.position = "none") +
      scale_color_manual(values = c("red", "#2166ac")) + 
      ggtitle(title) + 
      theme(plot.title = element_text(size=12))
    
  })

p <- wrap_plots(plot_list, ncol = 1)
p

ggsave("figs/case-study-ts.pdf", plot = p, width = 8, height = 6.5)
ggsave("figs/case-study-ts.png", plot = p, width = 8, height = 6.5)
