library(data.table)
library(tidyr)
library(dplyr)

area <- fread("data/out/area-per-lc-pixel.csv")

burnable <- fread("data/out/zero-goes-af-vpd-thresholds-with-landcover-codes.csv")

area_burnable <- 
  fread("data/out/area-per-lc.csv") %>% 
  filter(lc %in% burnable$koppen_modis_code) %>% 
  summarize(burnable_area_Mkm2 = sum(area_km2) / 1e6) %>% 
  pull(burnable_area_Mkm2)

# Read in the summarized MCD14ML data -- the same input as for the trend analyses
afd <- fread("data/out/mcd14ml_n-frp_month-lc-xy-daynight-summary.csv")

# Calculate overpass corrected rates of fire detection
afd[, n_per_op := n / op]

# Same data prep as for the trend analyses; overpass correction isn't perfect,
# so filter out cells that had 0 overpasses but did have active fire detections
afd <- afd[!(op == 0 & n > 0), ]

# Filter to only the period of interest (2003 to 2020)
# Summarize by individual 0.25 degree grid cells and day/night designation
# Calculate total detections per overpass across the 18-year record, the total number of
# detections, and the total FRP (just to keep consistent with the trend analysis data prep)
afd_summary <- afd[acq_year > 2002, .(total_n_per_op = sum(n_per_op), total_n = sum(n),
                                      total_frp = sum(sum_frp)), 
                   by = .(x_lc, y_lc, dn_detect)] 

# convert from long form to short form so we can calculate night proportion
afd_summary_wide <-
  afd_summary %>% 
  tidyr::pivot_wider(names_from = "dn_detect", values_from = starts_with("total")) %>% 
  mutate(prop_n_per_op_night = total_n_per_op_night / (total_n_per_op_day + total_n_per_op_night),
         prop_n_night = total_n_night / (total_n_night + total_n_day)) %>% 
  dplyr::select(x_lc, y_lc, prop_n_per_op_night, prop_n_night) %>% 
  as.data.table()

# join the per-cell info on night proportion with the "area per cell" data table
area_afd <- area[afd_summary_wide, on = c("x_lc", "y_lc")]

# filter to only cells that we considered "burnable" based on their landcover class
area_afd <- area_afd[lc %in% burnable$koppen_modis_code, ]

# filter to only cells with greater than or equal to 25% nighttime detections, calculate their total area
area_gte_25pct_night <-
  area_afd %>% 
  filter(prop_n_per_op_night >= 0.25) %>% 
  summarize(area_gt_25pct_night = sum(area_m2) / 1e12,
            pct_burnable_globe_gt_25pct_night = area_gt_25pct_night / area_burnable)

area_gte_25pct_night
