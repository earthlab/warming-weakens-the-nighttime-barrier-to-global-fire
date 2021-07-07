# Details of GAM fits (number of unique events, number of observations, etc.)

library(dplyr)
library(here)
library(stringr)
library(data.table)
library(readr)
library(sf)
library(pbapply)

dir.create("tables", showWarnings = FALSE)

# FIRED events (to get burned area)
na <- sf::st_read("data/out/fired_na_2017-nids_lc.gpkg")
sa <- sf::st_read("data/out/fired_sa_2017-nids_lc.gpkg")
nrow(na) + nrow(sa)

lc_lookup <- read.csv("data/out/koppen-modis-landcover-lookup-table.csv")
lc <- read.csv("data/out/area-per-lc.csv") %>% left_join(lc_lookup, by = c(lc = "koppen_modis_code"))

thresholds <- read.csv("data/out/zero-goes-af-vpd-thresholds-with-landcover-codes.csv")

gamready_files <- list.files(here("data", "out", "gamready"), 
                             full.names = TRUE, pattern = ".csv$") %>%
  file.info() %>%
  as_tibble(rownames = "file") %>%
  arrange(-size) %>% 
  dplyr::mutate(lc_name = basename(str_replace(file, "_gamready.csv", "")))

event_count <- pblapply(1:nrow(gamready_files), FUN = function(i) {
  gamready_data <- fread(gamready_files$file[i])
  nids <- unique(gamready_data$nid)
  
  event_polys <- 
    na %>% 
    rbind(sa) %>% 
    dplyr::filter(nid %in% nids) %>% 
    dplyr::mutate(area_km2 = as.numeric(sf::st_area(.)) / 1e6) %>% 
    sf::st_drop_geometry()
  
  if(!file.exists(here::here("data", "out", "mods", paste0(gamready_files$lc_name[i], "-gam.rds")))) {
    return(data.frame(lc_name = gsub(pattern = "_", replacement = " ", x = gamready_files$lc_name[i]), 
                      n_events_orig = length(nids),
                      sum_area_events_orig_km2 = sum(event_polys$area_km2),
                      mean_area_events_orig_km2 = mean(event_polys$area_km2),
                      sd_area_events_orig_km2 = sd(event_polys$area_km2),
                      min_area_events_orig_km2 = min(event_polys$area_km2),
                      max_area_events_orig_km2 = max(event_polys$area_km2),
                      n_obs_orig = sum(gamready_data$n_obs_before_suff_stat),
                      n_obs_suff_stat = nrow(gamready_data),
                      n_events_subset =  NA,
                      sum_area_events_subset = NA,
                      mean_area_events_subset_km2 = NA,
                      sd_area_events_subset_km2 = NA,
                      min_area_events_subset_km2 = NA,
                      max_area_events_subset_km2 = NA,
                      n_obs_subset = NA))
    
  }
  
  gam_fit <- readr::read_rds(here::here("data", "out", "mods", paste0(gamready_files$lc_name[i], "-gam.rds")))

  nids_subset <- unique(gam_fit$model$nid)
  
  event_polys_subset <-
    event_polys %>% 
    dplyr::filter(nid %in% nids_subset)
  
  return(data.frame(lc_name = gsub(pattern = "_", replacement = " ", x = gamready_files$lc_name[i]), 
                    n_events_orig = length(nids),
                    sum_area_events_orig_km2 = sum(event_polys$area_km2),
                    mean_area_events_orig_km2 = mean(event_polys$area_km2),
                    sd_area_events_orig_km2 = sd(event_polys$area_km2),
                    min_area_events_orig_km2 = min(event_polys$area_km2),
                    max_area_events_orig_km2 = max(event_polys$area_km2),
                    n_obs_orig = sum(gamready_data$n_obs_before_suff_stat),
                    n_obs_suff_stat = nrow(gamready_data),
                    n_events_subset = length(nids_subset),
                    sum_area_events_subset_km2 = sum(event_polys_subset$area_km2),
                    mean_area_events_subset_km2 = mean(event_polys_subset$area_km2),
                    sd_area_events_subset_km2 = sd(event_polys_subset$area_km2),
                    min_area_events_subset_km2 = min(event_polys_subset$area_km2),
                    max_area_events_subset_km2 = max(event_polys_subset$area_km2),
                    n_obs_subset = nrow(gam_fit$model)))
  
}) %>% 
  bind_rows() %>% 
  arrange(desc(n_events_orig))

out <- 
  event_count %>% 
  left_join(lc, by = c(lc_name = "koppen_modis_name")) %>% 
  left_join(thresholds) %>% 
  # left_join(lc, by = c(koppen_modis_code = "lc")) %>% 
  dplyr::mutate(area_Mkm2 = area_km2 / 1e6) %>% 
  dplyr::filter(!is.na(area_Mkm2)) %>% 
  dplyr::mutate(order = 1:nrow(.)) %>% 
  dplyr::mutate(order = ifelse(order > 24, yes = order - 1, no = order)) %>% 
  dplyr::mutate(order = ifelse(order > 25, yes = order + 1, no = order)) %>% 
  dplyr::mutate(order = ifelse(lc_name == "Polar Grasslands", yes = 26, no = order)) %>% 
  dplyr::arrange(order) %>% 
  dplyr::mutate(cum_area_Mkm2 = cumsum(area_Mkm2)) %>% 
  dplyr::mutate(cum_pct_total_area = cum_area_Mkm2 / sum(.$area_Mkm2, na.rm = TRUE)) %>% 
  dplyr::select(lc, lc_name, area_Mkm2, cum_area_Mkm2, cum_pct_total_area, 
                n_events_orig, sum_area_events_orig_km2, mean_area_events_orig_km2, sd_area_events_orig_km2,
                min_area_events_orig_km2, max_area_events_orig_km2,
                n_obs_orig, n_obs_suff_stat, 
                n_events_subset, sum_area_events_subset_km2, mean_area_events_subset_km2, sd_area_events_subset_km2, 
                min_area_events_subset_km2, max_area_events_subset_km2,
                n_obs_subset, 
                vpd_thresh_hpa, sd)

# Get some summary statistics of the fire sizes across the whole set used for the modeling
# First determine all of the unique FIRED event IDs that meet our criteria (have GOES-16 detections within them)
all_nids <- 
  gamready_files %>% 
  filter(lc_name %in% gsub(pattern = " ", replacement = "_", x = out[!is.na(out$vpd_thresh_hpa), "lc_name"])) %>% 
  pull(file) %>% 
  pblapply(data.table::fread) %>% 
  data.table::rbindlist() %>% 
  pull(nid) %>% 
  unique()

# Also figure out which IDs were actually used for the modeling (recall some of the landcovers
# with lots of observations were randomly subset)
subset_nids <- 
  gamready_files %>% 
  filter(lc_name %in% gsub(pattern = " ", replacement = "_", x = out[!is.na(out$vpd_thresh_hpa), "lc_name"])) %>% 
  mutate(rds_file = here::here("data", "out", "mods", paste0(lc_name, "-gam.rds"))) %>% 
  pull(rds_file) %>% 
  pblapply(FUN = function(x) { as.numeric(unique(readr::read_rds(x)$model$nid)) }) %>% 
  unlist()

all_event_polys <-
  na %>% 
  rbind(sa) %>% 
  dplyr::filter(nid %in% all_nids) %>% 
  dplyr::mutate(area_km2 = as.numeric(sf::st_area(.)) / 1e6) %>% 
  sf::st_drop_geometry()

subset_event_polys <-
  na %>% 
  rbind(sa) %>% 
  dplyr::filter(nid %in% subset_nids) %>% 
  dplyr::mutate(area_km2 = as.numeric(sf::st_area(.)) / 1e6) %>% 
  sf::st_drop_geometry()

out_with_vpd_thresh <-
  out %>% 
  filter(!is.na(vpd_thresh_hpa))

total_row <-
  data.frame(lc = NA, lc_name = "All Burnable Landcovers", area_Mkm2 = sum(out_with_vpd_thresh$area_Mkm2), 
             cum_area_Mkm2 = sum(out_with_vpd_thresh$area_Mkm2), 
             cum_pct_total_area = 1, 
             n_events_orig = sum(out_with_vpd_thresh$n_events_orig), 
             sum_area_events_orig_km2 = sum(all_event_polys$area_km2), 
             mean_area_events_orig_km2 = mean(all_event_polys$area_km2), 
             sd_area_events_orig_km2 = sd(all_event_polys$area_km2),
             min_area_events_orig_km2 = min(all_event_polys$area_km2),
             max_area_events_orig_km2 = max(all_event_polys$area_km2),
             n_obs_orig = sum(out_with_vpd_thresh$n_obs_orig), 
             n_obs_suff_stat = sum(out_with_vpd_thresh$n_obs_suff_stat), 
             n_events_subset = sum(out_with_vpd_thresh$n_events_subset), 
             sum_area_events_subset_km2 = sum(subset_event_polys$area_km2),
             mean_area_events_subset_km2 = mean(subset_event_polys$area_km2), 
             sd_area_events_subset_km2 = sd(subset_event_polys$area_km2), 
             min_area_events_subset_km2 = min(subset_event_polys$area_km2),
             max_area_events_subset_km2 = max(subset_event_polys$area_km2),
             n_obs_subset = sum(out_with_vpd_thresh$n_obs_subset), 
             vpd_thresh_hpa = NA, sd = NA)

total_row
out

out_just_with_vpdt <- rbind(out_with_vpd_thresh, total_row) %>% 
  mutate(vpd_thresh_kpa = vpd_thresh_hpa / 10) %>% 
  arrange(vpd_thresh_hpa)

write.csv(x = out, file = "tables/gam-model-properties.csv", row.names = FALSE)
write.csv(x = out_just_with_vpdt, file = "tables/gam-model-properties-with-vpd-thresh.csv", row.names = FALSE)

system2(command = "aws", args = "s3 cp tables/gam-model-properties.csv s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/tables/gam-model-properties.csv")
system2(command = "aws", args = "s3 cp tables/gam-model-properties-with-vpd-thresh.csv s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/tables/gam-model-properties-with-vpd-thresh.csv")
