# Details of GAM fits (number of unique events, number of observations, etc.)

library(dplyr)
library(here)
library(stringr)
library(data.table)
library(readr)

dir.create("tables", showWarnings = FALSE)

lc_lookup <- read.csv("data/out/koppen-modis-landcover-lookup-table.csv")
lc <- read.csv("data/out/area-per-lc.csv") %>% left_join(lc_lookup, by = c(lc = "koppen_modis_code"))

thresholds <- read.csv("data/out/zero-goes-af-vpd-thresholds-with-landcover-codes.csv")

gamready_files <- list.files(here("data", "out", "gamready"), 
                             full.names = TRUE, pattern = ".csv$") %>%
  file.info() %>%
  as_tibble(rownames = "file") %>%
  arrange(-size) %>% 
  dplyr::mutate(lc_name = basename(str_replace(file, "_gamready.csv", "")))

event_count <- lapply(1:nrow(gamready_files), FUN = function(i) {
  gamready_data <- fread(gamready_files$file[i])
  
  if(!file.exists(here::here("data", "mods", paste0(gamready_files$lc_name[i], "-gam.rds")))) {
    return(data.frame(lc_name = gsub(pattern = "_", replacement = " ", x = gamready_files$lc_name[i]), 
                      n_events_orig = length(unique(gamready_data$nid)),
                      n_obs_orig = nrow(gamready_data),
                      n_events_subset =  NA,
                      n_obs_subset = NA))
    
  }
  
  gam_fit <- readr::read_rds(here::here("data", "mods", paste0(gamready_files$lc_name[i], "-gam.rds")))

  return(data.frame(lc_name = gsub(pattern = "_", replacement = " ", x = gamready_files$lc_name[i]), 
                    n_events_orig = length(unique(gamready_data$nid)),
                    n_obs_orig = nrow(gamready_data),
                    n_events_subset =   length(unique(gam_fit$model$nid)),
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
  dplyr::select(lc, lc_name, area_Mkm2, cum_area_Mkm2, cum_pct_total_area, n_events_orig, n_obs_orig, n_events_subset, n_obs_subset, vpd_thresh_hpa, sd)
  
out

write.csv(x = out, file = "tables/gam-model-properties.csv", row.names = FALSE)
system2(command = "aws", args = "s3 cp tables/gam-model-properties.csv s3://earthlab-mkoontz/warming-weakens-the-nighttime-barrier-to-global-fire/tables/gam-model-properties.csv")
