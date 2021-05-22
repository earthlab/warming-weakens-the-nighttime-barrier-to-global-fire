# create landcover lookup table for Koppen + MODIS mashup

library(dplyr)
library(tidyr)

# From MCD12Q1.006 Users Guide; Last accessed 2021-04-01
# (MCD12_User_Guide_V6.pdf)
modis_lc_name <- c("Evergreen Needleleaf Forests",
                    "Evergreen Broadleaf Forests",
                    "Deciduous Needleleaf Forests",
                    "Deciduous Broadleaf Forests",
                    "Mixed Forests",
                    "Closed Shrublands",
                    "Open Shrublands",
                    "Woody Savannas",
                    "Savannas",
                    "Grasslands",
                    "Permanent Wetlands",
                    "Croplands",
                    "Urban and Built-up Lands",
                    "Cropland Natural Vegetation Mosaics",
                    "Permanent Snow and Ice",
                    "Barren",
                    "Water Bodies")
modis_lc_code <- 1:17

modis_lc <- data.frame(modis_lc_code, modis_lc_name)

# From Beck, H.E., N.E. Zimmermann, T.R. McVicar, N. Vergopolan, A. Berg, E.F. Wood: Present and future Köppen-Geiger climate classification maps at 1-km resolution, Scientific Data 5:180214, doi:10.1038/sdata.2018.214 (2018).
# https://figshare.com/articles/dataset/Present_and_future_K_ppen-Geiger_climate_classification_maps_at_1-km_resolution/6396959/2
# Table 2

koppen_lc_name <- c("Equatorial", "Arid", "Temperate", "Boreal", "Polar")
koppen_lc_original_name <- c("Tropical", "Arid", "Temperate", "Cold", "Polar")
koppen_lc_code <- (1:5) * 100

koppen_lc <- data.frame(koppen_lc_code, koppen_lc_name, koppen_lc_original_name)

koppen_modis_lookup_table <- 
  tidyr::crossing(modis_lc_name, koppen_lc_name) %>% 
  dplyr::left_join(modis_lc) %>% 
  dplyr::left_join(koppen_lc) %>% 
  dplyr::mutate(koppen_modis_name = paste(koppen_lc_name, modis_lc_name),
                koppen_orig_modis_name = paste(koppen_lc_original_name, modis_lc_name)) %>% 
  dplyr::mutate(koppen_modis_code = modis_lc_code + koppen_lc_code) %>% 
  dplyr::select(koppen_modis_code, koppen_modis_name, koppen_orig_modis_name) %>% 
  dplyr::arrange(koppen_modis_code)


write.csv(koppen_modis_lookup_table, file = "data/out/koppen-modis-landcover-lookup-table.csv", row.names = FALSE)

if (file.exists("data/out/zero-goes-af-vpd-thresholds.csv")) {
  vpd_thresholds <- read.csv("data/out/zero-goes-af-vpd-thresholds.csv")
}

vpd_thresholds_with_lc <- 
  vpd_thresholds %>% 
  dplyr::left_join(koppen_modis_lookup_table, by = c(lc_name = "koppen_modis_name"))

write.csv(vpd_thresholds_with_lc, file = "data/out/zero-goes-af-vpd-thresholds-with-landcover-codes.csv", row.names = FALSE)
