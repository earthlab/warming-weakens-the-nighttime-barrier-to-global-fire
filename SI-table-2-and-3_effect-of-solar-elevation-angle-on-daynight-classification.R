library(dplyr)
library(sf)
library(terra)
library(data.table)

# Landcovers we deem "burnable"
lc_lookup <- fread("data/out/zero-goes-af-vpd-thresholds-with-landcover-codes.csv")

# How much does it matter whether we use the center of the sun's disc to calculate day/night
# (basing on solar elevation angle) vs. using the top of the sun's disc?
afd_files <- list.files("data/out/mcd14ml_analysis-ready/", full.names = TRUE)

afd <- lapply(X = afd_files, FUN = fread, colClasses = c(acq_time = "character"))
afd <- data.table::rbindlist(afd)
afd <- afd[type == 0 & confidence >= 10]

afd[, .(pct_solar_ang_gt_0 = length(which(dn_detect == "day")) / length(dn_detect)), by = (daynight)]
afd[, .(pct_solar_ang_gt_neg_0.265 = length(which(solar_ang > -0.265)) / length(solar_ang)), by = (daynight)]

# aggregate to lon lat ----------------------------------------------------

solar_ang_daynight_mismatch_lon_lat <- 
  afd[, .(n = .N,
          n_dn_detect_day = sum(dn_detect == "day"),
          n_dn_detect_night = sum(dn_detect == "night")), 
      by = .(lon = x_lc, lat = y_lc, lc, daynight)]


solar_ang_daynight_mismatch_lc <- 
  solar_ang_daynight_mismatch_lon_lat[, .(mismatch_count = ifelse(daynight == "D", 
                                                                  yes = sum(n_dn_detect_night), 
                                                                  no = sum(n_dn_detect_day)),
                                          total_count = sum(n)),
                                      by = .(daynight, lc)]

solar_ang_daynight_mismatch_lc <-
  solar_ang_daynight_mismatch_lc %>% 
  dplyr::mutate(pct_mismatch = 100 * mismatch_count / total_count) %>% 
  dplyr::left_join(lc_lookup, by = c(lc = "koppen_modis_code")) %>% 
  dplyr::filter(complete.cases(.)) %>% 
  dplyr::select(lc_name, lc, daynight, mismatch_count, total_count, pct_mismatch) %>% 
  dplyr::arrange(lc_name, daynight)

solar_ang_daynight_mismatch_lc

write.csv(x = solar_ang_daynight_mismatch_lc, file = "tables/daynight-solar-ang-mismatch-by-landcover_all.csv")

# lc_name  lc daynight mismatch_count total_count pct_mismatch
# 1:                                 Arid Croplands 212        D              0     1312912  0.000000000
# 2:                                 Arid Croplands 212        N              0      109411  0.000000000
# 3:                                Arid Grasslands 210        D              0     4376564  0.000000000
# 4:                                Arid Grasslands 210        N              0      762938  0.000000000
# 5:                           Arid Open Shrublands 207        D              0     1398734  0.000000000
# 6:                           Arid Open Shrublands 207        N            135      495496  0.027245427
# 7:                                  Arid Savannas 209        D              0      510243  0.000000000
# 8:                                  Arid Savannas 209        N             57      108635  0.052469278
# 9:                            Arid Woody Savannas 208        D              0       87705  0.000000000
# 10:                            Arid Woody Savannas 208        N            322       27271  1.180741447
# 11:                               Boreal Croplands 412        D              0     2100519  0.000000000
# 12:                               Boreal Croplands 412        N             27      109491  0.024659561
# 13:            Boreal Evergreen Needleleaf Forests 401        D              0      208663  0.000000000
# 14:            Boreal Evergreen Needleleaf Forests 401        N           9467      113460  8.343909748
# 15:                              Boreal Grasslands 410        D              0      856852  0.000000000
# 16:                              Boreal Grasslands 410        N           6520      148452  4.391992024
# 17:                                Boreal Savannas 409        D              0     1407681  0.000000000
# 18:                                Boreal Savannas 409        N          92554      356434 25.966658624
# 19:                          Boreal Woody Savannas 408        D              0     1321425  0.000000000
# 20:                          Boreal Woody Savannas 408        N          66698      477790 13.959689403
# 21: Equatorial Cropland Natural Vegetation Mosaics 114        D              0      533715  0.000000000
# 22: Equatorial Cropland Natural Vegetation Mosaics 114        N              0       50666  0.000000000
# 23:                           Equatorial Croplands 112        D              0     2129593  0.000000000
# 24:                           Equatorial Croplands 112        N              0      287210  0.000000000
# 25:         Equatorial Deciduous Broadleaf Forests 104        D              0     2105226  0.000000000
# 26:         Equatorial Deciduous Broadleaf Forests 104        N              0      202319  0.000000000
# 27:         Equatorial Evergreen Broadleaf Forests 102        D              0     5850366  0.000000000
# 28:         Equatorial Evergreen Broadleaf Forests 102        N              0      847783  0.000000000
# 29:                          Equatorial Grasslands 110        D              0     8309149  0.000000000
# 30:                          Equatorial Grasslands 110        N              0     1040689  0.000000000
# 31:                  Equatorial Permanent Wetlands 111        D              0      211799  0.000000000
# 32:                  Equatorial Permanent Wetlands 111        N              0       30271  0.000000000
# 33:                            Equatorial Savannas 109        D              0    20167710  0.000000000
# 34:                            Equatorial Savannas 109        N              0     1903748  0.000000000
# 35:                      Equatorial Woody Savannas 108        D              0     5827074  0.000000000
# 36:                      Equatorial Woody Savannas 108        N              0      513873  0.000000000
# 37:                            Temperate Croplands 312        D              0      985830  0.000000000
# 38:                            Temperate Croplands 312        N              0      127680  0.000000000
# 39:          Temperate Deciduous Broadleaf Forests 304        D              0      704854  0.000000000
# 40:          Temperate Deciduous Broadleaf Forests 304        N              0       49557  0.000000000
# 41:          Temperate Evergreen Broadleaf Forests 302        D              0     1173352  0.000000000
# 42:          Temperate Evergreen Broadleaf Forests 302        N              0      159193  0.000000000
# 43:         Temperate Evergreen Needleleaf Forests 301        D              0      135208  0.000000000
# 44:         Temperate Evergreen Needleleaf Forests 301        N              0       82128  0.000000000
# 45:                           Temperate Grasslands 310        D              0     2888246  0.000000000
# 46:                           Temperate Grasslands 310        N              0      240408  0.000000000
# 47:                             Temperate Savannas 309        D              0     4240447  0.000000000
# 48:                             Temperate Savannas 309        N              0      306332  0.000000000
# 49:                       Temperate Woody Savannas 308        D              0     2473224  0.000000000
# 50:                       Temperate Woody Savannas 308        N              5      165012  0.003030083


solar_ang_daynight_mismatch_lc_table <- 
  solar_ang_daynight_mismatch_lc %>% 
  dplyr::filter(mismatch_count > 0) %>% 
  dplyr::arrange(desc(pct_mismatch))

readr::write_csv(solar_ang_daynight_mismatch_lc_table, path = "tables/daynight-solar-ang-mismatch-by-landcover_just-landcovers-with-mismatches.csv")

# lc_name  lc daynight mismatch_count total_count pct_mismatch
# 1:                     Boreal Savannas 409        N          92554      356434 25.966658624
# 2:               Boreal Woody Savannas 408        N          66698      477790 13.959689403
# 3: Boreal Evergreen Needleleaf Forests 401        N           9467      113460  8.343909748
# 4:                   Boreal Grasslands 410        N           6520      148452  4.391992024
# 5:                 Arid Woody Savannas 208        N            322       27271  1.180741447
# 6:                       Arid Savannas 209        N             57      108635  0.052469278
# 7:                Arid Open Shrublands 207        N            135      495496  0.027245427
# 8:                    Boreal Croplands 412        N             27      109491  0.024659561
# 9:            Temperate Woody Savannas 308        N              5      165012  0.003030083

# by koppen --------------------------------------------------------------

solar_ang_daynight_mismatch_lon_lat[, koppen := substr(lc, start = 1, stop = 1)]

solar_ang_daynight_mismatch_koppen <- 
  solar_ang_daynight_mismatch_lon_lat[, .(mismatch_count = ifelse(daynight == "D", 
                                                                  yes = sum(n_dn_detect_night), 
                                                                  no = sum(n_dn_detect_day)),
                                          total_count = sum(n)),
                                      by = .(daynight, koppen)]

solar_ang_daynight_mismatch_koppen <-
  solar_ang_daynight_mismatch_koppen %>% 
  dplyr::filter(complete.cases(.)) 

write.csv(x = solar_ang_daynight_mismatch_koppen, file = "tables/daynight-solar-ang-mismatch-by-koppen_all.csv", row.names = FALSE)

# daynight koppen mismatch_count total_count
# 1:        D      3              0    13629379
# 2:        D      1              0    46403564
# 3:        D      2              0     8047423
# 4:        N      1              0     4956408
# 5:        D      4              0     7388627
# 6:        N      3              5     1203238
# 7:        N      2            523     1623366
# 8:        N      4         258236     1596051
# 9:        N      5           2772       13372
# 10:        D      5              0       74619

burnable_koppen <- 
  lc_lookup %>% 
  mutate(koppen = substr(koppen_modis_code, start = 1, stop = 1)) %>% 
  pull(koppen) %>% 
  unique()

solar_ang_daynight_mismatch_koppen_table <-
  solar_ang_daynight_mismatch_koppen %>% 
  dplyr::filter(mismatch_count > 0 & koppen %in% burnable_koppen) %>%
  dplyr::mutate(pct_mismatch = 100 * mismatch_count / total_count)

# daynight koppen mismatch_count total_count pct_mismatch
# 1:        N      3              5     1203238 4.155454e-04
# 2:        N      2            523     1623366 3.221701e-02
# 3:        N      4         258236     1596051 1.617968e+01

write.csv(x = solar_ang_daynight_mismatch_koppen_table, file = "tables/daynight-solar-ang-mismatch-by-koppen_just-koppen-with-mismatches.csv", row.names = FALSE)

# by latitude -------------------------------------------------------------

solar_ang_daynight_mismatch_lon_lat[, lat_round := round(lat / 5) * 5]

mismatch_by_lat <-
  solar_ang_daynight_mismatch_lon_lat[, .(mismatch_count = ifelse(daynight == "D", 
                                                                  yes = sum(n_dn_detect_night), 
                                                                  no = sum(n_dn_detect_day)),
                                          total_count = sum(n)),
                                      by = .(daynight, lat_round)]

write.csv(mismatch_by_lat, file = "tables/daynight-solar-ang-mismatch-by-lat_all.csv", row.names = FALSE)

mismatch_by_lat_table <-
  mismatch_by_lat %>% 
  dplyr::filter(mismatch_count > 0) %>%
  dplyr::mutate(pct_mismatch = 100 * mismatch_count / total_count)

write.csv(mismatch_by_lat_table, file = "tables/daynight-solar-ang-mismatch-by-lat_just-landcovers-with-mismatches.csv", row.names = FALSE)
# Latitude rounded to the nearest 5 degree
# daynight lat_round mismatch_count total_count pct_mismatch
# 1:        N        55           2491      322787    0.7717163
# 2:        N        60          66480      445789   14.9128848
# 3:        N        65         176324      376927   46.7793498
# 4:        N        70          18820       27591   68.2106484