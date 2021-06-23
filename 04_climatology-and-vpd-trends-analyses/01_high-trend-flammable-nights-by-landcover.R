library(dplyr)
library(terra)
library(gt)
library(blastula)
library(data.table)

climatology_trend <- 
  terra::rast("data/out/climatology/nights_trend.nc") %>% 
  terra::rotate() %>% 
  as.data.frame(xy = TRUE) %>% 
  dplyr::rename(x_lc = "x", y_lc = "y") %>% 
  as.data.table()

# get landcover data ------------------------------------------------------

lc_burnable <- read.csv("data/out/zero-goes-af-vpd-thresholds-with-landcover-codes.csv")

lc_area_per_px <- 
  data.table::fread("data/out/area-per-lc-pixel.csv") %>% 
  dplyr::right_join(lc_burnable, by = c(lc = "koppen_modis_code")) %>% 
  dplyr::select(x_lc, y_lc, lc, area_m2, koppen_orig_modis_name)

lc_area <-
  read.csv("data/out/area-per-lc.csv") %>% 
  filter(lc %in% lc_burnable$koppen_modis_code)

# combine climatology with landcover

lc_climatology <- lc_area_per_px[climatology_trend, on = c("x_lc", "y_lc")]
lc_climatology <-
  lc_climatology %>% 
  dplyr::filter(complete.cases(.))

### Function

high_trend_flammable_nights_area <- function(lc_climatology, thresh) {
  # Subset to only landcovers we care about and only where trend >= thresh flammable
  # nights
  trend_df <- 
    lc_climatology %>%
    dplyr::filter(lc %in% lc_burnable$koppen_modis_code) %>% 
    dplyr::filter(nights >= thresh) %>% 
    dplyr::as_tibble()
  
  # Sum the pixel values for the subsetted data frame by landcover type
  # Join back to the data frame representing the total area of each landcover
  # calculate the % of area experiencing large increases in flammable nights
  # Join to data frame representing character labels of landcover types
  landcover_trends <-
    trend_df %>% 
    dplyr::group_by(lc) %>% 
    dplyr::summarize(gt_thresh_area_Mkm2 = sum(area_m2) / 1e12) %>% 
    dplyr::left_join(lc_area, by = "lc") %>%
    dplyr::mutate(area_Mkm2 = area_km2 / 1e6) %>% 
    dplyr::mutate(pct_big_trend = gt_thresh_area_Mkm2 / area_Mkm2) %>% 
    dplyr::left_join(lc_burnable, by = c(lc = "koppen_modis_code")) %>% 
    dplyr::select(lc, gt_thresh_area_Mkm2, area_Mkm2, pct_big_trend, lc_name)
  
  landcover_trends
  
  # Sum all high trend pixels per landcover
  # Sum all total area per landcover
  # calculate overall percent of burnable land experiencing high trends of 
  # increases in flammable nights
  overall_trend <-
    trend_df %>% 
    summarize(gt_thresh_area_Mkm2 = sum(area_m2) / 1e12) %>% 
    mutate(area_Mkm2 = sum(lc_area$area_km2) / 1e6) %>% 
    mutate(pct_big_trend = gt_thresh_area_Mkm2 / area_Mkm2) %>% 
    mutate(lc = NA, lc_name = "All burnable landcovers") %>% 
    dplyr::select(lc, gt_thresh_area_Mkm2, area_Mkm2, pct_big_trend, lc_name)
  
  overall_trend
  
  # Make a pretty table
  
  final_table <- 
    rbind(landcover_trends, overall_trend) %>% 
    dplyr::mutate(pct_big_trend = round(pct_big_trend * 100, 1)) %>% 
    setNames(c("Index", paste0(">=", thresh, " flammable nights area (Mkm^2)"), "Total area (Mkm^2)", paste0("Percent >=", thresh, " flammable nights (%)"), "Landcover")) %>% 
    gt()
  
  email_table <- gt::as_raw_html(final_table)
  email <- blastula::compose_email(body = md(email_table))
  
  print(email)
  
  return(overall_trend)
}


high_trend_flammable_nights_area(lc_climatology = lc_climatology, thresh = 14)
high_trend_flammable_nights_area(lc_climatology = lc_climatology, thresh = 10)
high_trend_flammable_nights_area(lc_climatology = lc_climatology, thresh = 7)
high_trend_flammable_nights_area(lc_climatology = lc_climatology, thresh = 1)
