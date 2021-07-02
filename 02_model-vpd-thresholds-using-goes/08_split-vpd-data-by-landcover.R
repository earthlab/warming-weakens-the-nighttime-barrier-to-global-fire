# split VPD data by landcover

library(tidyverse) ; library(sf); library(vroom); library(lubridate); library(data.table)

system("aws s3 sync s3://earthlab-amahood/night_fires/fired_polys data")

# effort (number of goes scenes per hour)
system("aws s3 cp s3://earthlab-mkoontz/goes16meta/sampling-effort-goes16.csv data/sampling-effort-goes16.csv")


# now vpd monsters =============================================================
system("aws s3 cp s3://earthlab-amahood/night_fires/luts.Rda data/luts.Rda")
load("data/luts.Rda")
system("aws s3 cp s3://earthlab-amahood/night_fires/sa_vpd_long_2017.csv data/sa_vpd_long_2017.csv")

sa_vpd <- vroom("data/sa_vpd_long_2017.csv") %>%
  mutate(fireID=as.character(fireID),
         nid = lut_saids[fireID],
         fireID=as.numeric(fireID),
         nid=as.numeric(nid))
vroom_write(sa_vpd, "data/sa_vpd_long_2017-nids.csv")
system("aws s3 cp data/sa_vpd_long_2017-nids.csv s3://earthlab-amahood/night_fires/sa_vpd_long_2017-nids.csv")
rm(sa_vpd)
gc()

system("aws s3 cp s3://earthlab-amahood/night_fires/na_vpd_long_2017.csv data/na_vpd_long_2017.csv")

na_vpd <- vroom("data/na_vpd_long_2017.csv") %>%
  mutate(fireID=as.character(fireID),
         nid = lut_naids[fireID],
         fireID=as.numeric(fireID),
         nid=as.numeric(nid))
vroom_write(na_vpd, "data/na_vpd_long_2017-nids.csv")
system("aws s3 cp data/na_vpd_long_2017-nids.csv s3://earthlab-amahood/night_fires/na_vpd_long_2017-nids.csv")
rm(na_vpd)

# joining vpd csvs, then splitting by landcover and koppen =====================

system("aws s3 cp s3://earthlab-amahood/night_fires/na_vpd_long_2017-nids.csv data/na_vpd_long_2017-nids.csv")
system("aws s3 cp s3://earthlab-amahood/night_fires/sa_vpd_long_2017-nids.csv data/sa_vpd_long_2017-nids.csv")
system("aws s3 sync s3://earthlab-amahood/night_fires/lc_splits data/lc_splits")

files <- list.files("data/", full.names = TRUE, pattern = "vpd_long_2017-nids.csv")
wh <- vroom(files) 

wh <- as.data.table(wh)

# wh <- wh %>% 
#   separate(hour, into = c("day", "hour"),sep="_",) # maybe do this in smaller chunks

vroom_write(wh, "data/wh_vpd.csv")
system("aws s3 cp data/wh_vpd.csv s3://earthlab-amahood/night_fires/wh_vpd.csv")


dir.create("data/vpd_lc")
fired_files <- list.files("data/lc_splits", pattern = ".gpkg", full.names = TRUE)
out_files<- list.files("data/vpd_lc", pattern = "csv")
# wh<- vroom("data/wh_vpd.csv")

date_correcting_number <- 2 # our vpd date extraction was off, fixing here

for(f in fired_files){
  out_fn <- str_replace(f, "lc_splits", "") %>%
    str_replace(".gpkg", "_vpds.csv") %>%
    str_replace_all("/", "") %>%
    str_replace("data", "")
  if(!file.exists(file.path("data","vpd_lc",out_fn))){
    firez<- st_read(f) 
    
    lut_dates <- firez$first_date_7 + date_correcting_number#add a +1 here to account for the discrepancy with john's vpd values
    names(lut_dates) <- firez$nid
    
    ids <- firez %>%
      pull(nid)
    
    # data.table way
    subsettt <- wh[nid %in% ids]
    subsettt[, c("day", "hour") := tstrsplit(day_hour, "_")]
    subsettt <- subsettt %>%
      mutate(first_date = lubridate::ymd(lut_dates[as.character(nid)]),
             date= first_date+as.numeric(day)) 
    
    fwrite(subsettt, file.path("data", "vpd_lc", out_fn))
    if(date_correcting_number==0){
    system(paste("aws s3 cp",
                 file.path("data", "vpd_lc", out_fn),
                 file.path("s3://earthlab-amahood", "night_fires","vpd_lc",out_fn)))
    }else{
      system(paste("aws s3 cp",
                   file.path("data", "vpd_lc", out_fn),
                   file.path("s3://earthlab-amahood", "night_fires",
                             paste0("vpd_lc", "_plus_",date_correcting_number),out_fn)))
      }
    
    rm(subsettt); rm(firez);gc()
  }
}
