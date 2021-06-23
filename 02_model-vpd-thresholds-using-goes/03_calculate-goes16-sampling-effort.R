# GOES 16 sampling effort

library(dplyr)
library(readr)

if(!file.exists("data/out/goes16-filenames.csv")) {
  source("01_get-goes16/get-af-metadata.R")
}

goes_meta <- read_csv(file = "data/out/goes16-filenames.csv")

goes_meta <-
  goes_meta %>% 
  mutate(rounded_datetime = round(scan_center_full, units = "hours")) %>% 
  group_by(rounded_datetime) %>% 
  summarize(n_scenes_per_hour = n())

write_csv(x = goes_meta, file = "data./out/sampling-effort-goes16.csv")

system2(command = "aws", args = "s3 cp data/out/sampling-effort-goes16.csv s3://earthlab-mkoontz/goes16meta/sampling-effort-goes16.csv --acl public-read")