library(dplyr)
library(sf)
library(tidyr)

# the list below removes Card Sound wbids ("6003", "6002", "ENRH2")
# there is also "ENRY7" and WBID 3226G3 that is bisected by the northern boundary 
enrs = c("ENRH1", "ENRH3", "ENRH4", "ENRH5", "ENRH6", "ENRH7", "ENRH8", "ENRH9", "ENRY7")

bb_nnc <- readRDS(file.path("Data", "BB_NNC.rds"))

bb_nnc_sub <- readRDS(file.path("Data", "BB_NNC.rds")) |> 
  filter(ENR %in% enrs) |> 
  select(Segment, ENR, WBID, ENR, TN, TP, CHLAC)
saveRDS(bb_nnc_sub, file.path("bb-dashboard", "data", "bb_nnc_sub.rds"))

bb_nnc_sub |>
  st_drop_geometry() |> 
  pivot_longer(cols = c(TN, TP, CHLAC), names_to = "masterCode", values_to = "refline") |> 
  saveRDS(file.path("bb-dashboard", "data", "refline.rds"))

# station locations
stations <- read_sf(file.path("Data", "IWR_Stations_Run66.shp")) |>
  st_transform(crs = 4326) |>
  select(sta = STATION_ID, WBID = WATERBODY_, lat = LATITUDE, lon = LONGITUDE, geometry) |> 
  filter(WBID %in% unique(bb_nnc_sub$WBID)) |> 
  st_intersection(select(bb_nnc_sub, ENR))
saveRDS(stations, file.path("Data", "BB_ENR_Stations.rds"))

