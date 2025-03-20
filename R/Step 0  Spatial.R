library(dplyr)
library(sf)
library(tidyr)

# the list below removes Card Sound wbids ("6003", "6002", "ENRH2")
# there is also "ENRY7" and WBID 3226G3 that is bisected by the northern boundary 
enrs = c("ENRH1", "ENRH3", "ENRH4", "ENRH5", "ENRH6", "ENRH7", "ENRH8", "ENRH9", "ENRY7")

# these polygons are based on WBID but not combining them to ENRs for now
bb_nnc_sub <- readRDS(file.path("Data", "BB_NNC.rds")) |> 
  filter(ENR %in% enrs) |> 
  select(Segment, ENR, WBID, ENR, TN, TP, CHLAC)

bb_nnc_sub |>
  st_drop_geometry() |> 
  pivot_longer(cols = c(TN, TP, CHLAC), names_to = "masterCode", values_to = "refline") |> 
  saveRDS(file.path("bb-dashboard", "data", "refline.rds"))

# station locations
stations <- read_sf(file.path("Data", "IWR_Stations_Run66.shp")) |>
  st_transform(crs = 4326) |>
  select(sta = STATION_ID, WBID = WATERBODY_, lat = LATITUDE, lon = LONGITUDE, geometry) |> 
  filter(WBID %in% unique(bb_nnc_sub$WBID)) |> 
  # add ENR to stations based on WBID
  left_join(select(st_drop_geometry(bb_nnc_sub), WBID, ENR, Segment))
saveRDS(stations, file.path("Data", "BB_ENR_Stations.rds"))
