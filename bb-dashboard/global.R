options(dplyr.summarise.inform = FALSE)
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(leaflet)
library(ggplot2)
library(plotly)

params_ts = setNames(c("TP", "TN", "CHLAC"),
                     c("Total Phosphorous (mg/L)", "Total Nitrogen (mg/L)", "Chlorophyll a (ug/L)"))

params_ts_df = data.frame(masterCode = unname(params_ts),
                          param = names(params_ts)) |> 
  mutate(param = factor(param, levels = names(params_ts)))

logmeans = read.csv(file.path("data", "logmeans.csv")) |> 
  filter(masterCode %in% params_ts) |> 
  left_join(params_ts_df, by = join_by(masterCode))

year_min = min(logmeans$year, na.rm = TRUE)
year_max = max(logmeans$year, na.rm = TRUE)

wbids = c("", sort(unique(logmeans$wbid)))

bb_nnc = sf::st_as_sf(readRDS(file.path("data", "BB_NNC.rds"))) |> 
  mutate(WBID_base = paste("Base", WBID))

ws<-read_sf(file.path("data","Watershed_Biscayne_Bay.shp"))|>
  st_transform(crs=4326)
  

# get reference line values
refline = bb_nnc |>
  sf::st_drop_geometry() |>
  select(wbid = WBID, enr = ENR, TN, TP, CHLAC) |>
  pivot_longer(cols = c(TN, TP, CHLAC), names_to = "masterCode", values_to = "refline") |> 
  left_join(params_ts_df, by = join_by(masterCode))

basemap = leaflet(options = leafletOptions(attributionControl = FALSE)) |>
  setView(lng = -80.231, lat = 25.61, zoom = 10) |>
  addProviderTiles(providers$Esri.WorldTopoMap) |>
  addPolygons(data=ws)

jackknife_station = readRDS(file.path("data", "jackknife_station.rds"))
bb_wbid = readRDS(file.path("data", "bb_wbid.rds"))

breaks_jack = c(-500, -15,-10, 10, 15, 500)
# Define a color palette with custom breaks
cp_jack <- colorBin(palette = c("red", "orange", "yellow", "lightblue", "blue"), 
                    domain = jackknife_station$pbias, 
                    bins = breaks_jack)

periods = c("Pre 1995", "1995-2008", "2009-2016", "2017-2023")
params_jack =  setNames(c("TP", "TN", "TKN", "CHLAC", "NO3O2"),
                                   c("Total Phosphorous", "Total Nitrogen", 
                                     "Total Kjeldahl Nitrogen", "Chlorophyll a", 
                                     "Nitrate"))
