options(dplyr.summarise.inform = FALSE)
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(leaflet)
library(ggplot2)
library(plotly)
library(sf)

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

bb_nnc = st_as_sf(readRDS(file.path("data", "bb_nnc_sub.rds"))) |> 
  # ENR is not unique so can't use it alone as ID
  mutate(ENR_WBID = paste(ENR, WBID),
         ENR_WBID_base = paste(ENR_WBID, "base"))

enrs = c("", sort(unique(bb_nnc$ENR)))

ws = read_sf(file.path("data", "Watershed_Biscayne_Bay.shp")) |>
  st_transform(crs = 4326)

refline = left_join(readRDS(file.path("data", "refline.rds")), params_ts_df, 
                    by = join_by(masterCode))

basemap = leaflet(options = leafletOptions(attributionControl = FALSE)) |>
  setView(lng = -80.27, lat = 25.61, zoom = 9) |>
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") |> 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |> 
  addLayersControl(baseGroups = c("Topo", "Satellite"),
                   options = layersControlOptions(collapsed = FALSE)) |> 
  addPolygons(data = ws,
              label = "Biscayne Bay Watershed",
              color = "black",
              fillOpacity = 0) 

jackknife_station = readRDS(file.path("data", "jackknife_station.rds"))

breaks_jack = c(-500, -15, -10, 10, 15, 500)
# Define a color palette with custom breaks
cp_jack <- colorBin(palette = c("red", "orange", "yellow", "lightblue", "blue"), 
                    domain = jackknife_station$pbias, 
                    bins = breaks_jack)

periods = c("2016-2023", "2009-2015", "1995-2008", "Pre 1995")
params_jack = setNames(c("TP", "TN", "TKN", "CHLAC", "NO3O2"),
                       c("Total Phosphorous", "Total Nitrogen", 
                         "Total Kjeldahl Nitrogen", "Chlorophyll a", 
                         "Nitrate"))
