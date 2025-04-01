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

stations_ts = readRDS(file.path("data", "Stations_TS.rds"))

stations_bb = readRDS(file.path("data", "Stations_BB.rds")) |>
  filter(WBID_sel)

year_min = min(logmeans$year, na.rm = TRUE)
year_max = max(logmeans$year, na.rm = TRUE)

enr_bb = st_as_sf(readRDS(file.path("data", "ENR_BB.rds"))) |>
  filter(ENR_sel) |> 
  mutate(ENR_base = paste(ENR, "base"))

enrs = sort(unique(enr_bb$ENR))

ws = readRDS(file.path("data", "Watershed_BB.rds"))

refline = readRDS(file.path("data", "refline.rds")) |>
  ungroup() |> 
  select(-WBID) |> 
  distinct() |> 
  left_join(params_ts_df, by = join_by(masterCode))

basemap = leaflet(options = leafletOptions(attributionControl = FALSE)) |>
  setView(lng = -80.27, lat = 25.61, zoom = 9) |>
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") |> 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |> 
  addLayersControl(baseGroups = c("Topo", "Satellite"),
                   options = layersControlOptions(collapsed = FALSE)) |> 
  addPolygons(data = ws,
              label = ~NAME,
              weight = 2,
              opacity = 1,
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
