options(dplyr.summarise.inform = FALSE)
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(leaflet)
library(ggplot2)
library(plotly)

params = setNames(c("TP", "TN", "CHLAC"),
                  c("Total Phosphorous", "Total Nitrogen", "Chlorophyll a"))

params_df = data.frame(masterCode = unname(params),
                       param = names(params)) |> 
  mutate(param = factor(param, levels = names(params)))

logmeans = read.csv(file.path("data", "logmeans.csv")) |> 
  filter(masterCode %in% params) |> 
  left_join(params_df, by = join_by(masterCode))

year_min = min(logmeans$year, na.rm = TRUE)
year_max = max(logmeans$year, na.rm = TRUE)

wbids = c("", sort(unique(logmeans$wbid)))

bb_nnc = sf::st_as_sf(readRDS(file.path("data", "BB_NNC.rds"))) |> 
  mutate(WBID_base = paste("Base", WBID))

# get reference line values
refline = bb_nnc |>
  sf::st_drop_geometry() |>
  select(wbid = WBID, enr = ENR, TN, TP, CHLAC) |>
  pivot_longer(cols = c(TN, TP, CHLAC), names_to = "masterCode", values_to = "refline") |> 
  left_join(params_df, by = join_by(masterCode))
