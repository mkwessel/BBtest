library(dplyr)
library(sf)
library(tidyr)
library(purrr)

# work_br created in 'Step 1  BB Geomean Shinyapp trimmed TMH.R'
work_br = readRDS(file.path("Data", "work_br.rds"))

# Create a list of unique sta within each WBID and Period
iwr_sta <- work_br |>
  group_by(ENR, period, year) |>
  distinct(sta) |>
  ungroup() |>
  arrange(ENR, period, year, sta)

##########################################################################

# to remove a station iteratively for each ENR and Period and calculate mean and standard error using jackknife estimation

work_br_split = split(work_br, ~ ENR + period + masterCode, drop = TRUE)

expmeans_jack = map(work_br_split, function(dfx){
  unique_stations <- unique(dfx$sta)
  map(unique_stations, function(station) {
    remaining_data <- dfx |>
      filter(sta != station)  # Drop the current station
    
    annual_averages <- remaining_data |>
      mutate(lresult = log(medresult)) |>
      group_by(ENR, period, masterCode, year) |>
      summarise(lmean = mean(lresult, na.rm = TRUE),
                .groups = 'drop')
    
    annual_averages |>
      mutate(geo_mean = exp(lmean)) |>
      ungroup() |>
      mutate(dropped_sta = station)
  }) |> 
    list_rbind()
}) |> 
  list_rbind()

# Calculate the grand average of the annual averages by ENR and Period
iwr_jack <- expmeans_jack |>
  group_by(ENR, period, masterCode, dropped_sta) |>
  summarise(
    n = sum(!is.na(geo_mean)),  # n() counts all rows, but we are dropping NAs so don't want to count them
    mean = mean(geo_mean, na.rm = TRUE),
    se = sd(geo_mean, na.rm = TRUE) / sqrt(n),
    .groups = 'drop') |> 
  filter(n > 0)

###############################################################################
# create true mean

calc_truemean <- work_br |>
  mutate(lresult = log(medresult)) |>
  group_by(ENR, period, year, masterCode) |>
  summarise(lmean=mean(lresult, na.rm=TRUE),
            .groups = 'drop')

expmeans <- calc_truemean |>
  mutate(geo_mean = exp(lmean)) |>
  ungroup()

# Calculate the grand average of the annual averages by ENR and Period
grandmean <- expmeans |>
  group_by(ENR, period, masterCode) |>
  summarise(
    t_n = sum(!is.na(geo_mean)),
    t_mean = mean(geo_mean, na.rm = TRUE),
    t_se = sd(geo_mean, na.rm = TRUE) / sqrt(t_n),
    .groups = 'drop') |> 
  filter(t_n > 0)

#######################################################################
### Calculate Percent Bias and MSE  

iwr_all <- iwr_jack |>
  left_join(grandmean, by = c("ENR", "period", "masterCode")) |>
  mutate(pbias = ((mean - t_mean)/t_mean) * 100,
         se_pbias = ((se - t_se)/t_se) * 100) 

########################################
# get dropped station mean
iwr_stamean <- work_br |>
  mutate(lresult = log(medresult)) |>  
  group_by(ENR, period, year, sta, masterCode) |>
  summarise(lmean = mean(lresult,na.rm=TRUE),
            .groups = 'drop') |>
  mutate(sta_mean=exp(lmean)) |>
  group_by(ENR, period, sta, masterCode) |>
  summarize(sta_n = sum(!is.na(sta_mean)),
            sta_Gmean = mean(sta_mean, na.rm = TRUE),
            sta_Gse = sd(sta_mean, na.rm = TRUE)/sqrt(sta_n)) |>
  rename(dropped_sta=sta)

iwr_all <- iwr_all |>
  left_join(iwr_stamean, by = c("ENR", "period", "dropped_sta", "masterCode"))|>
  mutate(pbias_sta = ((sta_Gmean - t_mean)/t_mean) * 100,
         se_pbias_sta = ((sta_Gse - t_se)/t_se) * 100) |>
  filter(pbias != 0)

# include only stations inside the watershed and selected ENRs
# also includes stations that are outside of the estuary
stations_bb = readRDS(file.path("Data", "Stations_BB.rds")) |> 
  filter(!is.na(Watershed) & ENR_sel)
  # geometry will be lost in left_join below so dropping it here
  st_drop_geometry()

iwr_sf <- left_join(iwr_all, stations_bb, by = c("ENR", "dropped_sta" = "Station")) |> 
  filter(!(is.na(Lat) | is.na(Lon))) |> 
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)

saveRDS(iwr_sf, file.path("bb-dashboard", "data", "jackknife_station.rds"))

