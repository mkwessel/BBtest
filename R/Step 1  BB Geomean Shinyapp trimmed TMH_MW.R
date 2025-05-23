library(dplyr)
library(sf)
library(haven)
library(stringr)
library(lubridate)
library(tidyr)


### the list below removes Card sound wbids ( "6003","6002","ENRH2") . THere is also "ENRY7" and WBID 3226G3 that is bisected by the northern boundary 
ENRs<-c("ENRH1","ENRH3","ENRH4","ENRH5","ENRH6","ENRH7","ENRH8","ENRH9","ENRY7")
wbids<-c("3226G3","3226H1","3226H2","3226H5","3226H6", "3226H3", "6001D","6001E",
                 "6001H","6001F", "6001G", "6001C")
parms<-c("TN","CHLAC","TP","NO3","NO2","NO3O2","TKN")

pth<-"Z:/Shared/Projects/00 - Legacy Firm - Office/JANICKI/DJ20232030.00 Biscayne Bay RAP/Data/Surface Water Quality/Compilation"
  
# dataset is very large so not committing to git
# can be downloaded from Egnyte: https://oneesa.egnyte.com/navigate/folder/c8304c62-bf0b-4a11-b36e-12f806e15beb
#iwr_bb_tmp = read_sas(file.path("Data", "bb_watershed_iwr66_123024.sas7bdat"))
iwr_bb_tmp = read_sas(file.path(pth, "bb_watershed_iwr66_123024.sas7bdat"))

iwr_bb = iwr_bb_tmp |>
  filter(wbid %in% wbids & masterCode %in% parms & year < 2024)|> 
  mutate(result = ifelse(rCode %in% c("U", "T"), result/2,
                         ifelse(rCode == "I", mdl, result)),
         period = case_when(
           1995 <= year & year <= 2008 ~ "1995-2008",
           2009 <= year & year <= 2015 ~ "2009-2016",
           year >= 2016 ~ "2017-2023",
           TRUE ~ "Pre 1995"
         ))

# Unique stations within subset
run66_sta<-iwr_bb|>
  distinct(sta)

# station locations
points<-read_sf(file.path("Data","IWR_Stations_Run66.shp"))|>
  filter(WATERBODY_ %in% wbids)|>
  rename(sta=STATION_ID, WBID = WATERBODY_)|>
  select(sta, WBID,LATITUDE,LONGITUDE,geometry)

run66_points<-left_join(run66_sta,points,by="sta")
ENR_pnts<-st_as_sf(run66_points,coords=c("LONGITUDE","LATITUDE"),crs=4326)

# ENR shape - Read, transform, and select relevant columns from the NNC shapefile
NNC <- read_sf(file.path("Data", "Numeric_Nutrient_Criteria_(NNC)_-_Estuary_Nutrient_Regions.shp")) |>
  st_transform(crs = 4326) |>
  select(-c("OBJECTID", "WATERBODY_", "ESTUARY", "ESTUARY_RE", "RULE_CITAT",
            "CHLA_ASSES", "TN_ASSESSM", "TP_ASSESSM", "EFFECTIVE_", "EPA_APPROV", "NOTES", "RULE_CIT_1")) |>
  rename(Segment = SEGMENT_NA, CHLAC = CHLA_CRITE, TN = TN_CRITERI, TP = TP_CRITERI,ENR=ESTUARY_SE,NNC_SHAPEAREA = SHAPEAREA, NNC_SHAPELEN=SHAPELEN, NNC_GEOM=geometry) |>
  filter(ENR %in% ENRs)|>
  st_make_valid()

### Intersect 
within_sta<-st_intersection(ENR_pnts,NNC)

#Pull only points from WBIDS wihtin ENR boundaries
iwr_bb2<-data.frame(left_join(within_sta,iwr_bb,by="sta"))

# removing large file from memory
rm(iwr_bb_tmp,iwr_bb)

# Function to check disqualification
check_disqual = function(code, comment, chars) {
  disqual = str_detect(code, str_c(chars, collapse = "|"))
  disqual2 = str_detect(comment, str_c(chars, collapse = "|"))
  disqual | disqual2
}

# Define the characters to check for disqualification
chars1 = c("V", "Y", "\\?", "G", "N", "O", "F", "L", "Z")
chars2 = c("Q", "V", "Y", "\\?", "G", "N", "O", "F", "L", "Z")

# Process the data
result = iwr_bb2 |>
  mutate(disqual = if_else(masterCode %in% c("FCOLI", "ECOLI", "ENCOC"),
                           check_disqual(rCode, newComment, chars1),
                           check_disqual(rCode, newComment, chars2)),
         output = if_else(disqual, "RECORDSDELETED", 
                          if_else(masterCode %in% c("FCOLI", "ECOLI", "ENCOC"), 
                                  "work_FIB", 
                                  "work_")))

# take the median
work = result |>
  filter(output == "work_") |>
  mutate(date = mdy(paste(month, day, year, sep = "-")),
         result = as.numeric(result)) |>
  select(-c("c1", "c2", "c3", "c4", "cycle")) |>
  group_by(wbid, period, year, sta, date, masterCode)|>
  summarise(medresult = median(result, na.rm = TRUE)) |> 
  ungroup()

work_tr = work|>
  pivot_wider(names_from = masterCode, values_from = medresult) |>
  mutate(TN2 = case_when(TKN > 0 & NO3O2 > 0 ~ TKN + NO3O2,
                        # TKN>0 & NO3>0 & NO2>0 ~ TKN+NO3O2,
                        #  TKN>0 & NO3>0 ~ TKN+NO3,
                        TKN > 0 ~ TKN,
                        .default = NA_real_),
         TN = ifelse(is.na(TN), TN2, TN)) |>
  select(-TN2)

work_br = work_tr |>
  pivot_longer(cols = CHLAC:TP, names_to = "masterCode", values_to = "medresult")

# save work product for dashboard
saveRDS(work_br, file.path("Data", "work_br.rds"))
  
logmeans = work_br |>
  mutate(lresult = log(medresult)) |>
  group_by(wbid, period, year, masterCode)|>
  summarise(lmean = mean(lresult,na.rm=TRUE)) |> 
  ungroup() |> 
  mutate(geo_mean = exp(lmean))

write.csv(logmeans, file.path("bb-dashboard", "data", "logmeans.csv"), row.names = FALSE)

### all code below this point in original file has been deleted b/c
### either incorporated into shiny app code or not currently used

BB_NNC<-readRDS(file.path("Data","BB_NNC.rds"))|>
  filter(WBID %in% wbids & ENR %in% ENRs)
saveRDS(BB_NNC,"BB_NNC.rds")
