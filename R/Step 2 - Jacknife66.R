rm(list = ls())

librarian::shelf( tidyverse, dplyr, ggplot2,sf,haven,zoo,leaflet,kableExtra, sf, leaflet)

path<-"Z:/Shared/Projects/00 - Legacy Firm - Office/JANICKI/DJ20232030.00 Biscayne Bay RAP/Data/Surface Water Quality/Compilation"
path2<- "Z:/Shared/Projects/00 - Legacy Firm - Office/JANICKI/DJ20232030.00 Biscayne Bay RAP/Data/GIS"


#WBID list

wbids<-as.list(c("3226G3","3226H1","3226H2","3226H5","3226H6", "3226H3", "6001D","6001E", "6001H","6001F", "6001G", "6001C", "6003","6002"))
parms<-as.list(c("TN","CHLAC","TP","NO3","NO2","NO3O2","TKN"))

# read in IWR SAS dataset
IWR_BB<-read_sas(file.path(path,"bb_watershed_iwr66_123024.sas7bdat"))|>
  rename(WBID=wbid)|>
  filter(WBID %in% wbids & masterCode %in% parms)|>
  mutate(Period = case_when(
    1995 <= year & year <= 2008 ~ "NNC",
    2009 <= year & year <= 2015 ~ "Mid",
    year >= 2016 ~ "New",
    TRUE ~ "Pre"
  ),
  result=case_when(rCode %in% c("U","T")~result/2,
                   rCode == c("I")~mdl,
                   TRUE ~ result))



# Function to check disqualification
check_disqual <- function(code, comment, chars) {
  disqual <- str_detect(code, str_c(chars, collapse = "|"))
  disqual2 <- str_detect(comment, str_c(chars, collapse = "|"))
  disqual | disqual2
}

# Define the characters to check for disqualification
chars1 <- c("V", "Y", "\\?", "G", "N", "O", "F", "L", "Z")
chars2 <- c("Q", "V", "Y", "\\?", "G", "N", "O", "F", "L", "Z")

# Process the data
result <- IWR_BB %>%
  mutate(
    disqual = if_else(masterCode %in% c("FCOLI", "ECOLI", "ENCOC"),
                      check_disqual(rCode, newComment, chars1),
                      check_disqual(rCode, newComment, chars2))
  ) %>%
  mutate(
    output = if_else(disqual, "RECORDSDELETED", if_else(masterCode %in% c("FCOLI", "ECOLI", "ENCOC"), "work_FIB", "work_"))
  )

# take the median
work<-result|>
  filter(output=="work_")|>
  mutate(Date = as.Date(mdy(paste(month, day, year, sep = "-"))),Result=as.numeric(result))|>
  select(-c("c1","c2","c3","c4","cycle"))|>
  group_by(WBID, Period,year,sta,Date,masterCode)|>
  summarise(
    medresult = median(Result, na.rm = TRUE),
    .groups = 'drop'
  )

work_tr<-work|>
  pivot_wider(names_from=masterCode, values_from = medresult)|>
  mutate(TN2=case_when( TKN>0 & NO3O2>0 ~ TKN+NO3O2,
                        # TKN>0 & NO3>0 & NO2>0 ~ TKN+NO3O2,
                        #  TKN>0 & NO3>0 ~ TKN+NO3,
                        TKN>0 ~ TKN,
  ))|>
  mutate(TN=ifelse(is.na(TN),TN2,TN))|>
  select(-c(TN2))

work_br <- work_tr %>%
  pivot_longer(
    cols = (-c("WBID","Period","year","sta","Date")), 
    names_to = "masterCode",
    values_to = "medresult"
  )


# Create a list of unique sta within each WBID and Period
IWR_sta <- work_br |>
  group_by(WBID,Period,year) |>
  distinct(sta)|>
  ungroup()|>
  arrange(WBID, Period, year,sta )

##########################################################################

# Create a function to remove a station iteratively for each WBID and Period and calculate mean and standard error using jackknife estimation
IWR_jack <- function(data) {
  data |>
  #  group_by(WBID, Period, year) |>
    nest() |>
    mutate(
      jack = map(data, ~{
        nested_data <- .x
        unique_stations <- unique(nested_data$sta)
        
        map_dfr(unique_stations, function(station) {
          remaining_data <- nested_data |>
            filter(sta != station)  # Drop the current station
   
          annual_averages <- remaining_data|>
            mutate(lresult=log(medresult))|>
            group_by(WBID,Period,year,masterCode)|>
            summarise(lmean=mean(lresult,na.rm=TRUE),
                      .groups = 'drop'
            )
          
          expmeans<-annual_averages|>
            mutate(geo_mean=exp(lmean))|>
            ungroup()|>
            droplevels()

          # Calculate the grand average of the annual averages by WBID and Period
          summarised_data <- expmeans |>
            group_by(WBID,Period,masterCode) |>
            summarise(
              n = n(),
              mean = mean(geo_mean, na.rm = TRUE),
              se = sd(geo_mean, na.rm = TRUE) / sqrt(n()),
              .groups = 'drop'
            )
          
          summarised_data <- summarised_data |>
            mutate(dropped_sta = station)
          
          return(summarised_data)
        })
      })
    ) |>
    unnest(jack)
}


# Apply this function to the IWR_BB dataset where param = TN
IWR_jack <- work_br |>
   IWR_jack()

# View the result
head(IWR_jack)

#################################################################################################
# test the function - it works! But stations are not always present
 test<-work_br |>
   filter(masterCode == "CHLAC" & WBID == "3226G3" & Period == "NNC" & sta != "21FLBROW39")|>
   mutate(lresult=log(medresult))|>
  group_by(WBID, Period,year) |>
   summarise(lmean=mean(lresult,na.rm=TRUE),
             .groups = 'drop'
   )

 test<-test|>
   mutate(geo_mean=exp(lmean))
          
  avg<-test|>
    group_by (WBID, Period) |>
    summarize(n=n(),
              mean=mean(geo_mean, na.rm = TRUE),
              se=sd(geo_mean, na.rm = TRUE)/sqrt(n()), .groups = 'drop')

  ###############################################################################
# create true mean
  
  calc_truemean<-work_br|>
      mutate(lresult=log(medresult))|>
      group_by(WBID,Period,year,masterCode)|>
      summarise(lmean=mean(lresult,na.rm=TRUE),
                .groups = 'drop'
      )
    
    expmeans<-calc_truemean|>
      mutate(geo_mean=exp(lmean))|>
      ungroup()|>
      droplevels()
    
    # Calculate the grand average of the annual averages by WBID and Period
    grandmean <- expmeans |>
      group_by(WBID,Period,masterCode) |>
      summarise(
        n = n(),
        t_mean = mean(geo_mean, na.rm = TRUE),
        t_se = sd(geo_mean, na.rm = TRUE) / sqrt(n()),
        .groups = 'drop'
      )
    
 
#######################################################################
### Calculate Percent Bias and MSE  
 
IWR_all<-IWR_jack|>
  left_join(grandmean, by=c("WBID", "Period","masterCode"))|>
  mutate(pbias = ((mean-t_mean)/t_mean)*100,
         se_pbias = ((se-t_se)/t_se)*100) 

########################################
# get dropped station mean
IWR_stamean <- work_br |>
      mutate(lresult=log(medresult))|>  
      group_by(WBID, Period,year,sta,masterCode) |>
            summarise(lmean=mean(lresult,na.rm=TRUE),
                .groups = 'drop'
      )

IWR_stamean<-IWR_stamean|>
  mutate(sta_mean=exp(lmean))|>
  group_by(WBID,Period,sta,masterCode) |>
  summarize(n=n(),
            sta_Gmean=mean(sta_mean, na.rm = TRUE),
            sta_Gse=sd(sta_mean, na.rm = TRUE)/sqrt(n()))
 
IWR_stamean<-IWR_stamean|>
  rename(dropped_sta=sta)
      
#
IWR_all<-IWR_all|>
  left_join(IWR_stamean, by=c("WBID", "Period", "dropped_sta","masterCode"))|>
  mutate(pbias_sta = ((sta_Gmean-t_mean)/t_mean)*100,
         se_pbias_sta = ((sta_Gse-t_se)/t_se)*100) |>
  filter(pbias != 0)

#########################################################
### get lat lons
#########################################################
latlons<-read_sf(file.path(path2,"IWRSTA_Run66","IWR_Stations_Run66.shp"))|>
  filter(WATERBODY_ %in% c("3226G3","3226H1","3226H2","3226H5","3226H6", "3226H3", "6001D","6001E", "6001H","6001F", "6001G", "6001C", "6003","6002"))|>
  rename (dropped_sta=STATION_ID,lat=LATITUDE,lon=LONGITUDE, WBID=WATERBODY_)|>
  select(WBID, dropped_sta, lat, lon, geometry)|>
  st_transform(crs=4326) 

IWR_all<-IWR_all|>
  left_join(latlons, by=c("WBID", "dropped_sta"))
 
saveRDS(IWR_all,"jacknife_station.rds")

BB_WBIDS <- read_sf(file.path(path2,"WBIDs_Run66","WBIDs_Run66.shp"))|>
  st_transform(crs=4326)|>
  filter(WBID %in% wbids)


#################################################################################################
### This is where the dashboard would take over and read in jacknife_station.rds instead of IWR_all
##################################################################################################

IWR_NNC<-IWR_all|>
  filter(Period == "New" & masterCode == "TN")   ## Period and Mastercode would be the reactive components

# Keep Red - Blue means dropping that station increases the mean relative to the true mean

# Define custom breaks
breaks <- c(-Inf, -10, 0, 10, 20, Inf)

# Define a color palette with custom breaks
color_palette <- colorBin(palette = c("blue", "lightblue", "yellow", "orange", "red"), 
                          domain = IWR_NNC$pbias, 
                          bins = breaks)

leaflet() %>%
 # addTiles() %>%
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addPolygons(data = BB_WBIDS, 
              fillColor = "white",  fillOpacity = 0,
              weight = 1, 
              opacity = 1, 
              color = "blue", 
             
              popup = ~WBID) %>%
  addCircleMarkers(data = IWR_NNC, 
                   popup = ~paste("Nyrs:", round(n,0),"%Bias:", round(pbias, 2), "%SE Bias:", round(se_pbias, 2)), 
                   label = ~dropped_sta, 
                   radius = 2, 
                   color = ~color_palette(pbias))
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
