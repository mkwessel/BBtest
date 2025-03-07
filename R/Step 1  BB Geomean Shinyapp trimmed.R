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

logmeans<-work_br|>
  mutate(lresult=log(medresult))|>
  group_by(WBID,Period,year,masterCode)|>
  summarise(lmean=mean(lresult,na.rm=TRUE),
            .groups = 'drop'
            )

expmeans<-logmeans|>
  mutate(geo_mean=exp(lmean))


# Bring in shape for left hand side - also has reference line values. 
BB_NNC<-st_as_sf(readRDS("BB_NNC.rds"))
leaflet() %>%
  addTiles() %>%
  addPolygons(data = BB_NNC, 
              fillColor = "blue", 
              weight = 1, 
              opacity = 1, 
              color = "blue", 
              fillOpacity = 0.1,
              popup = ~WBID)

# get reference line values
refline<-BB_NNC|>
  select(WBID,ENR, TN, TP, CHLAC)|>
  select(-c(NNC_GEOM))|>  ## not sure why it doesnt drop the geometry?
  st_drop_geometry() |>
  pivot_longer(cols=c(TN,TP,CHLAC), names_to="masterCode",values_to ="Refline")
 

addline<-left_join(expmeans,refline,by=c("WBID","masterCode"))|>
  filter(masterCode %in% c("TN","TP","CHLAC"))
  

# test plot
onewbid<-addline|>
filter(WBID =="3226G3")

unique_params<-unique(onewbid$masterCode)
unique_levels<-unique(onewbid$WBID)  
  
  
# Define a function to create and save the map with enhancements
create_plot <- function(level, param) {
  ggplot(oneoff) +
    geom_bar(aes(x = year, y = geo_mean), stat = "identity", fill="blue") +
 #   geom_hline(yintercept = Refline, linetype = "dashed", color = "red") +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.background = element_rect(fill = "white", color = "blue", size = 0.5)
    ) +
    labs(
      title = paste(level, param), 
      x = "Year", 
      y = "Geometric Average"
    )
}

# Loop through each combination of Param and Level

  for (level in unique_levels) {
    for (param in unique_params) {
    oneoff<-onewbid|>
      filter(WBID == level  & masterCode == param ) 
    
    
    # Generate the map with enhancements
    points <- create_plot( level, param)
    print(points)
    
    # Save the plot as a PDF
    ggsave(filename = paste0("plot_", level, "_", param, ".pdf"), plot = points, width = 8, height = 6)
  }
}















# Create a list of unique sta within each WBID and Period
IWR_sta <- IWR_BB|>
  group_by(WBID,Period,year) |>
  distinct(sta)|>
  ungroup()|>
  arrange(WBID, Period, year,sta )

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
          
          # Calculate the annual averages by WBID, Period, and year
          annual_averages <- remaining_data |>
            group_by(WBID, Period, year) |>
            summarise(
              meanresult = mean(result, na.rm = TRUE),
              .groups = 'drop'
            )

          # Exponentiate the meanresult to get back to the original scale
          annual_averages <- annual_averages |>
            mutate(meanresult = exp(meanresult))|>
            ungroup()|>
            droplevels()

          # Calculate the grand average of the annual averages by WBID and Period
          summarised_data <- annual_averages |>
            group_by(WBID,Period) |>
            summarise(
              n = n(),
              mean = mean(meanresult, na.rm = TRUE),
              se = sd(meanresult, na.rm = TRUE) / sqrt(n()),
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
IWR_TN <- IWR_BB |>
  filter(masterCode == "CHLAC") |>
  IWR_jack()


# View the result
head(IWR_TN)

# test the function - it works! But stations are not always present
 test<-IWR_BB|>
   filter(masterCode == "CHLAC" & WBID == "3226G3" & Period == "NNC" & sta != "21FLBBAPBB14")|>
  group_by(WBID, Period,year) |>
   summarize(n=n(),
             meanresult=mean(result, na.rm = TRUE),.groups = 'drop')|>
   ungroup()
 
 test<-test|>
   mutate(meanresult=exp(meanresult))
          
  avg<-test|>
    group_by (WBID, Period) |>
    summarize(n=n(),
              mean=mean(meanresult, na.rm = TRUE),
              se=sd(meanresult, na.rm = TRUE)/sqrt(n()) )

# create true mean
IWR_true <- IWR_BB |>
  filter(masterCode == "CHLAC") |>
  group_by(WBID, Period, year) |>
  summarize(n_true=n(),
            true_mean=mean(result, na.rm = TRUE,.groups = 'drop'))|>
              ungroup()
       
IWR_TRUE<-IWR_true|>
  mutate(true_mean=exp(true_mean))|>
  group_by(WBID, Period) |>
  summarize(n=n(),
            true_Gmean=mean(true_mean, na.rm = TRUE),
            true_Gse=sd(true_mean, na.rm = TRUE)/sqrt(n()) )

IWR_all<-IWR_TN|>
  left_join(IWR_TRUE, by=c("WBID", "Period"))|>
  mutate(pbias = ((mean-true_Gmean)/true_Gmean)*100,
         se_pbias = ((se-true_Gse)/true_Gse)*100) 


# get dropped station mean
IWR_stamean <- IWR_BB |>
  filter(masterCode == "CHLAC")|>
group_by(WBID, Period,year,sta,) |>
  summarize(sta_n=n(),
            sta_mean=mean(result, na.rm = TRUE,.group="drop"))|>
      ungroup() 

IWR_stamean<-IWR_stamean|>
  mutate(sta_mean=exp(sta_mean))|>
  group_by(WBID,Period,sta) |>
  summarize(n=n(),
            sta_Gmean=mean(sta_mean, na.rm = TRUE),
            sta_Gse=sd(sta_mean, na.rm = TRUE)/sqrt(n()))
 
IWR_stamean<-IWR_stamean|>
  rename(dropped_sta=sta)
      
#

IWR_all<-IWR_all|>
  left_join(IWR_stamean, by=c("WBID", "Period", "dropped_sta"))|>
  mutate(pbias_sta = ((sta_Gmean-true_Gmean)/true_Gmean)*100,
         se_pbias_sta = ((sta_Gse-true_Gse)/true_Gse)*100) |>
  filter(pbias != 0)


latlons<-latlons|>
  rename(dropped_sta=sta)

IWR_all<-IWR_all|>
  left_join(latlons, by=c("WBID", "dropped_sta"))
 
rm(IWR_BB)

IWR_NNC<-IWR_all|>
  filter(Period == "NNC")

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
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
