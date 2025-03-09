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





