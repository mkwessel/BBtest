library(dplyr)
library(sf)
library(haven)
library(stringr)
library(lubridate)
library(tidyr)

wbids = c("3226G3", "3226H1", "3226H2", "3226H5", "3226H6", "3226H3", "6001D", 
          "6001E", "6001H", "6001F", "6001G", "6001C", "6003", "6002")
parms = c("TN", "CHLAC", "TP", "NO3", "NO2", "NO3O2", "TKN")

# dataset is very large so not committing to git
# can be downloaded from Egnyte: https://oneesa.egnyte.com/navigate/folder/c8304c62-bf0b-4a11-b36e-12f806e15beb
iwr_bb_tmp = read_sas(file.path("Data", "bb_watershed_iwr66_123024.sas7bdat"))

iwr_bb = iwr_bb_tmp |>
  filter(wbid %in% wbids & masterCode %in% parms) |>
  mutate(result = ifelse(rCode %in% c("U", "T"), result/2,
                         ifelse(rCode == "I", mdl, result)),
         period = case_when(year > 2015 ~ "New",
                            year > 2008 ~ "Mid",
                            year > 1994 ~ "NNC",
                            .default = "Pre"))

# removing large file from memory
rm(iwr_bb_tmp)

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
result = iwr_bb |>
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
