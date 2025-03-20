library(dplyr)
library(sf)
library(haven)
library(stringr)
library(lubridate)
library(tidyr)

if (!file.exists(file.path("Data", "bb_watershed_iwr66_123024.sas7bdat"))){
  # ideally, we would use the Egnyte API to automate fetching large files
  # for now, though, we are manually moving them from Egnyte into the correct folder
  # the code below checks for the existence of the file in the project directory
  # and, if not found, points the user where to find them on Egnyte
  stop("bb_watershed_iwr66_123024.sas7bdat not found.
  Download the file from this URL: 
  https://oneesa.egnyte.com/navigate/file/d8364a06-166c-4982-9a9a-5b4af17329c4
  Place the downloaded file in BBTest/Data/")
} 

bb_nnc_sub = readRDS(file.path("bb-dashboard", "data", "bb_nnc_sub.rds"))

parms = c("TN", "CHLAC", "TP", "NO3", "NO2", "NO3O2", "TKN")

iwr_bb_tmp = read_sas(file.path("Data", "bb_watershed_iwr66_123024.sas7bdat"))

iwr_bb = iwr_bb_tmp |>
  filter(wbid %in% unique(bb_nnc_sub$WBID) & masterCode %in% parms & year < 2024) |>
  # add ENR to stations based on WBID
  left_join(select(st_drop_geometry(bb_nnc_sub), wbid = WBID, ENR, Segment)) |> 
  mutate(result = ifelse(rCode %in% c("U", "T"), result/2,
                         ifelse(rCode == "I", mdl, result)),
         period = case_when(year < 1995 ~ "Pre 1995",
                            year < 2009 ~ "1995-2008",
                            year < 2016 ~ "2009-2015",
                            year < 2024 ~ "2016-2023",
                            .default = NA_character_))

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
  rename(enr = ENR) |>
  group_by(enr, period, year, sta, date, masterCode) |>
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
  pivot_longer(cols = c("NO3O2", "TP", "TKN", "TN", "CHLAC"), 
               names_to = "masterCode", values_to = "medresult")
saveRDS(work_br, file.path("Data", "work_br.rds"))

logmeans = work_br |>
  mutate(lresult = log(medresult)) |>
  group_by(enr, period, year, masterCode)|>
  summarise(lmean = mean(lresult,na.rm=TRUE)) |> 
  ungroup() |> 
  mutate(geo_mean = exp(lmean))

write.csv(logmeans, file.path("bb-dashboard", "data", "logmeans.csv"), row.names = FALSE)

### all code below this point in original file has been deleted b/c
### either incorporated into shiny app code or not currently used
