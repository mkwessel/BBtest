
# ideally, we would use the Egnyte API to automate fetching these large files
# for now, though, we are manually moving them from Egnyte into the correct folder
# the code below checks for the existence of the files in the project directory
# and, if not found, points the user where to find them on Egnyte

if (!dir.exists("Data")) dir.create("Data")

if (!file.exists(file.path("Data", "bb_watershed_iwr66_123024.sas7bdat"))){
  stop("bb_watershed_iwr66_123024.sas7bdat not found.
  Download the file from this URL: 
  https://oneesa.egnyte.com/navigate/file/d8364a06-166c-4982-9a9a-5b4af17329c4
  Place the downloaded file in BBTest/Data/")
} 

shape_dir = file.path("Data", "shapefiles")
if (!dir.exists(shape_dir)) dir.create(shape_dir)

if (!file.exists(file.path("Data", "shapefiles", "IWR_Stations_Run66.shp"))){
  stop("IWR_Stations_Run66.shp not found.
  Download the files in the folder at this URL: 
  https://oneesa.egnyte.com/navigate/folder/0b9cc904-7841-4ddd-8291-34ede525ef00
  Place the downloaded files in BBTest/Data/shapefiles")
} 

if (!file.exists(file.path("Data", "shapefiles", "WBIDs_Run66.shp"))){
  stop("WBIDs_Run66.shp not found.
  Download the files in the folder at this URL: 
  https://oneesa.egnyte.com/navigate/folder/7f73a568-350b-43ac-a539-690c43f1916b
  Place the downloaded files in BBTest/Data/shapefiles")
} 
