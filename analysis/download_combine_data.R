library(here)
library(tidyverse)

#process data
source(here("osf.r"))

#specify OSF path
osf_path <-  "5v8zt"
file_name <- "massive_memory"
data_directory <- here("..","data")
raw_data_directory <- here("..","data","raw_data")
processed_data_directory <- here("..","data","processed_data")

#download data
#create data folder if it does not yet exist
if (!dir.exists(data_directory)) {
  dir.create(data_directory)
} else {
  print("Data Directory already exists!")
}

#as long as there isn't already an existing folder
#download the raw data
if (!dir.exists(raw_data_directory)) {
  get_raw_data(osf_path)
} else {
  print("Raw Data Directory already exists!")
}

#as long as there isn't already an existing folder
#create a processed data folder
if (!dir.exists(processed_data_directory)) {
  dir.create(processed_data_directory) 
} else {
  print("Processed Data Directory already exists!")
}

#merge data
merge_data <- function(raw_data_directory,merged_data_path) {
  
  #collect raw data files
  raw_files <- here(raw_data_directory, dir(here(raw_data_directory), "*.csv"))
  
  #read data
  raw_data <- map_df(raw_files, function(file) {
    d <- read_csv(file) %>% 
      mutate(
        file_name = file 
      ) %>%
      mutate(
        participant=as.character(participant)
      )
  }) 
  
  #keep cleaned data for verifying participation
  write_csv(raw_data, here(merged_data_path,paste0(file_name,"-alldata.csv")))
  
  return(raw_data)
}

#execute the merge
merged_data <- merge_data(raw_data_directory, processed_data_directory)
