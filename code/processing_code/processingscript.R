###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#The link for the data set used can be found here:
#https://wonder.cdc.gov/cancermort-v2017.HTML

#The following variables must be chosen in this order to replicate the data set:
#Group results by cancer sites and... Sex, Age Group, Race, and Ethnicity
#load needed packages. make sure they are installed.

library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","raw_data","United_States_Cancer_Mortality_1999-2017.xls")
                       

#load data. 
rawdata <-read_xls(data_location)

#take a look at the data
dplyr::glimpse(rawdata)

#Examine the data more closely to ensure everything is working properly
View(rawdata)

#The outcome of interest will be Cancer deaths
#Variables that will serve as predictors of interest will include...
#sex,age group, race, ethnicity, and cancer sites

processeddata <- rawdata %>% 
  select("Age Group", "Sex", "Deaths", "Ethnicity", "Race", "Cancer Sites")

#Make sure the processed data looks right

dplyr::glimpse(processeddata)

#One last check
View(processeddata)

# save data as RDS
# I suggest you save your processed and cleaned data as RDS or RDA/Rdata files. 
# This preserves coding like factors, characters, numeric, etc. 
# If you save as CSV, that information would get lost.
# See here for some suggestions on how to store your processed data:
# http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata

# location to save file
save_data_location <- here::here("data","processed_data", "processeddata.rds")

saveRDS(processeddata, file = save_data_location)

