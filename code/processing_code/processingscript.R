###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#The link for the data set used can be found here:
#https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data-with-Ge/n8mc-b4w4

#load needed packages. make sure they are installed.
library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(utils) #to read csv file

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("MADA","BrentCameron-MADA-analysis2","data","raw_data","COVID-19_Case_Surveillance_Public_Use_Data_with_Geography.csv")

#load data. 
rawdata <- utils::read.csv(data_location)

#take a look at the data
dplyr::glimpse(rawdata)

#Examine the data more closely to ensure everything is working properly
View(rawdata)

#The outcome of interest will be COVID-19 deaths
#Variables that will serve as predictors of interest will include...
#Ethnicity,age group, and whether or not there is an underlying condition

#Additionally, some variables (such as case month, and residents state) will 
#be included to assess whether there are noticeable differences in the data
#based on time and geography, particularly between states with varying amounts
#of minority racial groups, and to analyze the role of ethnicity in
#a resilience(or susceptibility) to COVID-19, possibly due to a good support
#system (or lack thereof) or other factors

processeddata <- rawdata %>% 
  select("death_yn", "ethnicity", "age_group", "underlying_conditions_yn",
         "case_month", "res_state")

#This is a large file with over 2 million observations, naturally, there are 
#many missing observations (N/A), in particular for recorded death and ethnicity
#To handle this, we will utilize pairwise deletion to omit all observations that
#have a missing value in either recorded death, or ethnicity depending on what 
#data testing we are doing at the time
#we can alternate if needed to gather more reliable data

#Pairwise deletion for missing death variable (death_yn)
processeddata[! is.na (processeddata$death_yn),]

#Pairwise deletion for missing ethnicity variables (ethnicity)
processeddata[! is.na (processeddata$ethnicity)]

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
save_data_location <- here::here("MADA","BrentCameron-MADA-analysis2","data",
                                 "raw_data","COVID-19_Case_Surveillance_Public_Use_Data_with_Geography.csv")

saveRDS(processeddata, file = save_data_location)

