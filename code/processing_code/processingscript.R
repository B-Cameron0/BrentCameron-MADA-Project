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

#we will not take a look at the data
dplyr::glimpse(rawdata)

#We will examine the data more closely to ensure everything is working properly
View(rawdata)

#Because there are several variables that will not be needed including cancer 
#sites code, age group code, sex code, race code, and ethnicity code, we will 
#filter out the non-desired variables by selecting only the ones we need 
#(the predictors of interest)

#The outcome of interest will be Cancer deaths
#Variables that will serve as predictors of interest will include...
#sex,age group, race, ethnicity, and cancer sites

#Select only the variables we are interested in to remove the rest
processeddata <- rawdata %>% 
  select("Age Group", "Sex", "Deaths", "Ethnicity", "Race", "Cancer Sites")

#rename Age Group to Age allow for easier data processing
colnames(processeddata) <- c("Age", "Sex", "Deaths", "Ethnicity", "Race", "CancerSite")

#Check to make sure new column name is reflected
colnames(processeddata)

#Make sure the processed data looks right
dplyr::glimpse(processeddata)

#One last check
View(processeddata)

#We will now save the processed data 
save_data_location <- here::here("data","processed_data", "processeddata.rds")

saveRDS(processeddata, file = save_data_location)

