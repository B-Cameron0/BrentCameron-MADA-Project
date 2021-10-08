###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidyverse) #for help with managing data
library(scales) #for help with making data more digestible
library(lubridate) #for help with data
library(dbplyr) #backend for data
library(viridis) #for charts

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata<-readRDS(data_location)

######################################
#Data exploration/description
######################################

#Our outcome of interest is Cancer deaths
#Variables that will serve as predictors of outcome include...
#Age group, sex, total deaths, ethnicity, race, and cancer sites

#Plot 1, Plotting Cancer deaths by Ethnicity

plot_1 <- mydata  %>%
  ggplot(aes(x=Race, y=Deaths)) +
  geom_bar(stat = "identity") +
  ggtitle("Cancer Deaths by Race in the United States")+
  geom_smooth(method='lm')+
  scale_x_discrete(guide = guide_axis(n.dodge = 1, check.overlap = TRUE))+
  scale_y_continuous(labels = comma)

#Examine plot
plot(plot_1)

#Save the plot
figure_file1 = here::here("results","resultfigure1.png")
ggsave(filename = figure_file1, plot = plot_1)

#Plot 2, Plotting COVID-19 deaths by Age Group

plot_2 <-mydata %>%
  ggplot(aes(x=`Age Group`, y=Deaths))+
  geom_bar(stat = "identity") +
  ggtitle("Cancer Deaths by Age Group in the United States")+
  geom_smooth(method='lm')+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(guide = guide_axis(n.dodge = 1, check.overlap = TRUE))+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1)) 



#Examine the Plot
plot(plot_2)

#Save the plot
figure_file2 = here::here("results","resultfigure2.png")
ggsave(filename = figure_file2, plot = plot_2)

#Plot 3, Plotting Cancer Deaths by Site

plot_3 <- mydata %>%
  ggplot(aes(x=`Cancer Sites`, y=Deaths)) +
  geom_bar(stat = "Identity") +
  ggtitle("Cancer Deaths by Site")+
  geom_smooth(method='lm')+
  scale_x_discrete(guide = guide_axis(n.dodge = 1, check.overlap = TRUE))+
  scale_y_continuous(labels = comma)+
  theme(text = element_text(size = 6),
        axis.text.x = element_text(angle = 90, hjust = 1)) 
  
#Examine plot
plot(plot_3)

#Save the plot
figure_file3 = here::here("results","resultfigure3.png")
ggsave(filename = figure_file3, plot = plot_3)
  