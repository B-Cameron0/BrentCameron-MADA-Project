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

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata<-readRDS(data_location)

######################################
#Data exploration/description
######################################

#Let's first examine a summary of our data before we continue with exploration
mysummary <- summary(mydata)
print(mysummary)

#As can be seen from the summary, all variables are considered characters except
#for Deaths, which is considered numerical

#Please note...

#Our outcome of interest is Cancer deaths
#Variables that will serve as predictors of outcome include...
#Age group, sex, total deaths, ethnicity, race, and cancer sites

#Before we do plotting of the data comparing our predictors of interest, we 
#will examine each variable separately to better understand the data we are 
#working with

#First we will examine the age group of our data set:
mydata %>% ggplot(aes(x = Age)) + geom_bar() +
  scale_x_discrete(guide = guide_axis(n.dodge = 1)) +
  theme(text = element_text(size = 10),
      axis.text.x = element_text(angle = 90, hjust = 1)) 

#As can be seen from the chart the population is largely composed of older 
#adults between the ages of 50-85
     
#Now the sex of the data set:
mydata %>% ggplot(aes(x = Sex)) + geom_bar()

#The chart shows that this data set is almost exactly equal in terms of number
# of male and female 

#Next we will examine the race of the data set:
mydata %>% ggplot(aes(x = Race)) + geom_bar() +
scale_x_discrete(guide = guide_axis(n.dodge = 2))

#As can be seen from the data, we are working with a majority White population
#with a moderate amount of African American and a lower amount of Asian or 
#Pacific Islander and American Indian or Alaska Native

#Now ethnicity:
mydata %>% ggplot(aes(x = Ethnicity)) + geom_bar() 

#This chart shows us that the data is composed of a mostly non-Hispanic 
#population, with Hispanic as the second most common followed by some unknown or
#missing

#Finally cancer sites:
mydata %>% ggplot(aes(x = CancerSite)) + geom_bar() +
  scale_x_discrete(guide = guide_axis(n.dodge = 1)) +
  theme(text = element_text(size = 8, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))

#It seems as though the largest number of cancer cases are localized to the 
#digestive system, lung and bronchus, and respiratory system, with a large number 
#of cases considered to be miscellaneous

#Now that we have completed a preliminary look at our predictors of interest,
#we will examine a closer look at the data to see if any basic trends 
#can be seen, with the first plot, we will be examining cancer deaths and 
#race to see if there exists any trends

#Plot 1, Plotting Cancer deaths by Race

Cancer_Deaths_By_Race <- mydata  %>%
  ggplot(aes(x=Race, y=Deaths)) +
    geom_bar(stat = "identity") +
     ggtitle("Cancer Deaths by Race in the United States") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2, check.overlap = FALSE))+
       scale_y_continuous(labels = comma)


#Examine plot
plot(Cancer_Deaths_By_Race)

#As we can see from the chart, cancer mortality is largely concentrated to the 
#White population group, with African American being the second highest

#Save the plot
figure_file1 = here::here("results","resultfigure1.png")
ggsave(filename = figure_file1, plot = Cancer_Deaths_By_Race)

#Now we will be plotting the number of Cancer deaths by Age Group to examine
#If a trend is present between certain age groups having higher levels of 
#relative mortality

#Plot 2, Plotting Cancer deaths by Age Group

Cancer_Deaths_By_Age_Group <-mydata %>%
  ggplot(aes(x=Age, y=Deaths))+
  geom_bar(stat = "identity") +
  ggtitle("Cancer Deaths by Age Group in the United States")+
  geom_smooth(method='lm')+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(guide = guide_axis(n.dodge = 1, check.overlap = TRUE))+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1)) 

#Examine the Plot
plot(Cancer_Deaths_By_Age_Group)

#Like the graph that was viewed earlier, deaths are largely concentrated towards
#older populations

#Save the plot
figure_file2 = here::here("results","resultfigure2.png")
ggsave(filename = figure_file2, plot = Cancer_Deaths_By_Age_Group)


# Now we will examine cancer deaths by site to see if there are particular 
#sites that are responsible for a higher overall mortality

#Plot 3, Plotting Cancer Deaths by Site

Cancer_Deaths_By_Site <- mydata %>%
  ggplot(aes(x= CancerSite, y=Deaths)) +
  geom_bar(stat = "Identity") +
  ggtitle("Cancer Deaths by Site")+
  geom_smooth(method='lm')+
  scale_x_discrete(guide = guide_axis(n.dodge = 1, check.overlap = TRUE))+
  scale_y_continuous(labels = comma)+
  theme(text = element_text(size = 6),
        axis.text.x = element_text(angle = 90, hjust = 1)) 
  
#Examine plot
plot(Cancer_Deaths_By_Site)

#Once again, as previously discussed in the prior chart examining count of 
#cancer sites, deaths are concentrated in digestive, lung and bronchial,
#and respiratory

#Save the plot
figure_file3 = here::here("results","resultfigure3.png")
ggsave(filename = figure_file3, plot = Cancer_Deaths_By_Site)

#Once again, as previously discussed in the prior chart examining count of 
#cancer sites, deaths are concentrated in digestive, lung and bronchial,
#and respiratory

#Save the plot
figure_file3 = here::here("results","resultfigure3.png")
ggsave(filename = figure_file3, plot = Cancer_Deaths_By_Site)

#########################################
#Data Fitting/ Statistical Analysis
#########################################