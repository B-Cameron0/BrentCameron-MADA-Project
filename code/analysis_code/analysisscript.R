###############################
# analysis script
###############################

#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#The statistical analysis can also be found in this script, with the most 
#important data discussed further in the manuscript file

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidyverse) #for easier data manipulation
library(tidymodels) #for easier statistical analysis

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

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
print(Cancer_Deaths_By_Race)

#As we can see from the chart, cancer mortality is largely concentrated to the 
#White population group, with African American being the second highest

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
print(Cancer_Deaths_By_Age_Group)

#Like the graph that was viewed earlier, deaths are largely concentrated towards
#older populations

#Now we will examine cancer deaths by site to see if there are particular 
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
print(Cancer_Deaths_By_Site)

#Once again, as previously discussed in the prior chart examining count of 
#cancer sites, deaths are concentrated in digestive, lung and bronchial,
#and respiratory
#summarize data 
mysummary = summary(mydata)

#look at summary
print(mysummary)

#change shape to data frame (for easier saving/showing in manuscript)
summary_df = data.frame(do.call(cbind, lapply(mydata, summary)))

#save data frame table to file for later use in manuscript
summarytable_file = here("results", "summarytable.rds")
saveRDS(summary_df, file = summarytable_file)

######################################
#Data fitting/statistical analysis
######################################

#This section of the analysis script will focus on the statistical 
#analysis of the data, through utilizing the "tidymodels" framework

#make sure location of data is set 
data_location <- here::here("data","processed_data","exploration.rds")

#load data. 
mydata <- readRDS(data_location)

#Now that we have processed, explored, and analyzed the data, we will now 
#work on splitting the data to create a training set and a testing set, 
#which we will then use to measure model performance

#First we fix the random numbers by setting the seed
#This allows for the construction of a reproducible analysis when random numbers
#are utilized
set.seed(123)

#We will now put 3/4 of our data into the training set
data_split <-initial_split(mydata, prop = 3/4)

#We will now create data frames for the two sets (training and testing)
train_data <- training(data_split)

test_data <- testing(data_split)

#Now that we have data that is split into our training and test data, we will
#now initiate a recipe to create a simple linear regression model
#Note that our main continuous predictor will be cancer deaths, while our
#main categorical predictors will be ethnicity and race 

#we will start our models by creating a recipe for our first main categorical
#predictor, which is ethnicity, and utilize all other predictors to determine
#if any statistically significant associations exist
mydata_rec <-
  recipe(Ethnicity ~ ., data = train_data) 

#Let's get the summary of our data to examine it more closely
summary(mydata_rec)

#Now that we have our recipe for our data, we will fit our model to the data
glm_mod <- logistic_reg() %>%
  set_engine("glm")

#Now that we have set our model, we will use the "workflow" package to 
#streamline the fitting process by bundling the recipe and models together
#Note that we use our recipe in the process and not the data by itself
mydata_wflow <-
  workflow() %>%
  add_model(glm_mod) %>%
  add_recipe(mydata_rec)

#Now we will prepare the recipe and train the model
mydata_fit <-
  mydata_wflow %>%
  fit(data = train_data)

#Now we will examine our fitted model by using the 'extract_fit' function
mydata_fit %>%
  extract_fit_parsnip() %>%
  tidy()

#With the past commands, so far we have 1. built our model, 2. created a recipe,
#3. bundled the model with our recipe, and 4. trained our workflow using a 
#single call to fit()

#Now we will use our trained workflow to predict with the test data using the 
#predict() command, which applies the recipe to the new data then passes the
#collected information to the fitted model
predict(mydata_fit, test_data)

#We weill use the 'augment' function to predict each possibility of our data
mydata_aug <- augment(mydata_fit, test_data)

mydata_aug %>%
  select(Ethnicity, .pred_class)

#We will now generate a ROC curve to examine the effectiveness of our model
#We will need to change Ethnicity from a character to a factor

mydata_aug$Ethnicity <- as_factor(mydata_aug$Ethnicity)

mydata_aug %>%
  roc_curve(truth = Ethnicity, .pred_class) %>%
  autoplot

#Now we will use the roc_auc() function to estimate the area under the curve
mydata_aug %>%
  roc_auc(truth = Ethnicity, .pred_class)

#Now that we have examined the ROC and ROC-AUC for the test_data we will examine 
#the ROC and ROC-AUC for the training data
predict(mydata_fit, train_data)
mydata_aug <- augment(mydata_fit, train_data)
mydata_aug %>%
  select(Ethnicity, .pred_class)

#We will now generate a ROC curve to examine the effectiveness of our model
mydata_aug %>%
  roc_curve(truth = Ethnicity, .pred_Yes) %>%
  autoplot()

#Now we will use the roc_auc() function to estimate the area under the curve
mydata_aug %>%
  roc_auc(truth = Ethnicity, .pred_Yes)

#Finally, we will examine an alternative model with only the main predictor 
#Deaths, to Ethnicity, rather than the other chosen predictors
#We must create a new recipe before we continue
alternative_rec <-
  recipe(Ethnicity ~ Deaths, data = train_data) 

#Just like before, we will create a workflow with the recipe
alternative_rec_workflow <-
  workflow() %>%
  add_model(glm_mod) %>%
  add_recipe(alternative_rec)

#Now we will prepare the recipe and train the model
alternative_fit <-
  alternative_rec_workflow %>%
  fit(data = train_data)

#Now we will examine our fitted model by using the 'extract_fit' function
alternative_fit %>%
  extract_fit_parsnip() %>%
  tidy()

# save fit results table  
table_file = here("results", "resulttable.rds")
saveRDS(alternative_fit, file = table_file)

