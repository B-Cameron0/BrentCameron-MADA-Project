---
  title: "Cancer Data Modeling"
output: 
  html_document:
  toc: FALSE
---

  #We have already done a basic exploration and analysis of the data, now we will 
#work on further modeling to better understand our data and infer any causation 
#present- our main predictors we will analyze will be ethnicity and race
  
  #Before we begin let's load some packages that we will need for the data 
#exploration, fitting, and modeling of the data 
#please note that If you do not have the packages installed you will have to
#first install them with the "install.packages()" command
library(tidyverse) #for streamlining manipulating data
library(tidymodels) # for streamlining fitting data to models
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(ggplot2) #for plotting
library(gapminder) #For reordering bar charts to be more easily understood
library(rpart) #for fitting tree model
library(glmnet) #for fitting LASSO model
library(ranger) #for fitting random forest model
library(vip) #for identifying most important variables in our models (the "VIPS")
library(skimr) #for viewing alternative information about variables
library(doParallel) # for parallel processing for quicker tuning

#Summary of our data tells us that we are dealing with almost all character 
#type variables, with the only numerical being deaths
summary(mydata)

#Using the glimpse command tells us that we have variable names including
#Age,Sex,Deaths,Ethnicity,Race,and CancerSite
glimpse(mydata)

#In addition, utilizing the "skimr" package allows us to more closely 
#view the individual variables
skim(mydata)

#We have 57 missing values from our data, and so we will drop them to allow 
#for better fitting
cancer_data_cleaned <- mydata %>% na.omit()

#We will also change our character type variables to numeric to allow for 
#modeling (all except for Deaths)

#Now that we have omitted NA variables, we can begin with the modeling,
#we will use a null model, single tree model, LASSO model, and a random forest 
#model to determine best fit

#######################
#Tuning and Modeling
#######################

#Now that we have finished examining our data, we will start our analysis by
#separating our data into a training set for tuning and evaluating the models,
#and a test set to compare our results to ensure strength of fit
#We first set our seed, which is simply initializing a pseudorandom number generator (makes sure we get the same results each time we run the data (can be any number of choice though)),
#then we split the data
set.seed(123)

#We will now split the data by 70% for our training data and 30% for our testing
#We will start by examiniing race as our strata
data_split <- initial_split(cancer_data_cleaned, prop = 7/10,#7/10 stands for 70% training
                            strata = "Race") # and the rest (30%) for testing) 

#Now we will organize our sets of training and test data
train_data <- training(data_split)

test_data <- testing(data_split)

#We will now utilize a 5-fold cross validation, 5 times repeated, we will 
#stratify on "Race" for the CV folds
FoldCV5 <- vfold_cv(train_data, v = 5, repeats = 5, strata = "Race")

#Now we will create our recipe for our data and fitting
#We will code the categorical variables as dummy variables
recipe_Race <-recipe(Race ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors())

#Now that we have decided on what our main predictor variable will be 
#(Race (we will do ethnicity later)) and created our data sets (training and 
#test), we can now begin tuning and modeling

#We will fit a null model, single tree model, LASSO model, and a random forest model (total of four)
#Our steps should be as follows...

#1. Model Specification

#2. Workflow Definition

#3. Tuning Grid Specification

#4. Tuning Using Cross- Validation and the tune_grid() function

####################################
#NULL MODEL
####################################
#Before we can adequately assess if any of our models posess good fit of our data
#we need to first create our null model that we can compare the rest of our models
#to, if none of our other models perform better than the Null, they are not worth 
#pursuing

#We need to specify our model before we start computing
lm_model <- linear_reg() %>% 
  set_engine('lm') %>%
  set_mode('regression')

#We will now compute the performance of a null model for our training and test data
#(doesn't use any predictor information)

#Train Data Computing
train_null_recipe <- lm(Race ~ 1, data = train_data)

#Calculating RMSE
train_null_recipe %>% augment(newdata = train_data) %>%
                      rmse(truth = Race, estimate = .fitted)

#Test Data Computing
test_null_recipe <- lm(Race ~ 1, data = test_data)

#Calculating RMSE
test_null_recipe %>% augment(newdata = test_data) %>%
  rmse(truth = Race, estimate = .fitted)


#When comparing our models, if the RMSE (our chosen measure of significance) of our other models is worse than what the null model 

####################################
#SINGLE TREE MODEL
####################################
#We will now start with our first comparison model (the single tree model)
#Specify Model
tune_spec_TREE <-
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune(),
  ) %>%
  set_engine("rpart") %>%
  set_mode("regression")
tune_spec_TREE

#We will now define the workflow for the tree 
workflow_TREE <- workflow() %>%
            add_model(tune_spec_TREE) %>%
            add_recipe(recipe_Race) 

#We will now specify the tuning grid
grid_TREE <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)
grid_TREE


#We will now tune using cross validation and the tune_grid() function
res_TREE<-
  workflow_TREE %>%
  tune_grid(resamples = FoldCV5 , grid = grid_TREE, metrics = metric_set(rmse))

#Now we will run the autoplot() function to look at some diagnostics
res_TREE %>%
  autoplot()

#Now we will select the best decision tree model
TOP_TREE <- res_TREE %>% 
  select_best("rmse")

TOP_TREE

#Now we need to finalize the workflow
workflow_FINAL <- workflow_TREE %>% finalize_workflow(TOP_TREE)
workflow_FINAL

#Now we will utilize the fit() function to fit to the training data
fit_FINAL_TREE <- workflow_FINAL %>% last_fit(data_split)

#Now we will collect the data from our fit
fit_FINAL_TREE %>% collect_metrics()

#We will also collect the predictions
pred_TREE <- fit_FINAL_TREE %>% collect_predictions()

#We will now make two plots, one that shows model predictions from the tuned
#model compared to actual outcomes, and one that plots residuals (RMSE)
pred_tree_plot <- ggplot(data = pred_TREE, aes(x = .pred, y = Race)) + 
           geom_point() +
           labs(title = "Plot Comparing Model Predictions from Tuned to Actual",
                x = "Predictions", y = "Outcomes")
#view the plot
pred_tree_plot

#We need to calculate our residuals before we can plot the second chart
#Note that the residuals is the difference between our main predictor and the others
pred_TREE$residuals <- pred_TREE$Race - pred_TREE$.pred


#Now we will plot our residuals
resid_tree_plot <- ggplot(data= pred_TREE, aes(x=.pred , y=residuals)) + geom_point() +
                    labs(title="Plot of Residuals",
                    x="Predictions", y= "Residuals")
#view the plot
resid_tree_plot 

#Now we will compare our residual plot to the null model
tree_model_performance <- res_TREE %>% show_best(n=1)
print(tree_model_performance)

###HOW DOES THIS MODEL PERFORM?


#########################################
#LASSO
#########################################
Now we will construct a LASSO model
code used from https://www.tidymodels.org/start/case-study/

```{r}
#We will once again start by constructing our model
lasso_model <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("glmnet") %>%
  set_args(penalty = tune(), mixture = 1) 
```
Please note that mixture refers to a number between zero and one that is the 
proportion of L1 regularization (lasso) in the model. In other, words, because
we are using mixture = 1, we are utilizing a "pure" lasso model here

We will now create our workflow
```{r}
lasso_workflow <-workflow() %>%
  add_model(lasso_model) %>%
  add_recipe(recipe_avg_time_lap)
```

Now we will tune our LASSO model
As our last model took a long time to run, we will utilize parallel computing
to make it faster
```{r}
library(doParallel)
ncores = 5 #Ncores is used to select the number of cores you want to recruit
#for processing, different computers will naturally have different ideal numbers

cluster <- makePSOCKcluster(5) #make PSOCKcluster stands for creating a sock
#cluster within the 'snow' package, this allowsa for increased computing time

  registerDoParallel(5) #registers parallel backend with foreach package
```

Now we will create our tuning grid 
```{r}
  lasso_reg_grid <- tibble(penalty = 10^seq(-3, 0, length.out = 30))
  #Now we tune the model
  lasso_tune_res <- lasso_workflow %>%
    tune_grid(resamples = FoldCV5,
              grid = lasso_reg_grid,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(rmse))
```
We will now turn off parallel clustering, the reason we turn the clustering off
after each use is to prevent computations and analysis from being slowed in 
later data analysis, fitting, modeling, etc.
```{r}
stopCluster(cluster)
```

  
We will now evaluate our LASSO model
```{r}
lasso_tune_res %>% autoplot()
```
Now we will get the tuned model that performs best
```{r}
  best_lasso <- lasso_tune_res %>% select_best(metric = "rmse")
  #We now finalize our workflow with the best model
  best_lasso_wf <- lasso_workflow  %>% finalize_workflow(best_lasso)
  #We now fit our best performing model
  best_lasso_fit <- best_lasso_wf %>%
    fit(data = train_data)
  
  lasso_pred <- predict(best_lasso_fit, train_data)
```

Now we will repeat our steps like the past model and plot LASSO variables as
a function of tuning parameter
```{r}
x <- best_lasso_fit$fit$fit$fit
plot(x, "lambda")
```

When a variable is 0 it is no longer being used in the model, thus we are using
all variables that are only part of the best fit model
```{r}
tidy(extract_fit_parsnip(best_lasso_fit)) %>% filter(estimate !=0)
```

Now we plot the observed/predicted and residual plots
We will try a new way to plot that does not require calculating the 
residuals before hand

First we will plot with the observe/predicted values
This code will plot a line with which we hope to see overlap with 
the values, thus signaling that the model is a good fit

For our x and y limits, the values 10 and 100 were chosen because they allow for
the clearest illustration of the values in the plane
The abline is used to add lines to the graph 
```{r}
plot(lasso_pred$.pred,train_data$avg_time_lap, xlim = c(10, 50), ylim = c(10, 100))
abline(a = 0, b = 1, col = 'red') #b = 1 creates a 45 degree diagonal line
```
Now our residual plot, note that because we are subtracting the two values 
used this time instead of putting them together, since residuals are by 
definition the difference between the regular predictors and the chosen predictor
```{r}
plot(lasso_pred$.pred-train_data$avg_time_lap)
abline(a=0, b=0, col = 'blue') #b = 0 creates a straight horizontal line
```

Let's look at the performance of the model
  ```{r}
  lasso_performance <- lasso_tune_res %>%
    show_best(n = 1)
  
  print(lasso_performance)
  ```
  
  The Lasso model seems to have better fit than than null, we will continue with out last #model to determine if it is better
  ################################
  #RANDOMFOREST
  ################################
  Both of our past models have not extremely significant fit, we will now repeat the
  steps with a random forest model in the hopes of finding significance
  
  *Please note that for Random Forest models, "num.threads" and importance 
  is required or else all models will fail*
    
    Repeat the steps of the last models in tuning and setting workflow
  ```{r}
  randomforest_model <- rand_forest() %>%
    set_args(mtry = tune(),
             trees = tune(), 
             min_n = tune()
    ) %>%
    #Now we set the engine
    set_engine("ranger", 
               num.threads = 5,
               importance = "permutation") %>%
    #We select either the continuous or binary classification
    set_mode("regression")
  ```
  
  We will set our workflow once again
  ```{r}
  randomforest_workflow <- workflow() %>% 
    add_model(randomforest_model) %>%
    add_recipe(recipe_avg_time_lap)
  ```
  
  
  We will now repeat our steps as the first two models to specify our tuning grid
  We will use parallel computing once again to vastly decrease the time it takes 
  to compute the model- since we have already use code previously to create it
  we now only need to use our name designation for our cluster and it will resume
  ```{r}
  cluster <-makePSOCKcluster(5)
  registerDoParallel(5)
  ```
  
  Now we will tune the grid
  ```{r}
  randomforest_grid <- expand.grid(mtry = c(3, 4, 5, 6), min_n = c(40, 50, 60),
                                   trees = c(500, 1000))
  ```
  
  We will now tune the model while optimizing RMSE
  ```{r}
  randomforest_tune_res <- randomforest_workflow %>%
    tune_grid(resamples = FoldCV5, #This is the name of our previous CV object
              grid = randomforest_grid,#This is the grid of values we want to try
              metrics = metric_set(rmse))
  ```
  
  Now we turn off our parallel clustering again to prevent slowing processing
  ```{r}
  stopCluster(cluster)
  ```
  
  Now we plot the performance of our different tuning parameters
  ```{r}
  randomforest_tune_res %>% autoplot()
  ```
  
  Now we will obtain the best performing model
  ```{r}
  best_randomforest <- randomforest_tune_res %>% select_best(metric = "rmse")
  ```
  
  Finalize the workflow with this model
  ```{r}
  best_randomforest_workflow <- randomforest_workflow %>% finalize_workflow(best_randomforest)
  ```
  
  Now we fit the best performing model
  ```{r}
  best_randomforest_fit <- best_randomforest_workflow %>% fit(data = train_data)
  randomforest_predict <-predict(best_randomforest_fit, train_data)
  ```
  
  although all variables stay in a random forest model, we can examine which are 
  the most imoportant using the 'vip' package
  ```{r}
  x<- best_randomforest_fit$fit$fit$fit
  ```
  
  plot the variables by importance
  ```{r}
  vip(x, num_features = 10)
  ```
  as can be seen from the plot, track length is the strongest factor, with 
  time_s being second. This makes sense as length of the course and time of 
  laps are both key when determining how long it will take to complete the race
  
  We will now plot the observed/ predicted and residual plots and compare them
  we will repeat the same process used as last time
  ```{r}
  plot(randomforest_predict$.pred,train_data$avg_time_lap, 
       xlim =c(10, 50), ylim=c(10, 50),
       abline(a = 0, b = 1, col = 'red'))
  ```
  
  residual plot
  ```{r}
  plot(randomforest_predict$.pred-train_data$avg_time_lap)
  abline(a = 0, b = 0, col = 'blue')
  ```
  
  now that we have finished plotting lets look at our model performance
  ```{r}
  randomforest_performance <- randomforest_tune_res %>% show_best(n = 1)
  print(randomforest_performance)
  ```
  
  The mean RMSE is 1.31, which is not significant
  
  The LASSO model had the lowest RMSE, which even though is not extremely significant, still puts it in a position to be chosen as the 
  most meaningful, thus we will choose it as our final model
  
  ########################################
  #final model (LASSO) fitting
  ########################################
  
  ```{r}
  #lets restart our parallel processing
  cluster<- makePSOCKcluster(5)
  registerDoParallel(5)
  ```
  
  Now we will fit on the training set evaluating with the test data
  ```{r}
  LASSO_fit_final <-best_lasso_wf %>% last_fit(data_split)
  ```
  
  We will now use a trained workflow to predict using our 
  test data
  ```{r}
  final_test_performance<-LASSO_fit_final %>% collect_predictions()
  print(final_test_performance)
  ```
  We will also collect our metrics since it tells us more information
  ```{r}
  final_test_performance_RMSE <- LASSO_fit_final %>% collect_metrics()
  print(final_test_performance_RMSE)
  ```
  
  Finally we will turn of our paralell processing one last time
  When comparing the prediction of our final model with the actual data, it 
  appears somewhat close but could definitely be better, which indicates that we mostly avoided overfitting, but there may be one or two things we could improve
  ```{r}
  stopCluster(cluster)
  ```
  
  unfortunately, when we examine the RMSE of our data we can see that it performs similarly as with the last data. While this shows the model is fairly consistent
  it still indicates that the model is not an adequate fit for our data
  
  We will finally plot our final models predicted compared with observed values
  and another plot for residuals
  
  predicted versus observed 
  ```{r}
  plot(final_test_performance$.pred, test_data$avg_time_lap, 
       xlim = c (10, 50), ylim = c(10, 50))
  abline(a = 0, b = 1, col = 'red')
  ```
  
  residual plot
  ```{r}
  plot(final_test_performance$.pred-test_data$avg_time_lap)
  abline(a = 0, b = 0, col = 'red')
  ```
  
  While the LASSO model performed best, there appears to be a few things that we can improve upon to make the model fit better. Additionally, the track length feature could be used as another, perhaps more adequate, measure of model
  performance.