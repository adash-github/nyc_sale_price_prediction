## ----setup, include=FALSE----------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----download, message=FALSE, warning=FALSE, echo=FALSE----------------------------------------------------------------------

# Download required libraries for the project
# Not all libraries are required as I had to reduce the scope of my project due to my own limitations. But it is best to download them all just in case. 

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr")
if(!require(stringr)) install.packages("stringr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(readr)) install.packages("readr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyr)) install.packages("tidyr")
if(!require(nortest)) install.packages("nortest")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(epiDisplay)) install.packages("epiDisplay")
if(!require(rpart)) install.packages("rpart")
if(!require(rpart.plot)) install.packages("rpart.plot")
if(!require(vip)) install.packages("vip")
if(!require(pdp)) install.packages("pdp")

library(tidyverse)
library(magrittr) 
library(stringr)
library(dplyr) 
library(readr)
library(ggplot2) 
library(tidyr) 
library(nortest)
library(caret) 
library(epiDisplay)
library(rpart)
library(rpart.plot)
library(vip)
library(pdp)

set.seed(1996) # Help in reproducibility 

### Load Data/Data Overview ###
nyc_data <- read_csv("nyc-rolling-sales.csv")
str(nyc_data)
head(nyc_data)



## ----echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------
### Data Pre-Processing
## Organizing and Formatting Data
# Make data easier to process and reference by making everything lowercase and replacing spaces with "_"
colnames(nyc_data) %<>% str_replace_all("\\s", "_") %>% tolower()



## ----message=FALSE, warning=FALSE, echo=FALSE--------------------------------------------------------------------------------
# Drop and NA values in the columns that we care about; gross_square_feet,land_square_feet, and sale_price
nyc_data <- nyc_data %>% distinct() %>% drop_na(c(gross_square_feet,land_square_feet, sale_price))

# Make gross_square_feet,land_square_feet, and sale_price numerical values for easier analysis
nyc_data$sale_price <- as.numeric(nyc_data$sale_price)
nyc_data$land_square_feet <- as.numeric(nyc_data$land_square_feet)
nyc_data$gross_square_feet <- as.numeric(nyc_data$gross_square_feet)

# Remove all NA's that were made by coercion from "as.numeric"
nyc_data <- nyc_data %>% distinct() %>% drop_na(c(gross_square_feet,land_square_feet, sale_price))



## ----message=FALSE, warning=FALSE, echo=FALSE--------------------------------------------------------------------------------

## Analayze initial data and remove Outliers
# We'll take a look at sale prices to see if their is anything wrong
# For example because property prices are very high in NYC we want to remove any low sale prices
# We'll take a look at the data and see what the frequency of sales
# We'll take a look at sale_price, land_square_feet and gross_square_feet
# We want to look at Square feet to find housing that we deem too small of people to live in

# Generate Frequence tables for each variable

freq_sale_price <- nyc_data %>% group_by(sale_price) %>% summarize(Freq = n())
freq_land_square_feet <- nyc_data %>% group_by(land_square_feet) %>% summarize(Freq = n())
freq_gross_square_feet <- nyc_data %>% group_by(gross_square_feet) %>% summarize(Freq = n())

freq_gross_square_feet
freq_land_square_feet
freq_sale_price

# We quickly see a few issues, first with both gross_square_feet and land_square_feet
# There are some really small plots (<100 square feet), we'll want to deal with them
# With sale price, their are over 10,000 sales of $0, we will want to filter them too



## ----message=FALSE, warning=FALSE, echo=TRUE---------------------------------------------------------------------------------

# Filter Unwanted Data

nyc_data <- nyc_data %>% filter(land_square_feet>100) %>% filter(gross_square_feet>100) %>% filter(sale_price >100000)



## ----message=FALSE, warning=FALSE, echo=FALSE--------------------------------------------------------------------------------

## Numerical Identification of Catergorical Data
#unique(nyc_data$borough)
# The Data Set uses these identifications for the borough variable
# 1 = Manhattan
# 2 = Bronx
# 3 = Brooklyn
# 4 = Queens 
# 5 = Staten Island

# Our goal is to create columns for each borough that acts as an indentifier for each borough

nyc_data$in_manhattan <- ifelse(nyc_data$borough == 1 , 1, 0 )
nyc_data$in_bronx <- ifelse(nyc_data$borough == 2 , 1, 0 )
nyc_data$in_brooklyn <- ifelse(nyc_data$borough == 3 , 1, 0 )
nyc_data$in_queens <- ifelse(nyc_data$borough == 4, 1, 0 )
nyc_data$in_staten_island <- ifelse(nyc_data$borough == 5, 1, 0 )
head(nyc_data)

nyc_data$tax_class_one <- ifelse(nyc_data$tax_class_at_time_of_sale==1,1,0)
nyc_data$tax_class_two <- ifelse(nyc_data$tax_class_at_time_of_sale==2,1,0)
nyc_data$tax_class_four <- ifelse(nyc_data$tax_class_at_time_of_sale==4,1,0)

# We are ignoring tax_class_three because they are owned by gas, telephone, and/or electric companies
# We will consider them public utilities companies and don't want to consider their sales

hist(nyc_data$borough)
hist(nyc_data$tax_class_at_time_of_sale)
nyc_data %>% filter()
print(nyc_data %>% group_by(building_class_category) %>% summarize(freq = n()))
# This will help identify possible colinearity in our correlation matrix later on



## ----message=FALSE, warning=FALSE, echo=TRUE---------------------------------------------------------------------------------

nyc_data <- subset(nyc_data, select = -c(zip_code, block, lot, `ease-ment`, address, apartment_number, sale_date, building_class_at_present, building_class_at_time_of_sale, neighborhood))





## ----message=FALSE, warning=FALSE, echo=TRUE---------------------------------------------------------------------------------
## Identify possible correlation
cor_matrix <- nyc_data %>% select_if(is.numeric) %>% cor(use = 'pairwise.complete.obs')
cor_matrix



## ----message=FALSE, warning=FALSE, echo=FALSE--------------------------------------------------------------------------------
nyc_data <- nyc_data %>% distinct() %>% drop_na(c(residential_units, total_units, tax_class_at_time_of_sale)) #clean up NA's if any


## ----message=FALSE, warning=FALSE, echo=TRUE---------------------------------------------------------------------------------

set.seed(1996)

nyc_partition <- createDataPartition(nyc_data$sale_price, p=0.8, list=FALSE)
nyc_train <- nyc_data[nyc_partition, ]
nyc_test <- nyc_data[-nyc_partition, ]

nyc_rmse_results <- data_frame()  # To store our model and RMSE results

model_gsf <- lm(sale_price~gross_square_feet, data = nyc_train)
summary(model_gsf)
#sigma(model_gsf)  #RMSE

nyc_rmse_results <- data_frame(Model = "gsf", RMSE = sigma(model_gsf))
nyc_rmse_results %>% knitr::kable() # To look at our results


## ----message=FALSE, warning=FALSE, echo=TRUE---------------------------------------------------------------------------------
set.seed(1996)

## Model Considering gross_square_feet and residential_units
model_2 <- update(model_gsf, . ~ . + residential_units + total_units)
summary(model_2)
#sigma(model_2)

nyc_rmse_results <- bind_rows(nyc_rmse_results, data_frame(Model = "2", RMSE = sigma(model_2)))
nyc_rmse_results %>% knitr::kable() # To look at our results


## ----message=FALSE, warning=FALSE, echo=TRUE---------------------------------------------------------------------------------
set.seed(1996)
## Model considering gross_square_feet, residential_units, total_units, and tax_class_at_time_of_sale
model_3 <- update(model_2, . ~ . + tax_class_one + tax_class_two + tax_class_four)
#sigma(model_3)

nyc_rmse_results <- bind_rows(nyc_rmse_results, data_frame(Model = "3", RMSE = sigma(model_3)))
nyc_rmse_results %>% knitr::kable() # To look at our results


## ----message=FALSE, warning=FALSE, echo=TRUE---------------------------------------------------------------------------------
set.seed(1996)
# Model Inluding all variables

model_all <- lm(sale_price ~ ., data = nyc_train)
nyc_rmse_results <- bind_rows(nyc_rmse_results, data_frame(Model = "all", RMSE = sigma(model_all)))
nyc_rmse_results %>% knitr::kable() # To look at our results



## ----message=FALSE, warning=FALSE, echo=TRUE---------------------------------------------------------------------------------
set.seed(1996)
# 10-fold Cross Validation for model_gf 
# using train() from caret package, specifying lm() method and k =10
(model_gsf_cv <- train(
  form = sale_price ~ gross_square_feet,
  data = nyc_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))



## ----message=FALSE, warning=FALSE, echo=FALSE--------------------------------------------------------------------------------
set.seed(1996)
# 10-fold Cross Validation for model_2 
(model_2_cv <- train(
  form = sale_price ~ gross_square_feet + residential_units + total_units,
  data = nyc_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))

# 10-fold Cross Validation for model_3
(model_3_cv <- train(
  form = sale_price ~ gross_square_feet + residential_units + total_units + tax_class_one + tax_class_two + tax_class_four,
  data = nyc_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))

# 10-fold Cross Validation for model_all
(model_all_cv <- train(
  form = sale_price ~ .,
  data = nyc_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))

# Get a summary of our model performances

summary(resamples(list(
  modelgsf = model_gsf_cv,
  model2 = model_2_cv,
  model3 = model_3_cv,
  modelall = model_all_cv
)))



## ----message=FALSE, warning=FALSE, echo=TRUE---------------------------------------------------------------------------------
set.seed(1996)
nyc_dec_tree1 <- rpart(
  formula = sale_price ~ .,
  data = nyc_train,
  method = "anova"
)

rpart.plot(nyc_dec_tree1)


## ----message=FALSE, warning=FALSE, echo=FALSE--------------------------------------------------------------------------------
set.seed(1996)
plotcp(nyc_dec_tree1)



## ----message=FALSE, warning=FALSE, echo=FALSE--------------------------------------------------------------------------------
set.seed(1996)
nyc_dec_tree1$cptable #using only rpart cross validation results

# caret cross-validation results

nyc_dec_tree2 <- train(
  sale_price ~.,
  data = nyc_train,
  method = "rpart",
  trControl = trainControl(method = "cv", number =10), # 10-fold cv
  tuneLength = 20
)

ggplot(nyc_dec_tree2)



## ----message=FALSE, warning=FALSE, echo=FALSE--------------------------------------------------------------------------------
set.seed(1996)
# 10-fold Cross Validation for model_all_test 
# using train() from caret package, specifying lm() method and k =10
(model_all_test_cv <- train(
  form = sale_price ~ .,
  data = nyc_test,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))



## ----message=FALSE, warning=FALSE, echo=FALSE--------------------------------------------------------------------------------
set.seed(1996)

nyc_dec_tree_test <- rpart(
  formula = sale_price ~ .,
  data = nyc_test,
  method = "anova"
)

rpart.plot(nyc_dec_tree_test)



