# Make sure to run code from "nyc_real_estate_project directory"

### Libraries Needed ###

library(tidyverse)
library(magrittr) #Make all column names lower case with no spaces
library(stringr)
library(dplyr) 
library(readr) #Load and write csv files
library(ggplot2) 
library(tidyr) 
library(nortest) #Normality testing
library(caret) 
library(epiDisplay) #Help Summarize our Data
library(rpart)
library(rpart.plot)
library(vip)
library(pdp)



### Load Data/Data Overview ###
nyc_data <- read_csv("nyc-rolling-sales.csv")


### Data Pre-Processing
## Organizing and Formatting Data
# Make data easier to process and reference by making everything lowercase and replacing spaces with "_"
colnames(nyc_data) %<>% str_replace_all("\\s", "_") %>% tolower()


# Drop and NA values in the columns that we care about; gross_square_feet,land_square_feet, and sale_price
nyc_data <- nyc_data %>% distinct() %>% drop_na(c(gross_square_feet,land_square_feet, sale_price))

# Make gross_square_feet,land_square_feet, and sale_price numerical values for easier analysis
nyc_data$sale_price <- as.numeric(nyc_data$sale_price)
nyc_data$land_square_feet <- as.numeric(nyc_data$land_square_feet)
nyc_data$gross_square_feet <- as.numeric(nyc_data$gross_square_feet)

# Remove all NA's that were made by coercion from "as.numeric"
nyc_data <- nyc_data %>% distinct() %>% drop_na(c(gross_square_feet,land_square_feet, sale_price))

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

# Filter Unwanted Data

nyc_data <- nyc_data %>% filter(land_square_feet>100) %>% filter(gross_square_feet>100) %>% filter(sale_price >100000)

## Numerical Identification of Catergorical Data
unique(nyc_data$borough)
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

# This will help identify possible colinearity in our correlation matrix later on


### Linear Modelling ###

## Identify possible correlation
cor_matrix <- nyc_data %>% select_if(is.numeric) %>% cor(use = 'pairwise.complete.obs')
cor_matrix

# We can notice that the correlation between sale_price and gross_square_feet is 0.5272, we'll want to use this variable
# Other variables with not as high correlation, but of interest are residential_units, total_units, and tax_class_at_time_of_sale
# One other observation, there is possible collinearity between the individual boroughs

nyc_data <- nyc_data %>% distinct() %>% drop_na(c(residential_units, total_units, tax_class_at_time_of_sale)) #clean up NA's if any


## Model Considering only the gross_square_feet

nyc_rmse_results <- data_frame()

model_gsf <- lm(sale_price~gross_square_feet, data = nyc_data)
summary(model_gsf)
#sigma(model_gsf)  #RMSE

nyc_rmse_results <- data_frame(Model = "1", RMSE = sigma(model_gsf))
nyc_rmse_results %>% knitr::kable() # To look at our results

# Check 95% confidence interval
confint(model_gsf, level=0.95)

## Model Considering gross_square_feet and residential_units
model_2 <- update(model_gsf, . ~ . + residential_units)
summary(model_2)
#sigma(model_2)

nyc_rmse_results <- bind_rows(nyc_rmse_results, data_frame(Model = "2", RMSE = sigma(model_2)))
nyc_rmse_results %>% knitr::kable() # To look at our results

# Comparing the RMSE's, we did see an improvement

## Model considering gross_square_feet, residential_units, and total_units
model_3 <- update(model_2, . ~ . + total_units)
#summary(model_3)
#sigma(model_3)

nyc_rmse_results <- bind_rows(nyc_rmse_results, data_frame(Model = "3", RMSE = sigma(model_3)))
nyc_rmse_results %>% knitr::kable() # To look at our results


## Model considering gross_square_feet, residential_units, total_units, and tax_class_at_time_of_sale
model_4 <- update(model_3, . ~ . + tax_class_at_time_of_sale)
#sigma(model_4)

nyc_rmse_results <- bind_rows(nyc_rmse_results, data_frame(Model = "4", RMSE = sigma(model_4)))
nyc_rmse_results %>% knitr::kable() # To look at our results

### Decision Tree Model

nyc_dec_tree1 <- rpart(
  formula = sale_price ~ .,
  data = nyc_data,
  method = "anova"
)

nyc_dec_tree1

