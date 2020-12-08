#########################
# Title: BST 222 Project 
# Purpose: Use Last value carried forward and propensity score matching for multiple imputation of missing values
#########################



# SETUP -------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(zoo)
library(mice)
library(MatchIt)


# LOAD DATA ---------------------------------------------------------------

mcar_data <- read_csv("clean_data/mcar_data.csv")
mar_data <- read_csv("clean_data/mar_data.csv")

# Last Value Carried Forward ----------------------------------------------

# https://stackoverflow.com/questions/2776135/last-observation-carried-forward-in-a-data-frame
lvcf_mcar_data <- na.locf(mcar_data) 
lvcf_mar_data <- na.locf(mar_data)

# Propensity Score Match --------------------------------------------------

#indicate if missing or not
mcar_data$missing_ind <- is.na(mcar_data$suicides_no)*1

#remove rows where year is 2016
mcar_data <- mcar_data %>% filter(year !=2016) 

#https://datascienceplus.com/how-to-use-r-for-matching-samples-propensity-score/
set.seed(1234)
match.it <- matchit(missing_ind ~population+gdp_for_year+`15-24 years`+`25-34 years`+
                      `5-14 years`+`55-74 years`+`75+ years`+`female`,
                    data = mcar_data, method="nearest", ratio=1)
summary(match.it)

df.match <- match.data(match.it)[1:ncol(mcar_data)]

mcar_logit <- glm(missing_ind ~population+gdp_for_year+`15-24 years`+`25-34 years`+
                    `5-14 years`+`55-74 years`+`75+ years`+`female`,family="binomial", data = mcar_data)
predict(mcar_logit, type="response")




# We will be using mice library in r
library(mice)

# Deterministic regression imputation via mice
imp <- mice(mcar_data %>% select(year,suicides_no, population, gdp_for_year,female), method = "norm.predict", m = 1)
# Store data

data_imp <- complete(imp)
# Multiple Imputation
imp <- mice(mydata, m = 5)
#build predictive model
fit <- with(data = imp, lm(y ~ x + z))
#combine results of all 5 models
combine <- pool(fit)
