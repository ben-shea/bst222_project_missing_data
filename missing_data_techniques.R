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

#https://datascienceplus.com/how-to-use-r-for-matching-samples-propensity-score/
set.seed(1234)
match.it <- matchit(missing_ind ~population+gdp_for_year+`15-24 years`+`25-34 years`+
                      `5-14 years`+`55-74 years`+`75+ years`+`female`,
                    data = mcar_data, method="nearest", ratio=1)
#a <- summary(match.it)


#indicate if missing or not
mcar_data$missing_ind <- is.na(mcar_data$suicides_no)*1

mcar_logit <- glm(missing_ind ~population+gdp_for_year+`15-24 years`+`25-34 years`+
                    `5-14 years`+`55-74 years`+`75+ years`+`female`,family="binomial", data = mcar_data)
predict(mcar_logit, type="response")
