#########################
# Title: BST 222 Project 
# Purpose: Use Last value carried forward, generalized linear mixed models, and mean imputation for missing values
#########################



# SETUP -------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(zoo)
library(mice)
library(micemd)
library(lme4)
library(blme)
library(rstudioapi)
library(optimx)

getActiveDocumentContext()$path
working_path <- dirname(getActiveDocumentContext()$path)
setwd(working_path)

# LOAD DATA ---------------------------------------------------------------

mcar_data <- read_csv("clean_data/mcar_data.csv")
mar_data <- read_csv("clean_data/mar_data.csv")

# CLEAN DATA --------------------------------------------------------------

# creating missing indicator column
mar_data$missing_ind <- is.na(mar_data$suicides_no)*1
mcar_data$missing_ind <- is.na(mcar_data$suicides_no)*1

# remove 2016 since it has missing age values
mar_data <- mar_data %>% filter(year != 2016)
mcar_data <- mcar_data %>% filter(year != 2016)

# LAST VALUE CARRIED FORWARD ----------------------------------------------

# https://stackoverflow.com/questions/2776135/last-observation-carried-forward-in-a-data-frame
lvcf_mcar_data <- na.locf(mcar_data)
lvcf_mar_data <- na.locf(mar_data)

# General Linear Mixed Model ----------------------------------------------

#View slopes of different United States
ggplot(mcar_data %>% filter(country %in% c("Albania","United States","Sweden","Spain","San Marino")), 
       aes(x=year, y=suicides_no/population, group=country, color=country))+geom_point()

#function to fill in missing values in dataset via generalized linear mixed model, with random intercept and fixed mean
glmm_intercept_func <- function(df){
  #convert suicide no to rates per 100, scale year and gdp_per_capita
  df <- df %>% mutate(suicide_rate=suicides_no/population*100000,
                   scaled_year = scale(year),
                   scaled_gdp_per_capita = scale(gdp_per_capita))
  
  missing_model_intercept <- lmer(suicide_rate ~ scaled_year + scaled_gdp_per_capita+(1|country), data= df)
  missing_data_df <- df %>% filter(is.na(suicide_rate))
  
  #generalized linear mixed model for random effect for intercept
  missing_model_intercept <- lmer(suicide_rate ~ scaled_year + scaled_gdp_per_capita+(1|country), data= df)
  
  #predict rows with missing values
  fitted_values <- lme4:::predict.merMod(missing_model_intercept,missing_data_df %>% 
                                           dplyr::select(country,scaled_year, scaled_gdp_per_capita))
  #add year and country to fitted values to merge back into mcar dataset
  df$filled_suicide_rate <- df$suicide_rate
  df$filled_suicide_rate[which(is.na(df$filled_suicide_rate))] <-fitted_values
  return(df)
}

mcar_glmm_df <- glmm_intercept_func(mcar_data)
mar_glmm_df <- glmm_intercept_func(mar_data)




