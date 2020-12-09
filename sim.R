#########################
# Title: BST 222 Project 
# Purpose: Simulate missing data, perform imputation, compare methods
#########################


# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)


# LOAD DATA ---------------------------------------------------------------
data <- read_csv("../clean_data/suicide_data.csv")

data <- data %>% mutate(country_year = paste0(country, year))

first_year_country <- data[!duplicated(data$country),]$country_year

#remove first year/instance of a country so that they wouldn't be sampled out; need baseline suicide rate
data_subset <- data[-which(data$country_year %in% first_year_country),]

# sample to remove data using different probs based off quartile of gdp_per_year
data_subset$gdp_quartile <- cut(data_subset$gdp_for_year, quantile(data_subset$gdp_for_year), labels = c("0-25","25-50","50-75","75-100"))


# SIMULATION FUNCTIONS ----------------------------------------------------

# 1. Missing Data Generating Functions

# a. Function that removes observations from the outcome (suicides_no) using "country_year" as the indicator using MCAR
mk_mcar <- function(df, seed, n) {
  
  set.seed(seed)
  mcar_missing_data_country_year <- sample(data_subset$country_year,n)
  
  mcar_data <- df
  mcar_data$suicides_no[which(mcar_data$country_year %in% mcar_missing_data_country_year)] <- NA 
  mcar_data[which(is.na(mcar_data$suicides_no)),]
  
  data_og <- df %>% 
    select(country_year, suicides_no) %>% 
    rename(suicides_no_org = suicides_no)
  
  mcar_data <- mcar_data %>% 
    left_join(data_og, by = c("country_year"))
  
  return(mcar_data)
}  

# b. Function that removes observations from the outcome (suicides_no) using MCAR

# i. mar sampling function
mar_sampling <- function(quartile, num_sample){
  data_quartile_country_year <- data_subset %>% filter(gdp_quartile==quartile) %>% select(country_year) %>% pull()
  
  # For simplicity, setting seed with num_sample
  set.seed(num_sample)
  sampled_countries <- sample(data_quartile_country_year, num_sample)
  return(sampled_countries)
}

# ii. Function that returns MCAR data
mk_mar <- function(df, quart_n) {

  mar_data_0_25 <- mar_sampling("0-25",quart_n[1])
  mar_data_25_50 <- mar_sampling("25-50",quart_n[2])
  mar_data_50_75 <- mar_sampling("50-75",quart_n[3])
  mar_data_75_100 <- mar_sampling("75-100",quart_n[4])
  
  mar_dat_country_year <- c(mar_data_0_25,mar_data_25_50,mar_data_50_75,mar_data_75_100)
  
  mar_data <- df
  mar_data$suicides_no[which(mar_data$country_year %in% mar_dat_country_year)] <- NA
  mar_data[which(is.na(mar_data$suicides_no)),]
  
  data_og <- df %>% 
    select(country_year, suicides_no) %>% 
    rename(suicides_no_org = suicides_no)
  
  mar_data <- mar_data %>% 
    left_join(data_og, by = c("country_year")) 

  
  return(mar_data)
}

# Test missing data creation
mcar <- mk_mcar(data, 12345, 100)
mar <- mk_mar(data,  c(45, 30, 15,10))

# 2. Missing Imputation Functions  

# 3. Evaluation Functions
# a. Variance

# i. Sample variance
var_within_yr <- function(df) {
  df %>% 
    group_by(year) %>% 
    summarize(var_yr = var(suicides_no)) %>% 
    return()
  
}

# ii. Delta method variance

# B. Covariance

# i. Sample covariance
# Will only work when size of sample is the same (still investigating this)
cov_within_two_years <- function(df, yr1, yr2) {
  cov(
  df %>% filter(year == yr1) %>% pull(suicides_no),
  df %>% filter(year == yr2) %>% pull(suicides_no)) %>% 
    return()
}

# ii. Other methods?


# SIMULATION --------------------------------------------------------------

# Function to generate missing data, run imputation methods, compare them, and output summary