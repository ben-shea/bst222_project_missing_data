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
data <- read_csv("./clean_data/suicide_data.csv")

data <- data %>% mutate(country_year = paste0(country, year))

first_year_country <- data[!duplicated(data$country),]$country_year

#remove first year/instance of a country so that they wouldn't be sampled out; need baseline suicide rate
data_subset <- data[-which(data$country_year %in% first_year_country),]

# sample to remove data using different probs based off quartile of gdp_per_year
data_subset$gdp_quartile <- cut(data_subset$gdp_for_year, quantile(data_subset$gdp_for_year), 
                                labels = c("0-25","25-50","50-75","75-100"))


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
    left_join(data_og, by = c("country_year"))  %>% 
    mutate(gdp_quartile = cut(gdp_for_year, quantile(gdp_for_year), 
                          labels = c("0-25","25-50","50-75","75-100")))

  
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
augment_missing <- function(df){
  df <- df %>% mutate(I = ifelse(is.na(suicides_no), 0, 1) )
  df$suicides_no[which(is.na(df$suicides_no))] <- 0
  
  year_means <- df %>% group_by(year, gdp_quartile) %>% summarise(
                      tot_suicide =sum(suicides_no), 
                      tot_I = sum(I), 
                      n = n())
  year_means <- year_means %>% mutate(avg_suicide = tot_suicide/n,
                                      avg_I = tot_I/n)
  return(year_means %>% select(c(year, gdp_quartile, avg_suicide, avg_I)))
    
  
}

delta_var_diff <- function(df, yr1, yr2){
  # df -> grouped year/gdp quartile, avg_suicide, avg_I
  # var(X_yr2 - X_yr1) = var(X_yr2) + var(X_yr1) - 2Cov(X_yr1, X_yr2)
  
  # implementation of formula (4)
  calc_stats <- function(yr){
    year_stats <- df %>% filter(year == yr) %>% summarise(y_bar = mean(avg_suicide),
                                             x_bar = mean(avg_I),
                                             y_var = var(avg_suicide),
                                             x_var = var(avg_I),
                                             xy_cov = cov(avg_suicide, avg_I),
                                             n = n())
    return(year_stats)
    
  }
  
  calc_var_ratio <- function(stats){
            (stats$y_var - 
            2*stats$y_bar/stats$x_bar*stats$xy_cov + 
            (stats$y_bar**2/stats$x_bar**2)*stats$x_var)/(stats$n*stats$x_bar)
  }
  
  
  stats_1 = calc_stats(yr1)
  stats_2 =  calc_stats(yr2)
  
  # covariance
  t_1 = df %>% filter(year == yr1) %>% mutate(x = avg_suicide/avg_I) %>% .$x
  t_2 =  df %>% filter(year == yr2) %>% mutate(x = avg_suicide/avg_I) %>% .$x
  covar = cov(t_1, t_2)
  
  return(calc_var_ratio(stats_1) + calc_var_ratio(stats_2) -2*covar)
  
}

# B. Covariance

# i. Sample covariance
# Will only work when size of sample is the same (still investigating this)
cov_within_two_years <- function(df, yr1, yr2) {
  cov(
  df %>% filter(year == yr1) %>% pull(suicides_no),
  df %>% filter(year == yr2) %>% pull(suicides_no)) %>% 
    return()
}

calc_MSE <- function(df) {
  mean((df$suicides_no - df$suicides_no_org)^2)
}

# ii. Other methods?


# SIMULATION --------------------------------------------------------------

# Function to generate missing data, run imputation methods, compare them, and output summary
