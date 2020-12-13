#########################
# Title: BST 222 Project 
# Purpose: Simulate missing data, perform imputation, compare methods
#########################


# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(zoo)
library(mice)
library(lme4)
library(rstudioapi)


# LOAD DATA ---------------------------------------------------------------

data <- read_csv("clean_data/suicide_data.csv")


data <- data %>% mutate(country_year = paste0(country, year))



# MODIFY OUTCOME ----------------------------------------------------------
data <- data %>% 
  mutate(
    suicides_no = (suicides_no/population)*100000
  )

# CREATE SUBSET -----------------------------------------------------------
first_year_country <- data[!duplicated(data$country),]$country_year

#remove first year/instance of a country so that they wouldn't be sampled out; need baseline suicide rate
data_subset <- data[-which(data$country_year %in% first_year_country),]

# sample to remove data using different probs based off quartile of gdp_per_year
data_subset$gdp_quartile <- cut(data_subset$gdp_for_year, quantile(data_subset$gdp_for_year), 
                                labels = c("0-25","25-50","50-75","75-100"))


# SIMULATION FUNCTIONS ----------------------------------------------------

# 1. Missing Data Generating Functions

# a. Function that removes observations from the outcome (suicides_no) using "country_year" as the indicator using MCAR
mk_mcar <- function(df, n, seed) {
  
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
mar_sampling <- function(quartile, num_sample, seed){
  data_quartile_country_year <- data_subset %>% filter(gdp_quartile==quartile) %>% select(country_year) %>% pull()
  
  # Take a maximum of 60% of the rows missing per quartile
  set.seed(seed)
  sampled_countries <- sample(data_quartile_country_year, min(num_sample, floor(length(data_quartile_country_year)*.6)))
  return(sampled_countries)
}

# ii. Function that returns MCAR data
mk_mar <- function(df, rows_to_rm, seed) {
  quart_n <- floor(c(0.45*rows_to_rm, 0.30*rows_to_rm, 0.15*rows_to_rm,0.10*rows_to_rm))
  mar_data_0_25 <- mar_sampling("0-25",quart_n[1], seed)
  mar_data_25_50 <- mar_sampling("25-50",quart_n[2], seed)
  mar_data_50_75 <- mar_sampling("50-75",quart_n[3], seed)
  mar_data_75_100 <- mar_sampling("75-100",quart_n[4], seed)
  
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


# 2. Missing Imputation Functions  

# i. Last value carried forward 
lvcf <- function(df) {
  df %>% 
    arrange(country_year) %>% 
    group_by(year) %>% 
    na.locf %>% 
    ungroup() %>% 
    return()
} 

# ii. mean value
mean_val <- function(df) {
  
}

# iii. GLMM
glmm <- function(df) {
  
} 

# iv. KNN 

# a. KNN 1
knn1 <- function(df) {
  
}

# b. KNN 2 
knn2 <- function(df) {
  
}

# c. KNN 3
knn3 <- function(df) {
  
}


# 3. Evaluation Functions
# a. Variance

# i. Sample variance
var_within_yr <- function(df) {
  df %>% 
    group_by(year) %>% 
    summarize(var_yr = var(suicides_no),.groups = "drop") %>% 
    return()
  
}

# ii. Delta method variance
# augment_missing <- function(df){
#   df <- df %>% mutate(I = ifelse(is.na(suicides_no), 0, 1) )
#   df$suicides_no[which(is.na(df$suicides_no))] <- 0
#   
#   year_means <- df %>% group_by(year, gdp_quartile) %>% summarise(
#                       tot_suicide =sum(suicides_no), 
#                       tot_I = sum(I), 
#                       n = n())
#   year_means <- year_means %>% mutate(avg_suicide = tot_suicide/n,
#                                       avg_I = tot_I/n)
#   return(year_means %>% select(c(year, gdp_quartile, avg_suicide, avg_I)))
#     
#   
# }

delta_var_diff <- function(df, yr1, yr2){
  # df -> imputed data frame

  # implementation of formula (4)
  calc_stats <- function(df, yr1, yr2){
    year_stats <- df %>% filter(year %in% c(yr1, yr2)) %>% group_by(year) %>%
                         summarise(y_bar = mean(suicides_no),
                                   y_var = var(suicides_no),
                                   n = n())
    year_stats['xy_cov'] = cov(subset(data, year == yr1)$suicides_no,
                                   subset(data, year == yr2)$suicides_no)
    
    year_stats['x_bar'] = year_stats$y_bar[1] #yr1
    year_stats['x_var'] = year_stats$y_var[1] #yr1
    
    return(year_stats[2,])
    
  }
  
  calc_ci_ratio <- function(stats){
        se <- sqrt((stats$y_var - 
              2*stats$y_bar/stats$x_bar*stats$xy_cov + 
              (stats$y_bar**2/stats$x_bar**2)*stats$x_var))/(sqrt(stats$n)*stats$x_bar)
        low <- stats$y_bar/stats$x_bar - 1.96*se
        up <- stats$y_bar/stats$x_bar  + 1.96*se
        ci<- c(low, up)
        return(ci)
  }
  
  
  calc_ci_ratio2 <- function(stats){
    se <- sqrt((stats$y_var - 
                  2*stats$y_bar/stats$x_bar*stats$xy_cov + 
                  (stats$y_bar**2/stats$x_bar**2)*stats$x_var))/(sqrt(stats$n)*stats$x_bar)
    low <- stats$y_bar/stats$x_bar - 1.96*se
    up <- stats$y_bar/stats$x_bar + 1.96*se
    ci<- c(low, up)
    return(ci)
  }
  
  
  
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


# c. Root MSE
root_MSE <- function(df) {
  mean((df$suicides_no_org - df$suicides_no)^2) %>% 
    sqrt() %>% 
    return()
}


# SIMULATION --------------------------------------------------------------

# Function to generate missing data, run imputation methods, compare them, and output summary  

# N is number of iterations
# perc_missing is the percentage of observations that go missing (assuming we begin with a full dataset)
sim <- function(N = 100, perc_missing = 0.25) {
  
  # get the number of rows to remove
  rows_to_rm <- ceiling(nrow(data)*perc_missing)
  
  # Create empty containers for different results
  # If it's a single value per iteration, we can create dataframes 
  var_by_yr <- list()
  root_mse <- data.frame(sim_run = NULL, ind = NULL, impute_type = NULL, missing_type = NULL, rmse = NULL)
  for (i in 1:N) {
    print(sprintf("Running simulation %f for %f missing data", i, perc_missing))
    # Create data sets 
    mcar <- mk_mcar(data, rows_to_rm, i)
    mar <- mk_mar(data,  rows_to_rm, i)
    
    # Impute values
    impute_mcar_lvcf <- lvcf(mcar)
    impute_mar_lvcf <- lvcf(mar)
    
    
    # Get Root MSE
    root_mse <-  bind_rows(root_mse,
                           data.frame(
                             sim_run = perc_missing,
                             ind = i,
                             impute_type = "Last Value Carried Forward",
                             missing_type = "MCAR",
                             rmse = root_MSE(impute_mcar_lvcf)
                           ),
                           data.frame(
                             sim_run = perc_missing,
                             ind = i,
                             impute_type = "Last Value Carried Forward",
                             missing_type = "MAR",
                             rmse = root_MSE(impute_mar_lvcf)
                           )
                           ### Add in other methods
                           )
    
    # Get variance by year
    var_by_yr[["var_mcar"]] <- bind_rows(var_by_yr[["var_mcar"]], var_within_yr(impute_mcar_lvcf))
    var_by_yr[["var_mar"]] <- bind_rows(var_by_yr[["var_mar"]], var_within_yr(impute_mcar_lvcf))
  }
  
  # Get averages across all simulations (variance by year)
  var_by_yr[["var_mcar"]] <- var_by_yr[["var_mcar"]] %>% group_by(year) %>% summarize(avg_var_within_yr = mean(var_yr), .groups = "drop") %>% mutate(sim_run = perc_missing)
  
  var_by_yr[["var_mar"]] <- var_by_yr[["var_mar"]] %>% group_by(year) %>% summarize(avg_var_within_yr = mean(var_yr), .groups = "drop") %>% mutate(sim_run = perc_missing)
  
  # Get average RMSE for each set of simulations
  root_mse <- root_mse %>% group_by(sim_run, impute_type, missing_type) %>% summarize(avg_rmse = mean(rmse), .groups = "drop")
  
  return(list(var_by_yr, root_mse))
}


### RUN SIMULATION 

percs = seq(0.1, .6, by = 0.01)

system.time(
  sim_output <- pmap(.l = list(.x = percs), .f = ~sim(perc_missing = .x, N = 10))
)


#### Plot RMSE
# Combine data into one data frame
rmse_df <- sim_output %>% 
  # Extract the MSE output for percent missing
  map(function(x) return(x[[2]])) %>% 
  # Row bind these together
  map_df(bind_rows)

rmse_df %>% 
  ggplot(aes(x = sim_run, y = avg_rmse, color = impute_type)) +
  geom_line() +
  scale_color_discrete("Imputation Type") +
  facet_grid(missing_type ~ .) +
  xlab("% of Data Missing") + 
  ylab("Average Root MSE") + 
  ggtitle("Simulation: Average MSE for Different Imputation Methods") + 
  theme_bw()  




