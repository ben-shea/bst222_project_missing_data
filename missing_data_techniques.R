#########################
# Title: BST 222 Project 
# Purpose: Use Last value carried forward, propensity score matching, and regression imputation for missing values
#########################



# SETUP -------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(zoo)
library(mice)
library(lme4)
library(rstudioapi)

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

# PROPENSITY SCORE MATCH  --------------------------------------------------

# http://www.asasrms.org/Proceedings/y2004/files/Jsm2004-000368.pdf

ps_match <- function(data) {
  # run logistic regression of covariates on missing indicator & get predicted probs
  logit <- glm(missing_ind ~ year + factor(country) + population + gdp_for_year + female + mean_age, family="binomial", data=data)
  ps <- predict(logit, type="response")
  data$ps <- ps 
  
  # create seperate dfs for missing and not missing data
  data_missing <- data %>% filter(is.na(suicides_no))
  data_not_missing <- data %>% filter(!is.na(suicides_no))
  
  suicides_no_imp <- rep(NA, length(data_missing$ps))
  
  for (i in 1:length(data_missing$ps)) {
    
    ps_missing <- data_missing$ps[i]
    
    # filter compare missing obs propensity to other propensities for observed obs within same country
    missing_country <- data_missing %>% 
      filter(ps == ps_missing) %>% 
      select(country) %>% pull(.)
    
    # take abs difference between missing obs propensity and each observed obs within country
    df <- data_not_missing %>% 
      filter(country == missing_country) %>% 
      mutate(abs_diff = abs(ps_missing - ps))
    
    # find min of abs differences
    imp <- df %>% 
      filter(abs_diff == min(df$abs_diff)) %>% 
      select(suicides_no) %>% 
      pull(.)
    
    suicides_no_imp[i] <- imp
    
  }
  data_missing$suicides_no_imp <- suicides_no_imp
  
  # join imputed suicide number values to full data frame
  ps_data <- data %>% 
    left_join(data_missing %>% 
                select(country_year, suicides_no_imp), by = "country_year")
  
  # create vectors of imputed suicide values and original suicide values
  ps_suicides_no_org <- ps_data %>% 
    filter(missing_ind == 1) %>% 
    select(suicides_no_org) %>% pull(.)
  ps_suicides_no_imp <- ps_data %>% 
    filter(missing_ind == 1) %>% 
    select(suicides_no_imp) %>% pull(.)
  
  mse_ps <- mean((ps_suicides_no_imp - ps_suicides_no_org)^2)
  
  result <- list(ps_data = ps_data,
                 mse_ps = mse_ps)
  return(result)
}

mcar_ps <- ps_match(mcar_data)
ps_mcar_data <- mcar_ps$ps_data
mar_ps <- ps_match(mar_data)
ps_mar_data <- mar_ps$ps_data

# # DETERMINISTIC REGRESSION IMPUTATION VIA MICE  --------------------------------------------------
#https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
#https://stats.stackexchange.com/questions/421545/multiple-imputation-by-chained-equations-mice-explained

imp_mcar <- mice(mcar_data %>% select(suicides_no,mean_age,gdp_per_capita,female,year,country),group="country", method = "norm.predict", m = 1)
imp_mar <- mice(mar_data %>% select(suicides_no,mean_age,gdp_per_capita,female,year,country), group="country",method = "norm.predict", m = 1)
# Store data
data_imp_mcar <- complete(imp_mcar)
data_imp_mar <- complete(imp_mar)

impute_mcar_df <- mcar_data %>% select(suicides_no,mean_age,gdp_per_capita,female,year,country)
impute_mar_df <- mar_data %>% select(suicides_no,mean_age,gdp_per_capita,female,year,country)

#remove Cabo Verde, Dominica, Macau; only has 1 row
impute_mcar_df <- impute_mcar_df %>% filter(!country %in% c("Cabo Verde","Dominica","Macau"))
impute_mar_df <- impute_mar_df %>% filter(!country %in% c("Cabo Verde","Dominica","Macau"))

#https://stackoverflow.com/questions/58801562/combine-imputed-data-by-group-in-r-using-mice
impute_mcar.clean<-lapply(split(impute_mcar_df,impute_mcar_df$country), function(x){
                   print(x$country)
                   mice::complete(mice(x,m=1,method = "norm.predict",))
                  }
          )
impute_mar.clean<-lapply(split(impute_mar_df,impute_mcar_df$country), function(x){
  print(x$country)
  mice::complete(mice(x,m=1,method = "norm.predict",))
}
)

data_impute_mcar <- do.call(args = impute_mcar.clean, what = rbind)
data_impute_mar <- do.call(args = impute_mar.clean, what = rbind)

#check that there's no missing data in imputed dataset
data_impute_mcar %>% filter(is.na(data_impute_mcar$suicides_no)) #Bosnia and Herzegovina only has 2 rows
data_impute_mcar %>% filter(is.na(data_impute_mar$suicides_no)) 

# CALCULATE MSE -----------------------------------------------------------

# lvcf method
mcar_lvcf_suicides_no_org <- lvcf_mcar_data %>% 
  filter(missing_ind == 1) %>%
  select(suicides_no_org) %>% pull(.)
mcar_lvcf_suicides_no_imp <- lvcf_mcar_data %>% 
  filter(missing_ind == 1) %>%
  select(suicides_no) %>% pull(.)
mcar_mse_lvcf <- mean((mcar_lvcf_suicides_no_imp - mcar_lvcf_suicides_no_org)^2)

mar_lvcf_suicides_no_org <- lvcf_mar_data %>% 
  filter(missing_ind == 1) %>%
  select(suicides_no_org) %>% pull(.)
mar_lvcf_suicides_no_imp <- lvcf_mar_data %>% 
  filter(missing_ind == 1) %>%
  select(suicides_no) %>% pull(.)
mar_mse_lvcf <- mean((mar_lvcf_suicides_no_imp - mar_lvcf_suicides_no_org)^2)
  
# ps method
mcar_mse_ps <- mcar_ps$mse_ps
mar_mse_ps <- mar_ps$mse_ps


# comparison
mse_comp <- rbind(cbind(mcar_mse_lvcf, mar_mse_lvcf), cbind(mcar_mse_ps, mar_mse_ps))
colnames(mse_comp) <- c("mcar", "mse")
rownames(mse_comp) <- c("lvcf", "ps")
mse_comp
