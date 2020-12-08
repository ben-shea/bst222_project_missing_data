#########################
# Title: BST 222 Project 
# Purpose: Create missing data for project
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

# MCAR --------------------------------------------------------------------

mcar_missing_data_country_year <- sample(data_subset$country_year,100)

mcar_data <- data
mcar_data$suicides_no[which(mcar_data$country_year %in% mcar_missing_data_country_year)] <- NA 
mcar_data[which(is.na(mcar_data$suicides_no)),]

# MAR ---------------------------------------------------------------------

# sample to remove data using different probs based off quartile of gdp_per_year
data_subset$gdp_quartile <- cut(data_subset$gdp_for_year, quantile(data_subset$gdp_for_year), labels = c("0-25","25-50","50-75","75-100"))

mar_sampling <- function(quartile, num_sample){
  data_quartile_country_year <- data_subset %>% filter(gdp_quartile==quartile) %>% select(country_year) %>% pull()
  sampled_countries <- sample(data_quartile_country_year, num_sample)
  return(sampled_countries)
}

mar_data_0_25 <- mar_sampling("0-25",45)
mar_data_25_50 <- mar_sampling("25-50",30)
mar_data_50_75 <- mar_sampling("50-75",15)
mar_data_75_100 <- mar_sampling("75-100",10)
mar_dat_country_year <- c(mar_data_0_25,mar_data_25_50,mar_data_50_75,mar_data_75_100)

mar_data <- data
mar_data$suicides_no[which(mar_data$country_year %in% mar_dat_country_year)] <- NA
mar_data[which(is.na(mar_data$suicides_no)),]

data <- data %>% 
  select(country_year, suicides_no) %>% 
  rename(suicides_no_org = suicides_no)

mar_data <- mar_data %>% 
  left_join(data, by = c("country_year")) 
mcar_data <- mcar_data %>% 
  left_join(data, by = c("country_year"))

# OUTPUT DATA -------------------------------------------------------------

write_csv(mcar_data, "../clean_data/mcar_data.csv")
write_csv(mar_data, "../clean_data/mar_data.csv")


