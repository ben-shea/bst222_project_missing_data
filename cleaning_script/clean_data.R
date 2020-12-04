#########################
# Title: BST 222 Project 
# Purpose: Process data for project
# Source: https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016?select=master.csv
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)


# LOAD DATA ---------------------------------------------------------------

sui <- read_csv("../source_data/master.csv")

# clean field names
names(sui) <- str_replace_all(str_trim(str_replace_all(names(sui), "\\(\\$\\)|\\/", "")), " |-", "_")



# AGGREGATE ---------------------------------------------------------------

# Roll up data to country-year level

sui_agg <- sui %>% 
  group_by(country_year) %>% 
  summarize(
    suicides_no  = sum(suicides_no, na.rm = T),
    population  = sum(population, na.rm = T),
    gdp_for_year = unique(gdp_for_year, na.rm = T)
  ) %>% ungroup()



# OUTPUT DATA -------------------------------------------------------------

write_csv(sui_agg, "../clean_data/suicide_data.csv")
