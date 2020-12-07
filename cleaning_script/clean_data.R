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
  summarise(
    suicides_no  = sum(suicides_no, na.rm = T),
    population  = sum(population, na.rm = T),
    gdp_for_year = unique(gdp_for_year, na.rm = T)
  ) %>% ungroup()

age_dist <- sui %>% group_by(country_year, age) %>% 
  summarise(population = sum(population, na.rm=T)) %>%
  spread(age,population)
age_dist$total_pop <- rowSums(age_dist[unique(sui$age)])
age_dist <- age_dist %>%
  mutate_at(vars(-total_pop,-country_year), funs(. / total_pop)) %>%
  select(everything(),-total_pop)

sex_dist <- sui %>% group_by(country_year, sex) %>% 
  summarise(population = sum(population, na.rm=T)) %>%
  spread(sex,population)
sex_dist$total_pop <- rowSums(sex_dist[unique(sui$sex)])
sex_dist <- sex_dist %>%
  mutate_at(vars(-total_pop,-country_year), funs(. / total_pop)) %>%
  select(everything(),-total_pop)

# DATA CLEANING -----------------------------------------------------------

#split country and year, then add them in as 2 new columns
country_year_split <- strsplit(sui_agg$country_year,
                               "(?<=[A-Za-z])\\s*(?=\\d+$)", perl = TRUE)
sui_agg$country <- sapply(country_year_split, "[", 1)
sui_agg$year <- sapply(country_year_split, "[", 2)

#join in age and sex distributions

sui_agg <- sui_agg %>% left_join(age_dist) %>% left_join(sex_dist) %>% select(everything(),-male)

# OUTPUT DATA -------------------------------------------------------------
write_csv(sui_agg, "../clean_data/suicide_data.csv")
