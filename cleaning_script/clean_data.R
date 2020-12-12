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
  group_by(country, year) %>% 
  summarise(
    suicides_no  = sum(suicides_no, na.rm = T),
    population  = sum(population, na.rm = T),
    gdp_for_year = unique(gdp_for_year, na.rm = T),
    gdp_per_capita = unique(gdp_per_capita, na.rm = T)
  ) %>% ungroup()
sui_agg <- sui_agg %>% group_by(country) %>% 
  mutate(population_change = population - lag(population),
         gdp_per_capita_change = (gdp_per_capita - lag(gdp_per_capita))/gdp_per_capita) %>% 
  ungroup()

age_dist <- sui %>% group_by(country, year, age) %>% 
  summarise(population = sum(population, na.rm=T)) %>%
  spread(age,population)
age_dist$total_pop <- rowSums(age_dist[unique(sui$age)])
age_dist <- age_dist %>%
  mutate_at(vars(-total_pop,-country, -year), funs(. / total_pop)) %>%
  select(everything(),-total_pop) %>% ungroup()
age_dist <- age_dist %>% mutate(mean_age = 10*.$`5-14 years`+20*.$`15-24 years`+30*.$`25-34 years`
                                +45*.$`35-54 years` + 65*.$`55-74 years` + 85*.$`75+ years`)

sex_dist <- sui %>% group_by(country, year, sex) %>% 
  summarise(population = sum(population, na.rm=T)) %>%
  spread(sex,population)
sex_dist$total_pop <- rowSums(sex_dist[unique(sui$sex)])
sex_dist <- sex_dist %>%
  mutate_at(vars(-total_pop,-country, -year), funs(. / total_pop)) %>%
  select(everything(),-total_pop)


# DATA CLEANING -----------------------------------------------------------

#join in age and sex distributions

sui_agg <- sui_agg %>% left_join(age_dist, by=c('country', 'year')) %>% left_join(sex_dist, by=c('country', 'year')) %>% select(everything(),-male)

# Remove 2016 data
sui_agg <- sui_agg %>% 
  filter(year != 2016)

# OUTPUT DATA -------------------------------------------------------------
write_csv(sui_agg, "../clean_data/suicide_data.csv")
