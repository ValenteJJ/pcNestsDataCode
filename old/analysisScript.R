
rm(list=ls())

require(groundhog)


groundhog.library(unmarked, '2025-04-01')
groundhog.library(terrainr, '2025-04-01')
groundhog.library(tidyterra, '2025-04-01')
groundhog.library(cowplot, '2025-04-01')
groundhog.library(ggnewscale, '2025-04-01')
groundhog.library(ggspatial, '2025-04-01')
groundhog.library(sf, '2025-04-01')
groundhog.library(tigris, '2025-04-01')
groundhog.library(terra, '2025-04-01')
groundhog.library(FedData, '2025-04-01')
groundhog.library(tidyverse, '2025-04-01')
groundhog.library(spAbundance, '2025-04-01')

#----------------------------------------------------
#Summary information about plots
#----------------------------------------------------

plots = st_read('plots.shp')

plots %>% 
  filter(year==2011) %>% 
  summarise(mean = mean(hectares),
            sd = sd(hectares),
            min = min(hectares),
            max = max(hectares))

plots %>% 
  filter(year==2012) %>% 
  summarise(mean = mean(hectares),
            sd = sd(hectares),
            min = min(hectares),
            max = max(hectares))


#----------------------------------------------------
#Summary information about point counts
#----------------------------------------------------

pcLocations = read.csv("pcLocations.csv")
surveys = read.csv('surveys.csv')

#Survey dates
pcLocations %>% 
  left_join(surveys, by='point') %>% 
  group_by(year) %>% 
  summarise(startDate = min(date),
            endDate = max(date))

#Surveys per year per point
pcLocations %>% 
  left_join(surveys, by='point') %>% 
  group_by(year, point) %>% 
  summarise(count = n())

#Points per plot per year
pcLocations %>% 
  left_join(surveys, by='point') %>% 
  select(year, point, plot) %>% 
  unique() %>% 
  group_by(year, plot) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(plot, year) %>% 
  pivot_wider(names_from=year, values_from=count)




  
