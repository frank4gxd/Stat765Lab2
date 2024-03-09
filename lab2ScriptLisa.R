library(tidyverse)
library(readr)
library(zoo)
library(purrr)

raw.df <- readxl::read_xlsx(file.choose(),
                            skip = 7, 
                            col_names=FALSE)

## create column names x 
header_names <- expand.grid(x = c('small', 'medium', 'large'),
                            y = c('offices'),
                            z = c('concrete', 'curtainWall')) %>% 
  mutate(nn = paste(x, y, z, sep='_')) %>% 
  pull(nn)  

names(raw.df) <- c('weather', 'description', header_names)
 

## dealing with merged cells, create 'group' e.g. overall, sensible cooling etc
x1 <- as.data.frame(matrix(rep(NA, length(raw.df)), nrow = 1))
names(x1) <- names(raw.df)
df2 <- bind_rows(x1, raw.df) %>% 
  mutate(group = ifelse(is.na(description), 'overall',
                        ifelse(description=='Sensible Cooling', 'sensible_cooling',
                               ifelse(description=='Sensible Heating', 'sensible_heating', NA)))) %>% 
  fill(group, .direction = 'down') %>% 
  fill(weather, .direction = 'down') %>% 
  filter(!(description %in%c(NA, 'Sensible Cooling',
                             'Sensible Heating'))) %>% 
  pivot_longer(cols = small_offices_concrete:large_offices_curtainWall,
               names_to = 'CATEGORY',
               values_to = 'value') %>% 
  separate(CATEGORY, c('office_size', 'office', 'office_type'),
           sep='_') %>% 
  select(-office)