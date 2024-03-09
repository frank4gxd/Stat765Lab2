library(tidyverse)

sort_data <- function()
{
raw.df <- readxl::read_xlsx(file.choose(),skip=7,col_names = FALSE)


#header_names <- expand.grid(x = c('small', 'medium', 'large'),
#                            y = c('offices'),
#                           z = c('concrete', 'curtainWall')) %>% 
#  mutate(nn = paste(x, y, z, sep='_')) %>% 
# pull(nn)  

names(raw.df) <- c('weather', 'description', 'small_concrete','small_curtain','medium_concrete','medium_curtain','large_concrete','large_curtain')

#raw.df <- raw.df %>% filter(!is.na(description))
#raw.df <- fill(raw.df,weather,.direction="down")


x1 <- as.data.frame(matrix(rep(NA, length(raw.df)), nrow = 1))
names(x1) <- names(raw.df)
df2 <- bind_rows(x1, raw.df) %>% 
  mutate(group = ifelse(is.na(description), 'overall',
                        ifelse(description=='Sensible Cooling', 'sensible_cooling',
                               ifelse(description=='Sensible Heating', 'sensible_heating', NA)))) %>% 
  fill(group, .direction = 'down') %>% 
  fill(weather, .direction = 'down') %>% 
  filter(!(is.na(description) | description=='Sensible Cooling' | description=='Sensible Heating') ) %>% 
  pivot_longer(cols = small_concrete:large_curtain,
               names_to = 'CATEGORY',
               values_to = 'value') %>% 
  separate_wider_delim(CATEGORY,'_',names=c('office_size','office_type'))

} 

  