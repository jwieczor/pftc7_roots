### Summary values extracted from clean soil texture and nutrients data

### 1. Set up ----
library(tidyverse)

#### load in soil data
soil <- read_csv('data/soil/clean/clean_soil.csv')

# summarise texture and nutriets at each elevation
soil_summary <- soil %>%
  group_by(soil_measurements, elevation_m_asl) %>%
  summarise_at('values', list(mean = mean, sd = sd), na.rm = T)