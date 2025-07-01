### Summary values extracted from clean soil texture and nutrients data
# modified 01/07/2025 by Joe

### 1. Set up ----
library(tidyverse)

#### load in soil data
soil <- read_csv('xii_soil_texture_nutrients/xii_PFTC7_SA_clean_soil_2023.csv')

# summarise texture and nutriets at each elevation
soil_summary <- soil %>%
  group_by(variable, elevation_m_asl) %>%
  summarise_at('value', list(mean = mean, sd = sd), na.rm = T)
