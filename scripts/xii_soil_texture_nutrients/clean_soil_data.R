### Clean raw soil texture and nutrients data
# modified 01/07/2025 by Joe

### 1. Set up ----
library(tidyverse)
library(osfr)

### retrieve raw data file from the OSF project page
osf_retrieve_file('https://osf.io/yb9t2') %>%
  osf_download(path = 'raw_data/xii_raw_soil_texture_nutrients/', conflicts = 'overwrite')

### load in soil data
soil <- read_csv('raw_data/xii_raw_soil_texture_nutrients/PFTC7_SA_raw_soil_2023.csv')

### 2. Clean data
### filter out extra record
soil_filt <- soil %>% 
  filter(BlockNo != '5_EXTRA FIC 9697')

### 3. Format data
soil_format <- soil_filt %>%
  mutate(id = gsub(' ', '', substr(BlockNo, 7, nchar(BlockNo))),
         date = "2023-12-16",
         aspect = substr(BlockNo, 3, 3),
         site_id = substr(BlockNo, 1, 1),
         elevation_m_asl = substr(BlockNo, 1, 1),
         plot_id  = substr(BlockNo, 5, 5)) %>%
  mutate(aspect = case_when(
    aspect == 'W' ~ 'west',
    aspect == 'E' ~ 'east'
  ),elevation_m_asl = case_when(
           elevation_m_asl == 1 ~ 2000,
           elevation_m_asl == 2 ~ 2200,
           elevation_m_asl == 3 ~ 2400,
           elevation_m_asl == 4 ~ 2600,
           elevation_m_asl == 5 ~ 2800,
         )) %>%
  mutate(elevation_m_asl = as.factor(elevation_m_asl)) %>%
  dplyr::select(id, date, aspect, site_id, elevation_m_asl, plot_id, 
    tc = C, tn = TN, tp = Total_P, cec = CEC, ph = pH, clay = Clay, sand = Sand, silt = Silt) %>%
  pivot_longer(cols = tc:silt, names_to = 'variable', values_to = 'value') %>%
  mutate(unit = case_when(
    variable == 'tc' ~ '%',
    variable == 'tn' ~ '%',
    variable == 'tp' ~ 'mg kg-1',
    variable == 'cec' ~ 'cmol kg-1',
    variable == 'pH' ~ '',
    variable == 'clay' ~ '%',
    variable == 'sand' ~ '%',
    variable == 'silt' ~ '%'))

### 4. Save clean data
write_csv(soil_format, 'xii_soil_texture_nutrients/xii_PFTC7_clean_soil_2023.csv')
