### Clean raw soil texture and nutrients data

### 1. Set up ----
library(tidyverse)
library(osfr)

### retrieve raw data file from the OSF project page
osf_retrieve_file('https://osf.io/yb9t2') %>%
  osf_download(path = 'raw_data/12_raw_soil_texture_nutrients/')

### load in soil data
soil <- read_csv('raw_data/12_raw_soil_texture_nutrients/PFTC7_SA_raw_soil_2023.csv')

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
    tc_perc = C, tn_perc = TN, tp_mg_kg = Total_P, cec_cmol_kg = CEC, ph = pH, clay_perc = Clay, sand_perc = Sand, silt_perc = Silt) %>%
  pivot_longer(cols = tc_perc:silt_perc, names_to = 'variable', values_to = 'value')

### 4. Save clean data
write_csv(soil_format, '12_soil_texture_nutrients/PFTC7_SA_clean_soil_2023.csv')
