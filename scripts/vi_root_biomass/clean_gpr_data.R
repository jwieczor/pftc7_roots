### Clean raw soil GPR plot, transect and root biomass data
# modified 01/07/2025 by Joe

### 1. Set up ----
library(tidyverse)
library(osfr)

### retrieve raw data files from the OSF project page
osf_retrieve_node('hk2cy') %>%
  osf_ls_files(path = 'raw_data/vi_raw_root_biomass/') %>%
  filter(!str_ends(name, '.zip')) %>% # don't download the raw dzt files
  osf_download(path = 'raw_data/vi_raw_root_biomass/', conflicts = 'overwrite')

### load in scan file name changes
name_changes <- read_csv('raw_data/vi_raw_root_biomass/scan_file_name_changes.csv')

### 2. Format raw and save clean PLOT data ----
### load in gpr plot data
plot_dir <- list.files('raw_data/vi_raw_root_biomass/plots/', full.names = T)
plots <- lapply(plot_dir, read_csv) %>% bind_rows()

### format
plots <- plots %>%
  select(scan_file = `SCAN FILE`, type = TYPE, position = `POSITION (m)`, depth = `DEPTH (m)`, amplitude = AMPLITUDE, pixel_count = `PIXEL COUNT`) %>%
  filter(str_detect(scan_file, 'ROOT')) # filter out extra metadata in files on zone detections

# add in columns and change names
clean_plots <- plots %>%
  left_join(name_changes, by = c('scan_file' = 'original_name')) %>%
  mutate(site_id = substr(new_name, 16, 16),
         aspect = substr(new_name, nchar(new_name), nchar(new_name)),
         aspect = case_when(
           aspect == 'e' ~ 'east',
           aspect == 'w' ~ 'west'
         ),
         elevation_m_asl = case_when(
           site_id == 1 ~ 2000,
           site_id == 2 ~ 2200,
           site_id == 3 ~ 2400,
           site_id == 4 ~ 2600
         ),
         date = case_when(
           site_id %in% c(1, 2, 4) ~ '14/12/2023',
           site_id == 3 ~ '9/12/2023'
         ),
         type = case_when(
           type == 'DETECT' ~ 'detect',
           type == 'MARKER' ~ 'marker'
         )) %>%
  dplyr::select(date, elevation_m_asl, site_id, aspect, scan_file = new_name, type:pixel_count) %>%
  mutate(across(depth:pixel_count, ~gsub('-', NA, .)),
         across(position:pixel_count, ~as.numeric(.))) %>%
  pivot_longer(cols = position:pixel_count, names_to = 'variable') %>%
  mutate(unit = case_when(
    variable == 'position' ~ 'm',
    variable == 'depth' ~ 'm',
    variable == 'amplitude' ~ 'dB',
    variable == 'pixel_count' ~ 'count'
  ))

### save clean plot data
write_csv(clean_plots, 'vi_root_biomass/vi_PFTC7_clean_gpr_plot_2023.csv')

### 3. Format raw and save clean TRANSECT and ROOT BIOMASS data ----
### load in root biomass data
list.files('raw_data/vi_raw_root_biomass')

root_biomass <- read_csv('raw_data/vi_raw_root_biomass/PFTC7_SA_raw_root_biomass_2023.csv') %>%
  dplyr::select(site_id, transect, position_m:dry_soil_mass_g) %>%
  mutate(site_id = as.character(site_id),
         transect = as.character(transect),
         root_to_soil_ratio = dry_root_mass_g/dry_soil_mass_g,
         root_to_soil_and_stone_ratio = dry_root_mass_g/(dry_soil_mass_g + stone_mass_g)) %>%
  rename(position = position_m)

### load in gpr transect data
transects_dir <- list.files('raw_data/vi_raw_root_biomass/transects/', full.names = T)
transects <- lapply(transects_dir, function(x) read_csv(x, col_types = cols(.default = 'd', `SCAN FILE` = 'c', TYPE = 'c'))) %>% bind_rows()

### format
transects <- transects %>%
  select(scan_file = `SCAN FILE`, type = TYPE, position = `POSITION (m)`, depth = `DEPTH (m)`, amplitude = AMPLITUDE, pixel_count = `PIXEL COUNT`) %>%
  filter(str_detect(scan_file, 'ROOT')) %>% # filter out extra metadata in files on zone detections
  filter(type != 'MARKER') # remove marker from the transect files

# add in columns and change names
clean_transects <- transects %>%
  left_join(name_changes, by = c('scan_file' = 'original_name')) %>%
  mutate(site_id = substr(new_name, 16, 16),
         aspect = 'west',
         transect = substr(new_name, nchar(new_name), nchar(new_name)),
         elevation_m_asl = case_when(
           site_id == 1 ~ 2000,
           site_id == 2 ~ 2200,
           site_id == 3 ~ 2400,
           site_id == 4 ~ 2600
         ),
         date = case_when(
           site_id %in% c(1, 2, 4) ~ '14/12/2023',
           site_id == 3 ~ '9/12/2023'
         )) %>%
  dplyr::select(date, elevation_m_asl, site_id, aspect, transect, scan_file = new_name, position:pixel_count)

# join root biomass data on to gpr transects 
joined_transects <- clean_transects %>%
  left_join(root_biomass, by = c('site_id', 'transect', 'position')) %>%
  dplyr::select(date:scan_file, sample_no, position, depth, amplitude, pixel_count, dry_root_mass = dry_root_mass_g, stone_mass = stone_mass_g, dry_soil_mass = dry_soil_mass_g, root_to_soil_ratio, root_to_soil_and_stone_ratio) %>%
  drop_na() %>%
  pivot_longer(cols = position:root_to_soil_and_stone_ratio, names_to = 'variable') %>%
  mutate(unit = case_when(
    variable == 'position' ~ 'm',
    variable == 'depth' ~ 'm',
    variable == 'amplitude' ~ 'dB',
    variable == 'pixel_count' ~ 'count',
    variable == 'dry_root_mass' ~ 'g',
    variable == 'stone_mass' ~ 'g',
    variable == 'dry_soil_mass' ~ 'g',
    variable == 'root_to_soil_ratio' ~ 'g g-1',
    variable == 'root_to_soil_and_stone_ratio' ~ 'g g-1'))

### save clean transect and root biomass data
write_csv(joined_transects, 'vi_root_biomass/vi_PFTC7_clean_gpr_transect_root_biomass_2023.csv')
