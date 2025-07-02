# 1) Clean raw root trait field data, join with rootpainter scan and derive root traits; 2) Clean and join all root scan data
# created 18/10/2024 by Lina
# modified 01/07/2025 by Joe

### 1. Set up ----
library(tidyverse)
library(plotly)
library(osfr)

### retrieve raw data files from the OSF project page
osf_retrieve_node('hk2cy') %>%
  osf_ls_files(path = 'raw_data/v_raw_root_traits/') %>%
  filter(!str_ends(name, '.zip')) %>% # don't download the raw scan files
  osf_download(path = 'raw_data/v_raw_root_traits/', conflicts = 'overwrite')

### 2. Format raw and save clean root trait (field and scan) data ----
### load in root trait field data
roots <- read_csv('raw_data/v_raw_root_traits/PFTC7_SA_raw_root_trait_field_data_2023.csv')
names(roots)

### load in scans
rootpainter_scans <- read_csv('raw_data/v_raw_root_traits/PFTC7_SA_raw_rootpainter_scans_2023.csv')

### Join rootpainter scan metrics on to root trait field data
roots_joined <- roots %>%
  left_join(rootpainter_scans, by = 'File.Name')
names(roots_joined)

### known issues with root dry and volume - plot interactively
ggplotly(ggplot(roots_joined, aes(Volume.mm3, root_dry_mass)) +
           geom_point(aes(fill = File.Name)) +
           scale_fill_discrete(guide = 'none'))

### Change outlier values for FET3274, human input error (divide by 10)
roots_joined[roots_joined$File.Name == "FET3274", "root_dry_mass"] <- roots_joined[roots_joined$File.Name == "FET3274", "root_dry_mass"]/10

### Calculate derived root and leaf traits, clean value names
derived_roots <- roots_joined  %>% 
  mutate(bgb_agb = BGB/AGB,
         srl = (Total.Root.Length.mm*0.001)/root_dry_mass,
         rtd = root_dry_mass/(Volume.mm3*0.001),
         rdmc = (root_dry_mass*1000)/root_wet_mass_g,
         sla = (leaf_area/leaf_dry_mass),
         ldmc = (leaf_dry_mass/leaf_wet_mass_g)) %>% 
  rowwise() %>% 
  mutate(leaf_thickness = mean(c_across(starts_with("leaf_thick")), na.rm = F), .after = srl) %>%
  mutate(aspect = case_when(
    aspect == 'W' ~ 'west',
    aspect == 'NW' ~ 'northwest'
  ), 
  fire = ifelse(is.na(fire),0,1),
  sp = case_when(
    sp == 'EC' ~ 'Eragrostis capensis',
    sp == 'HF' ~ 'Harpochloa falx',
    sp == 'HPA' ~ 'Helichrysum pallidum',
    sp == 'SG' ~ 'Senecio glaberrimus',
    sp == 'TT' ~ 'Themeda triandra'
  ))

### Select relevant variables and rename variables
clean_roots <- derived_roots %>%
  dplyr::select(id = File.Name, date, aspect, site_id = siteID, elevation_m_asl = elevation, plant_id = plantID, species = sp, root_depth, no_root_scan, root_wet_mass = root_wet_mass_g, root_dry_mass = root_dry_mass, total_root_length = Total.Root.Length.mm, total_root_volume = Volume.mm3, rd = Average.Diameter.mm, bi = Branching.frequency.per.mm, srl, rtd, rdmc, tuber_wet_mass = tuber_wet_mass_g, tuber_dry_mass = tuber_dry_mass, reproductive_height = leafID, veg_height = height, no_leaves = no_leaf, leaf_wet_mass = leaf_wet_mass_g, leaf_dry_mass = leaf_dry_mass, leaf_area = leaf_area, sla, leaf_thickness, ldmc,  bgb_agb, aboveground_biomass = AGB, belowground_biomass = BGB) 

### known issues with RTD - plot interactively
ggplotly(ggplot(clean_roots, aes(root_dry_mass, total_root_volume)) +
           geom_point(aes(col = id, size = rtd)))
### root dry mass and volume for FEK5954 are not outside of typical range; leave value in but filter out for summaries and figures later.

### pivot long and add units as new column
long_roots <- clean_roots %>%
  pivot_longer(cols = root_depth:belowground_biomass, names_to = 'traits', values_to = 'value') %>%
  mutate(unit = case_when(
    traits == 'root_depth' ~ 'cm',
    traits == 'no_root_scan' ~ 'count',
    traits == 'root_wet_mass' ~ 'g',
    traits == 'root_dry_mass' ~ 'g',
    traits == 'total_root_length' ~ 'cm',
    traits == 'total_root_volume' ~ 'mm3',
    traits == 'rd' ~ 'mm',
    traits == 'bi' ~ 'count mm-1',
    traits == 'srl' ~ 'm g-1',
    traits == 'rtd' ~ 'g cm-3',
    traits == 'rdmc' ~ 'mg g-1',
    traits == 'tuber_wet_mass' ~ 'g',
    traits == 'tuber_dry_mass' ~ 'g',
    traits == 'reproductive_height' ~ 'cm',
    traits == 'no_leaf' ~ 'count',
    traits == 'veg_height' ~ 'cm',
    traits == 'leaf_wet_mass' ~ 'g',
    traits == 'leaf_dry_mass' ~ 'g',
    traits == 'leaf_area' ~ 'cm2',
    traits == 'sla' ~ 'cm2 g-1',
    traits == 'leaf_thickness' ~ 'mm',
    traits == 'ldmc' ~ 'g g-1',
    traits == 'bgb_agb' ~ 'g g-1',
    traits == 'aboveground_biomass' ~ 'g',
    traits == 'belowground_biomass' ~ 'g',
  ))

### save clean root trait field and rootpainter scan data
write_csv(long_roots, 'v_root_traits/v_PFCT7_clean_root_traits_2023.csv')
