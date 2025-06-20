# 1) Clean raw root trait field data, join with rootpainter scan and derive root traits; 2) Clean and join all root scan data
# created 18/10/2024 by Lina
# modified 17/06/2025 by Joe

### 1. Set up ----
library(tidyverse)
library(plotly)
library(osfr)

### retrieve raw data files from the OSF project page
osf_retrieve_node('hk2cy') %>%
  osf_ls_files(path = 'raw_data/05_raw_root_traits/') %>%
  filter(!str_ends(name, '.zip')) %>% # don't download the raw scan files
  osf_download(path = 'raw_data/05_raw_root_traits/', conflicts = 'overwrite')

### 2. Format raw and save clean root trait (field and scan) data ----
### load in root trait field data
roots <- read_csv('raw_data/05_raw_root_traits/PFTC7_SA_raw_root_trait_field_data_2023.csv')
names(roots)

### load in scans
raw_scans <- read_csv('raw_data/05_raw_root_traits/PFTC7_SA_raw_scans_2023.csv')
gimp_scans <- read_csv('raw_data/05_raw_root_traits/PFTC7_SA_raw_gimp_scans_2023.csv')
rootpainter_scans <- read_csv('raw_data/05_raw_root_traits/PFTC7_SA_raw_rootpainter_scans_2023.csv')

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
  mutate(agb_bgb = AGB/BGB,
         srl_m_g = (Total.Root.Length.mm*0.001)/root_dry_mass,
         rtd_g_cm3 = root_dry_mass/(Volume.mm3*0.001),
         rdmc_mg_g = (root_dry_mass*1000)/root_wet_mass_g,
         sla_cm2_g = (leaf_area/leaf_dry_mass),
         ldmc_g_g = (leaf_dry_mass/leaf_wet_mass_g)) %>% 
  rowwise() %>% 
  mutate(lt_mean_mm = mean(c_across(starts_with("leaf_thick")), na.rm = F), .after = srl_m_g) %>%
  mutate(aspect = case_when(
    aspect == 'W' ~ 'west',
    aspect == 'NW' ~ 'northwest'
  ), 
  fire = ifelse(fire == "",0,1),
  sp = case_when(
    sp == 'EC' ~ 'Eragrostis capensis',
    sp == 'HF' ~ 'Harpochloa falx',
    sp == 'HPA' ~ 'Helichrysum pallidum',
    sp == 'SG' ~ 'Senecio glaberrimus',
    sp == 'TT' ~ 'Themeda triandra'
  ))

### Select relevant variables and rename variables
clean_roots <- derived_roots %>%
  dplyr::select(id = File.Name, date, elevation_m_asl = elevation, site_id = siteID, aspect, species = sp, plant_id = plantID, fire, 
                root_depth_cm = root_depth, no_root_scan, total_root_wet_mass_g = root_wet_mass_g, total_root_dry_mass_g = root_dry_mass, total_root_length_mm = Total.Root.Length.mm, total_root_volume_mm3 = Volume.mm3, rd_mm = Average.Diameter.mm, bi_branches_mm = Branching.frequency.per.mm, srl_m_g, rtd_g_cm3, rdmc_mg_g, no_tuber_scan, total_tuber_wet_mass_g = tuber_wet_mass_g, total_tuber_dry_mass_g = tuber_dry_mass,
                reproductive_height_cm = leafID, veg_height_cm = height, no_leaf, total_leaf_wet_mass_g = leaf_wet_mass_g, total_leaf_dry_mass_g = leaf_dry_mass, leaf_thick_1_mm, leaf_thick_2_mm, leaf_thick_3_mm, total_leaf_area_cm2 = leaf_area, sla_cm2_g, lt_mean_mm, ldmc_g_g,  agb_bgb, aboveground_biomass_g = AGB, belowground_biomass_g = BGB)

### known issues with RTD - plot interactively
ggplotly(ggplot(clean_roots, aes(total_root_dry_mass_g, total_root_volume_mm3)) +
           geom_point(aes(col = id, size = rtd_g_cm3)))
### root dry mass and volume for FEK5954 are not outside of typical range; leave value in but filter out for summaries and figures later.

### pivot long
long_roots <- clean_roots %>%
  pivot_longer(cols = root_depth_cm:belowground_biomass_g, names_to = 'traits', values_to = 'value')

### save clean root trait field and rootpainter scan data
write_csv(long_roots, '05_root_traits/PFCT7_SA_clean_root_traits_2023.csv')

### 3. Format scan data ----
# select root mass measurements
roots_select <- clean_roots %>%
  dplyr::select(id, total_root_dry_mass_g, total_root_wet_mass_g)

# select id and key root scan metrics
# raw
raw_scans_select <- raw_scans %>%
  dplyr::select(id = File.Name, total_root_length_mm = Total.Root.Length.mm, total_root_volume_mm3 = Volume.mm3, rd_mm = Average.Diameter.mm, bi_branches_mm = Branching.frequency.per.mm)

# gimp
gimp_scans_select <- gimp_scans %>%
  dplyr::select(id = File.Name, total_root_length_mm = Total.Root.Length.mm, total_root_volume_mm3 = Volume.mm3, rd_mm = Average.Diameter.mm, bi_branches_mm = Branching.frequency.per.mm)

# rootpainter
rootpainter_scans_select <- rootpainter_scans %>%
  dplyr::select(id = File.Name, total_root_length_mm = Total.Root.Length.mm, total_root_volume_mm3 = Volume.mm3, rd_mm = Average.Diameter.mm, bi_branches_mm = Branching.frequency.per.mm)

# combine root scans together
all_scans <- bind_rows(raw_scans_select, gimp_scans_select, rootpainter_scans_select, .id = 'source') %>%
  mutate(source = case_when(
    source == 1 ~ 'raw',
    source == 2 ~ 'gimp',
    source == 3 ~ 'rootpainter'
  ))

# join together with key root trait metrics and calculate derived traits (SRL, RTD, RDMC)
methods_comparison <- all_scans %>%
  left_join(roots_select, by = 'id') %>% 
  mutate(srl_m_g = (total_root_length_mm*0.001)/total_root_dry_mass_g,
  rtd_g_cm3 = total_root_dry_mass_g/(total_root_volume_mm3*0.001),
  rdmc_mg_g = (total_root_dry_mass_g*1000)/total_root_wet_mass_g) %>%
  dplyr::select(source:bi_branches_mm, srl_m_g:rdmc_mg_g)

# save root scan comparison dataset for technical validation
write_csv(methods_comparison, '05_root_traits/PFCT7_SA_clean_scan_methods_comparison_2023.csv')
