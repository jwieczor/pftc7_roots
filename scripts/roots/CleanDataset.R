#### load libraries
library(dplyr)
library(tidyverse)
library(stringr)
library(tidyr)

#### load in root data
raw.root.table <- read.csv('data/roots/processed/RawRootsDataset.csv', h = T)
head(raw.root.table)

### Change outlier values
raw.root.table[raw.root.table$File.Name == "FET3274", "root_dry_mass"] <- 0.01004

### Calculate derived Root and Leaf Traits
raw.root.table1 <- raw.root.table  %>% 
  mutate(SRL = (Total.Root.Length.mm*0.001)/root_dry_mass) %>% 
  mutate(RTD = root_dry_mass/(Volume.mm3*0.001)) %>% 
  mutate(RDMC = (root_dry_mass*1000)/root_wet_mass_g) %>% 
  mutate(SLA = (leaf_area/leaf_dry_mass)) %>% 
  mutate(LDMC = (leaf_dry_mass*1000/leaf_wet_mass_g)) %>% 
  rowwise() %>% 
  mutate(LT = mean(c_across(starts_with("leaf_thick")), na.rm = F), .after = SLA)
  
### Select relevant variables and create clean data set
clean.root.table <- raw.root.table1 %>% 
  select(File.Name, date, elevation, siteID, aspect, sp, plantID, fire, root_depth, no_root_scan, root_wet_mass_g, 
         root_dry_mass, Total.Root.Length.mm, Volume.mm3, Average.Diameter.mm, Branching.frequency.per.mm, 
         SRL, RTD, RDMC, no_tuber_scan, tuber_wet_mass_g, tuber_dry_mass, leafID, height, no_leaf, 
         leaf_wet_mass_g, leaf_dry_mass, leaf_thick_1_mm, leaf_thick_2_mm, leaf_thick_3_mm,
         leaf_area, SLA, LT, LDMC, BGB, AGB, AGB.BGB) %>% 
  rename(ID = File.Name) %>% 
  rename(reproductive_height_cm = leafID) %>% 
  rename(vegetative_height_cm = height) %>% 
  rename(root_depth_cm = root_depth) %>% 
  rename(total_root_wet_mass_g = root_wet_mass_g) %>% 
  rename(total_root_dry_mass_g = root_dry_mass) %>% 
  rename(total_root_length_mm = Total.Root.Length.mm) %>% 
  rename(total_root_volume_mm3 = Volume.mm3) %>% 
  rename(RD = Average.Diameter.mm) %>% 
  rename(BI = Branching.frequency.per.mm) %>% 
  rename(total_tuber_wet_mass_g = tuber_wet_mass_g) %>% 
  rename(total_tuber_dry_mass_g = tuber_dry_mass) %>% 
  rename(total_leaf_wet_mass_g = leaf_wet_mass_g) %>% 
  rename(total_leaf_dry_mass_g = leaf_dry_mass) %>% 
  rename(total_leaf_area_cm2 = leaf_area) %>% 
  rename(belowground_biomass_g = BGB) %>% 
  rename(aboveground_biomass_g = AGB) %>% 
  rename("ABG_BGB" = AGB.BGB) %>%
  mutate(fire = ifelse(fire == "",0,1))


write.csv(clean.root.table,'data/roots/processed/BelowgroundTraitsDataset.csv', row.names = FALSE)
