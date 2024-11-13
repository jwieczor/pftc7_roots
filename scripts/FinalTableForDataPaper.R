## Load packages
pkgs <- c("dplyr", "tidyverse", "stringr", "tidyr")
lapply(pkgs, library, character.only = TRUE)
remove(pkgs)


# Load Whole Roots Table 
ft <- read.csv("D:/OneDrive - University of Miami/UMiami/PFTC7/DATA/pftc7_roots/data/roots/processed/Root_and_leaf_traits_RP_20241029.csv", h = T)

#Reorganize Table Root Traits, Leaf Traits, and Biomass Tratis. Change names to match paper style. Calculate LT
ftFINAL <-ft %>% 
  select(File.Name, date, elevation, siteID, aspect, sp, plantID, fire, root_depth, no_root_scan, root_wet_mass_g, 
         root_dry_mass, Total.Root.Length.mm, Volume.mm3, Average.Diameter.mm, Branching.frequency.per.mm, 
         SRL, RTD, RDMC, no_tuber_scan, tuber_wet_mass_g, tuber_dry_mass, leafID, height, no_leaf, 
         leaf_wet_mass_g, leaf_dry_mass, leaf_thick_1_mm, leaf_thick_2_mm, leaf_thick_3_mm,
         leaf_area, SLA, LDMC, BGB, AGB, AGB.BGB) %>% 
  rename(ID = File.Name) %>% 
  rename(reproductive_height_cm = leafID) %>% 
  rename(vegetative_height_cm = height) %>% 
  rename(root_depth_cm = root_depth) %>% 
  rename(root_dry_mass_g = root_dry_mass) %>% 
  rename(total_root_length_mm = Total.Root.Length.mm) %>% 
  rename(root_volume_mm3 = Volume.mm3) %>% 
  rename(RD = Average.Diameter.mm) %>% 
  rename(BI = Branching.frequency.per.mm) %>% 
  rename(tuber_dry_mass_g = tuber_dry_mass) %>% 
  rename(leaf_dry_mass_g = leaf_dry_mass) %>% 
  rename(leaf_area_mm2 = leaf_area) %>% 
  rename(belowground_biomass_g = BGB) %>% 
  rename(aboveground_biomass_g = AGB) %>% 
  rename("ABG:BGB" = AGB.BGB) %>%
  mutate(fire = ifelse(fire == "",0,1)) %>% 
  rowwise() %>% 
  mutate(LT = mean(c_across(starts_with("leaf_thick")), na.rm = F), .after = SLA)
  

ftFINAL[ftFINAL$ID == "FET3274", "root_dry_mass_g"] <- 0.01004

ftFINAL2<- ftFINAL %>% 
  select(-RDMC) %>% 
  mutate(RDMC = (root_dry_mass_g*1000)/root_wet_mass_g, .after = RTD)

write.csv(ftFINAL2, "D:/OneDrive - University of Miami/UMiami/PFTC7/DATA/pftc7_roots/data/roots/processed/BelowgroundTraitsDataset.csv",
          row.names=FALSE)



