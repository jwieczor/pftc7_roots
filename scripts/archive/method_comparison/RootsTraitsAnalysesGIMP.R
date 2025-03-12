### Root Analysis on GIMP Outputs###

## Load packages
pkgs <- c("dplyr", "tidyverse", "stringr", "tidyr")
lapply(pkgs, library, character.only = TRUE)
remove(features)

# Load Whole Roots Table 
# field.data <- read.csv("G:/My Drive/data/RootWholeTable.csv")
field.data <- read.csv("H:/My Drive/data/archive/RootWholeTable.csv")
head(field.data)
colnames(field.data)[1] <- "File.Name" #Rename column barcode to File.Name

# Load Features Table APC with RhizoVision output for each invididual
featuresGIMP <- read.csv("H:/My Drive/root_scans/RhizoVision_outputAPC/RV_on_GIMP_outputs/featuresGIMP.csv")
head(featuresGIMP)

featuresGIMP1 <- featuresGIMP %>% 
  mutate(File.Name = str_extract(File.Name, ".*(?=\\.)")) #Remove .png part of File.Name values

#Join the two tables Whole Roots Table & Features Table using the File.Name column
full.tableGIMP <- full_join(field.data, featuresGIMP1, "File.Name")

full.tableGIMP[full.tableGIMP$File.Name == "FET3274", "root_dry_mass"] <- 0.01004
#Calculate SRL, RTD, and RDMC
full.tableGIMP1 <- full.tableGIMP  %>% 
  mutate(SRL = (Total.Root.Length.mm*0.001)/root_dry_mass) %>% 
  mutate(RTD = root_dry_mass/(Volume.mm3*0.001)) %>% 
  mutate(RDMC = (root_dry_mass*1000)/root_wet_mass_g) %>% 
  drop_na(Region.of.Interest)



##Export table with calculated functional traits
write.csv(full.tableGIMP1, "D:/OneDrive - University of Miami/UMiami/PFTC7/DATA/pftc7_roots/data/roots/processed/FullTableGIMP_20241113.csv")
write.csv(full.tableGIMP1, "H:/My Drive/data/archive/FullTableGIMP_20241113.csv")

