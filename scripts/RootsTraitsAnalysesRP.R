### Root Analysis on GIMP Outputs###

## Load packages
pkgs <- c("dplyr", "tidyverse", "stringr", "tidyr")
lapply(pkgs, library, character.only = TRUE)
remove(pkgs)

# Load Whole Roots Table 
field.data <- read.csv("H:/My Drive/data/RootWholeTable.csv") #Check the folder path on your computer
head(field.data)
colnames(field.data)[1] <- "File.Name" #Rename column barcode to File.Name

# Load Features Table APC with RhizoVision output for each invididual
featuresRP <- read.csv("H:/My Drive/root_scans/RhizoVision_outputAPC/RV_on_RP_outputs/featuresRP.csv") #Check the folder path on your computer
head(featuresRP)

featuresRP1 <- featuresRP %>% 
  mutate(File.Name = str_extract(File.Name, ".*(?=\\.)")) #Remove .png part of File.Name values

#Join the two tables Whole Roots Table & Features Table using the File.Name column
full.tableRP <- full_join(field.data, featuresRP1, "File.Name")


#Calculate SRL, RTD, and RDMC
full.tableRP1 <- full.tableRP  %>% 
  mutate(SRL = (Total.Root.Length.mm*0.001)/root_dry_mass) %>% 
  mutate(RTD = root_dry_mass/(Volume.mm3*0.001)) %>% 
  mutate(RDMC = (root_dry_mass*0.001)/root_wet_mass_g) %>% 
  drop_na(Region.of.Interest)

##Export table with calculated functional traits
write.csv(full.tableRP1, "H:/My Drive/data/20240825_FullTableRP1.csv")
write.csv(full.tableRP1, "D:/LinaAragon/Downloads/20240825_FullTableRP1.csv")
