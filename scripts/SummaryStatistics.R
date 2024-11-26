
#Load table 
fT <- read.csv("D:/OneDrive - University of Miami/UMiami/PFTC7/DATA/pftc7_roots/data/roots/processed/BelowgroundTraitsDataset.csv",
               h=T)

#group ftFINAL by elevation and calculate mean and sd
sumsta_all <- fT %>% 
  summarise(across(RD:RDMC,
                   .fns = list(mean = mean, sd = sd),
                   na.rm = TRUE)) %>% 
  pivot_longer(everything(), names_sep='_', names_to=c('variable', '.value'))
sumsta_all



#group ftFINAL by elevation and calculate mean and sd
sumsta_elevation <- fT %>% 
  group_by(elevation) %>% 
  summarise(across(c("RD","BI", "SRL", "RTD", "RDMC"),
                   .fns = list(mean = mean, sd = sd),
                   na.rm = TRUE)) %>% 
  pivot_longer(!elevation, names_sep='_', names_to=c('variable', '.value')) %>% 
  arrange(variable,elevation)
sumsta_elevation
