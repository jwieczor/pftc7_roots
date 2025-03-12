### Summary values extracted from clean root trait data

### 1. Set up ----
library(tidyverse)

### load in root trait data
roots <- read_csv('data/roots/clean/clean_root_trait_data.csv')

### FEK5954 is an outlier, convert to NA for summaries
roots[roots$id == "FEK5954", "RTD"] <- NA

# summarise root traits all sites
roots_summary_all <- roots %>%
  filter(traits %in% c("RD", "BI", "SRL", "RTD", "RDMC")) %>%
  group_by(traits) %>%
  summarise_at('value', list(mean = mean, sd = sd), na.rm = T)

# summarise root traits at each elevation
roots_summary_elev <- roots %>%
  filter(traits %in% c("RD", "BI", "SRL", "RTD", "RDMC")) %>%
  group_by(traits, elevation_m_asl) %>%
  summarise_at('value', list(mean = mean, sd = sd), na.rm = T)
