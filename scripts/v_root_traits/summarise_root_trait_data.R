### Summary values extracted from clean root trait data
# modified 01/07/2025 by Joe

### 1. Set up ----
library(tidyverse)

### load in root trait data
roots <- read_csv('v_root_traits/v_PFCT7_clean_root_traits_2023.csv')

### FEK5954 is an outlier, convert to NA for summaries
roots[roots$id == "FEK5954", "rtd_g_cm3"] <- NA

# summarise root traits all sites
roots_summary_all <- roots %>%
  filter(traits %in% c("rd", "bi", "srl", "rtd", "rdmc")) %>%
  group_by(traits) %>%
  summarise_at('value', list(mean = mean, sd = sd), na.rm = T)

# summarise root traits at each elevation
roots_summary_elev <- roots %>%
  filter(traits %in% c("rd", "bi", "srl", "rtd", "rdmc")) %>%
  group_by(traits, elevation_m_asl) %>%
  summarise_at('value', list(mean = mean, sd = sd), na.rm = T)
