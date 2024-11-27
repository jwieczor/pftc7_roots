### Ploting trait densities by elevation
# Anya Courtenay
# 25/09/2024
# Updated: 26/11/2024

### 1. Set up ----

install.packages('dplyr')
install.packages('ggplot2')
install.packages('ggpubr')
install.packages('viridis')
install.packages('patchwork')
library(dplyr)
library(ggplot2)
library(ggpubr)
library(viridis)
library(patchwork)

rp <- read.csv('data/roots/processed/BelowgroundTraitsDataset.csv') # Make sure final clean dataset

### 2. Format data ----

# Select just trait variables with species and elevation
traits <- rp %>%
  select(ID, sp, elevation, SRL, RTD, RDMC, RD, BI)

# Update elevation column for plotting 
traits$elevation <- factor(traits$elevation)
traits$elevation <- paste0(traits$elevation, " m")
traits <- traits %>%
  rename(Elevation = elevation)

# Filter out RTD >0.5 to remove outliers for display purposes
traits <- traits %>%
  filter(RTD <= 0.5) # removes 2 values

### 3. Density plots per trait by elevation (RP scans) ----

ggplot(traits, aes(x = RD, color = Elevation, fill = Elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "RD_mm",
       y = NULL,
       color = "Elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme_bw() +
  
ggplot(traits, aes(x = BI, color = Elevation, fill = Elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "BI_mm-1",
       y = NULL,
       color = "Elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme_bw() +
  
ggplot(traits, aes(x = SRL, color = Elevation, fill = Elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "SRL_m_g-1",
       y = NULL,
       color = "Elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme_bw() +

ggplot(traits, aes(x = RTD, color = Elevation, fill = Elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "RTD_g_cm-3",
       y = NULL,
       color = "Elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme_bw() +
  
ggplot(traits, aes(x = RDMC, color = Elevation, fill = Elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "RDMC_mg_g-1",
       y = NULL,
       color = "Elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme_bw() +
  
# Edit combined plots
plot_annotation(tag_levels = 'a', tag_suffix = ')') +
plot_layout(guides = "collect")

# 4. Save figure
ggsave('results/roots/Elevation/plotElevation.png',
       width = 8.36, height = 6.15, dpi = 320)
