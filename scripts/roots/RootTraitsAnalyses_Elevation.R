### Ploting trait densities by elevation
# Anya Courtenay
# 25/09/2024
# Updated: 26/11/2024
# Updated JW 11/02/2025

### 1. Set up ----
library(tidyverse)
library(ggpubr)
library(ggridges)
library(viridisLite)
library(RColorBrewer)


### 2. Format data ----
# Load in root trait data
root_traits <- read.csv('data/roots/processed/BelowgroundTraitsDataset.csv') %>% 
  pivot_wider(names_from = traits, values_from = value) %>%
  mutate(elevation_m_asl = as.factor(elevation_m_asl))

scl= 10
alp3 = 0.7
cols2<- c("#6F00A8", "#A72197",  "#DD5E66", "#EF7F4F") # 2000m: #FDCB26
lwd = 0.5

### 3. Density plots per trait by elevation (RP scans) ----
rd <- ggplot(root_traits, aes(x = RD, y = elevation_m_asl, fill = elevation_m_asl)) +
  geom_density_ridges(alpha= alp3, scale = scl, linewidth=lwd)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400, 2200))+
  scale_y_discrete(expand = expansion(mult = c(0.1, 3.1)))+
  labs(x = "RD (mm)", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

bi <- ggplot(root_traits, aes(x = BI, y = elevation_m_asl, fill = elevation_m_asl)) +
  geom_density_ridges(alpha= alp3, scale = scl, linewidth=lwd)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400, 2200))+
  scale_y_discrete(expand = expansion(mult = c(0.1, 3.4)))+
  labs(x = "BI (branches/mm)", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

srl <- ggplot(root_traits, aes(x = SRL, y = elevation_m_asl, fill = elevation_m_asl)) +
  geom_density_ridges(alpha= alp3, scale = scl, linewidth=lwd)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400, 2200))+
  scale_y_discrete(expand = expansion(mult = c(0.1, 2.6)))+
  labs(x = "SRL (m/g)", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

rtd <- root_traits %>%
  filter(RTD <= 0.5) %>% # Filter out high RTD values to remove outliers; removes 2 values
ggplot(aes(x = RTD, y = elevation_m_asl, fill = elevation_m_asl)) +
  geom_density_ridges(alpha= alp3, scale = scl, linewidth=lwd)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400, 2200))+
  scale_y_discrete(expand = expansion(mult = c(0.1, 3.1)))+
  labs(x = expression(paste("RTD (g/",cm^3,")")), color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

rdmc <- root_traits %>%
  filter(RDMC <= 1000) %>% # Filter out high RDMC values to remove outliers; removes 1 values
ggplot(aes(x = RDMC, y = elevation_m_asl, fill = elevation_m_asl)) +
  geom_density_ridges(alpha= alp3, scale = scl, linewidth=lwd)+
  scale_fill_manual(values = cols2, breaks = c(2800,  2600, 2400, 2200))+
  scale_y_discrete(expand = expansion(mult = c(0.1, 3.4)))+
  labs(x = "RDMC (mg/g)", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

# Combine into single panel 
root_traits_elev <- ggarrange(rd + rremove("y.text"), bi + rremove("y.text"), srl + rremove("y.text"), rtd + rremove("y.text"), rdmc + rremove("y.text"), common.legend = TRUE,legend = "right", align = "v", label.y = 1) %>%
  annotate_figure(left = text_grob("Density",rot = 90))
root_traits_elev

# 4. Save figure
ggsave("root_traits_elev.png", root_traits_elev, path= "results/roots/Elevation", height = 3.333, width = 7 , bg= "white", units= "in")
