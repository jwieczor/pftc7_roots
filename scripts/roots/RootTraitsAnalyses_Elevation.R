### Ploting trait densities by elevation
# Anya Courtenay
# 25/09/2024

## TO EDIT: add units, remove grids, higher alpha?
## TO CHECK: outliers

### 1. Set up ----

library(dplyr)
library(ggplot2)
library(ggpubr)
library(viridis)

setwd('H:/My Drive')

rp <- read.csv('data/20240825_FullTableRP1.csv') # Make sure final clean dataset

field <- read.csv('data/23-12-23_RootData.csv')


### 2. Format data ----

# Select just trait variables by group

traits <- rp %>%
  rename(RD = Average.Diameter.mm, BI = Branching.frequency.per.mm) %>%
  mutate(Group = 'RP') %>%
  select(File.Name, sp, Group, elevation, SRL, RTD, RDMC, RD, BI)

traits$elevation <- factor(traits$elevation)
traits$elevation <- paste0(traits$elevation, " m")

### 3. Density plots per trait by elevation (RP scans) ----

RD <- ggplot(traits, aes(x = RD, color = elevation, fill = elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "Root_Diameter_mm",
       y = NULL,
       color = "elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(RD)


BI <- ggplot(traits, aes(x = BI, color = elevation, fill = elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "Branching_Intensity_mm-1",
       y = NULL,
       color = "elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
    theme(legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
plot(BI)

SRL <- ggplot(traits, aes(x = SRL, color = elevation, fill = elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "Specific_Root_Length_m_g-1",
       y = NULL,
       color = "elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(SRL)

RTD <- ggplot(traits, aes(x = RTD, color = elevation, fill = elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "Root_Tissue_Density_g_cm-3",
       y = NULL,
       color = "elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(RTD)

RDMC <- ggplot(traits, aes(x = RDMC, color = elevation, fill = elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "Root_Dry_Matter_Content_mg_g-1",
       y = NULL,
       color = "elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(RDMC)

# Combine to one figure

plotElevation <- ggarrange(RD, BI, SRL, RTD, RDMC,
          ncol = 3, nrow = 2, 
          align = "v",
          common.legend = TRUE, legend = "right")

plotElevation <- annotate_figure(plotElevation, 
                              # fig.lab = 'All Herbaceous',  # No extra labels needed
                              fig.lab.face = "plain",
                              left = text_grob("Density", rot = 90, vjust = 1, size = 12)) +
                              theme(plot.background = element_rect(fill = "white", color = NA))

plot(plotElevation)

ggsave('Analyses/Elevation/plotElevation.png', plotElevation, height = 6, width = 10)


### 3. Density plots per trait by elevation by functional group ----

# Combine species ID
field <- field %>%
  rename(File.Name = barcode)

traits_sp <- traits %>%
  left_join(field %>% select(File.Name, sp), by = 'File.Name')

# Separate data into functional group
grasses <- c('SG', 'HPA')
forbs <- c('TT', 'EC', 'HF')

traits_grasses <- traits %>%
  filter(sp %in% grasses)

traits_forbs <- traits %>%
  filter(sp %in% forbs)

### Grasses ----

RDg <- ggplot(traits_grasses, aes(x = RD, color = elevation, fill = elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "Root_Diameter_mm",
       y = NULL,
       color = "elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(RDg)


BIg <- ggplot(traits_grasses, aes(x = BI, color = elevation, fill = elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "Branching_Intensity_mm-1",
       y = NULL,
       color = "elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(BIg)

SRLg <- ggplot(traits_grasses, aes(x = SRL, color = elevation, fill = elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "Specific_Root_Length_m_g-1",
       y = NULL,
       color = "elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(SRLg)

RTDg <- ggplot(traits_grasses, aes(x = RTD, color = elevation, fill = elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "Root_Tissue_Density_g_cm-3",
       y = NULL,
       color = "elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(RTDg)

RDMCg <- ggplot(traits_grasses, aes(x = RDMC, color = elevation, fill = elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "Root_Dry_Matter_Content_mg_g-1",
       y = NULL,
       color = "elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(RDMCg)

# Combine to one figure

plotElevation_grasses <- ggarrange(RDg, BIg, SRLg, RTDg, RDMCg,
                           ncol = 2, nrow = 2, 
                           align = "v",
                           common.legend = TRUE, legend = "right")

plotElevation_grasses <- annotate_figure(plotElevation_grasses, 
                                 fig.lab = 'Grasses',  # No extra labels needed
                                 fig.lab.face = "plain",
                                 left = text_grob("Density", rot = 90, vjust = 1, size = 12)) +
  theme(plot.background = element_rect(fill = "white", color = NA))

plot(plotElevation_grasses)

ggsave('Analyses/Elevation/plotElevation_grasses.png', plotElevation_grasses, height = 7, width = 7)



### Forbs ----

RDf <- ggplot(traits_forbs, aes(x = RD, color = elevation, fill = elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "Root_Diameter_mm",
       y = NULL,
       color = "elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(RDf)


BIf <- ggplot(traits_forbs, aes(x = BI, color = elevation, fill = elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "Branching_Intensity_mm-1",
       y = NULL,
       color = "elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(BIf)

SRLf <- ggplot(traits_forbs, aes(x = SRL, color = elevation, fill = elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "Specific_Root_Length_m_g-1",
       y = NULL,
       color = "elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(SRLf)

RTDf <- ggplot(traits_forbs, aes(x = RTD, color = elevation, fill = elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "Root_Tissue_Density_g_cm-3",
       y = NULL,
       color = "elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(RTDf)

RDMCf <- ggplot(traits_forbs, aes(x = RDMC, color = elevation, fill = elevation)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "Root_Dry_Matter_Content_mg_g-1",
       y = NULL,
       color = "elevation") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(RDMCf)

# Combine to one figure

plotElevation_forbs <- ggarrange(RDf, BIf, SRLf, RTDf, RDMCf,
                           ncol = 2, nrow = 2, 
                           align = "v",
                           common.legend = TRUE, legend = "right")

plotElevation_forbs <- annotate_figure(plotElevation_forbs, 
                                 fig.lab = 'Forbs',  # No extra labels needed
                                 fig.lab.face = "plain",
                                 left = text_grob("Density", rot = 90, vjust = 1, size = 12)) +
  theme(plot.background = element_rect(fill = "white", color = NA))

plot(plotElevation_forbs)

ggsave('Analyses/Elevation/plotElevation_forbs.png', plotElevation_forbs, height = 7, width = 7)


### EDIT GRASSES AND FORBS SO AXES THE SAME AND IMPROVE POSITION OF TITLE


plotElevation_all <- ggarrange(
                     ggarrange(RD, BI, SRL, RTD, ncol = 1, labels = 'All', legend = F),
                     ggarrange(RDg, BIg, SRLg, RTDg, ncol = 1, labels = 'Grasses', legend = F),
                     ggarrange(RDf, BIf, SRLf, RTDf, ncol = 1, labels = 'Forbs', legend = F),
                     ncol = 3, common.legend = TRUE, legend = "right")
                               
plot(plotElevation_all)


plotElevation_all <- annotate_figure(plotElevation_all, 
                                       #fig.lab = 'Forbs',  # No extra labels needed
                                       fig.lab.face = "plain",
                                       left = text_grob("Density", rot = 90, vjust = 1, size = 12)) +
  theme(plot.background = element_rect(fill = "white", color = NA))

plot(plotElevation_all)




ggsave('Analyses/Elevation/plotElevation_all.png', plotElevation_all, height = 7, width = 5)

