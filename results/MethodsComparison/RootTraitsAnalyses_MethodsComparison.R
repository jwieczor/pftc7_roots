### Ploting trait densities by processing methodologies
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

rp <- read.csv('data/20240825_FullTableRP1.csv')
cut <- read.csv('data/20240825_FullTableCUT.csv')
gimp <- read.csv('data/20240825_FullTableGIMP1.csv')


### 2. Format data ----

# Select just trait variables by group

rpTraits <- rp %>%
  rename(RD = Average.Diameter.mm, BI = Branching.frequency.per.mm) %>%
  mutate(Group = 'RP') %>%
  select(Group, elevation, SRL, RTD, RDMC, RD, BI)
  
cutTraits <- cut %>%
  rename(RD = Average.Diameter.mm, BI = Branching.frequency.per.mm) %>%
  mutate(Group = 'CUT') %>%
  select(Group, elevation, SRL, RTD, RDMC, RD, BI)

gimpTraits <- gimp %>%
  rename(RD = Average.Diameter.mm, BI = Branching.frequency.per.mm) %>%
  mutate(Group = 'GIMP') %>%
  select(Group, elevation, SRL, RTD, RDMC, RD, BI)

# Combine all trait data

allTraits <- bind_rows(rpTraits, cutTraits, gimpTraits)

write.csv(allTraits, 'Analyses/MethodsComparison/20240925_AllTraits.csv')


### 3. Density plots per trait by method ----

RD <- ggplot(allTraits, aes(x = RD, color = Group, fill = Group)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "RD",
       y = NULL,
       color = "Group") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank())
plot(RD)

BI <- ggplot(allTraits, aes(x = BI, color = Group, fill = Group)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "BI",
       y = NULL,
       color = "Group") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
    theme(legend.title = element_blank())
plot(BI)

SRL <- ggplot(allTraits, aes(x = SRL, color = Group, fill = Group)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "SRL",
       y = NULL,
       color = "Group") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank())
plot(SRL)

RTD <- ggplot(allTraits, aes(x = RTD, color = Group, fill = Group)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "RTD",
       y = NULL,
       color = "Group") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
    theme(legend.title = element_blank())
plot(RTD)

RDMC <- ggplot(allTraits, aes(x = RDMC, color = Group, fill = Group)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "RDMC",
       y = NULL,
       color = "Group") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
    theme(legend.title = element_blank())
plot(RDMC)

# Combine to one figure

plotTraitsMethod <- ggarrange(RD, BI, SRL, RTD, RDMC,
          ncol = 2, nrow = 3, 
          align = "v",
          common.legend = TRUE, legend = "right")

plotTraitsMethod <- annotate_figure(plotTraitsMethod, 
                              fig.lab = NULL,  # No extra labels needed
                              fig.lab.face = "plain",
                              left = text_grob("Density", rot = 90, vjust = 1, size = 12)) +
                              theme(plot.background = element_rect(fill = "white", color = NA))

plot(plotTraitsMethod)

ggsave('Analyses/MethodsComparison/plotTraitsMethod.png', plotTraitsMethod, height = 7, width = 7)

### 3. Density plots comparing RD variables ----

# Select just trait variables by group

rpRD <- rp %>%
  rename(AverageRD = Average.Diameter.mm, MedianRD = Median.Diameter.mm, MaxRD = Maximum.Diameter.mm) %>%
  mutate(Group = 'RP') %>%
  select(Group, elevation, AverageRD, MedianRD, MaxRD)

cutRD <- cut %>%
  rename(AverageRD = Average.Diameter.mm, MedianRD = Median.Diameter.mm, MaxRD = Maximum.Diameter.mm) %>%
  mutate(Group = 'CUT') %>%
  select(Group, elevation, AverageRD, MedianRD, MaxRD)

gimpRD <- gimp %>%
  rename(AverageRD = Average.Diameter.mm, MedianRD = Median.Diameter.mm, MaxRD = Maximum.Diameter.mm) %>%
  mutate(Group = 'GIMP') %>%
  select(Group, elevation, AverageRD, MedianRD, MaxRD)

# Combine all RD

allRD <- bind_rows(rpRD, cutRD, gimpRD)

write.csv(allRD, 'Analyses/MethodsComparison/20240925_AllRD.csv')

# Density plots by RD

avRD <- ggplot(allRD, aes(x = AverageRD, color = Group, fill = Group)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "AverageRD",
       y = NULL,
       color = "Group") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank())
plot(avRD)

medRD <- ggplot(allRD, aes(x = MedianRD, color = Group, fill = Group)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "MedianRD",
       y = NULL,
       color = "Group") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank())
plot(medRD)

maxRD <- ggplot(allRD, aes(x = MaxRD, color = Group, fill = Group)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "MaximumRD",
       y = NULL,
       color = "Group") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank())
plot(maxRD)

# Combine to one figure

plotRDmethod <- ggarrange(avRD, medRD, maxRD,
                              ncol = 3, nrow = 1, 
                              align = "v",
                              common.legend = TRUE, legend = "right")

plotRDmethod <- annotate_figure(plotRDmethod, 
                                    fig.lab = NULL,  # No extra labels needed
                                    fig.lab.face = "plain",
                                left = text_grob("Density", rot = 90, vjust = 1, size = 12)) +
  theme(plot.background = element_rect(fill = "white", color = NA))

plot(plotRDmethod)

ggsave('Analyses/MethodsComparison/plotRDmethod.png', plotRDmethod, height = 3, width = 7)

