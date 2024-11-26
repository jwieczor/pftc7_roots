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
library(patchwork)

#### Load in all scan data across different methods
all_data <- read.csv('data/roots/processed/TechnicalValidationRootScannersData.csv')

#### filter to only include matched scans
glimpse(all_data)
IDs <- all_data %>%
  filter(source == 'GIMP') %>%
  select(ID)

compare <- all_data %>% filter(ID %in% IDs$ID)

###
compare %>%
  select(ID, source, Median.Diameter.mm) -> test

### 2. Format data ----

# Select just trait variables by source
compare_plot_data <- compare %>%
        select(ID, source, 
         total_root_length_mm = Total.Root.Length.mm,
         median_diameter_mm = Median.Diameter.mm,
         root_volume_mm3 = Volume.mm3,
         BI)

### 3. Density plots per trait by method ----

ggplot(compare_plot_data, aes(x = total_root_length_mm, color = source, fill = source)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "total_root_length_mm",
       y = NULL,
       color = "source") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank()) +

ggplot(compare_plot_data, aes(x = median_diameter_mm, color = source, fill = source)) +
  geom_density(alpha = 0.5, adjust = 5) +
  theme_bw() +
  labs(x = "median_diameter_mm",
       y = NULL,
       color = "source") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
    theme(legend.title = element_blank()) +

ggplot(compare_plot_data, aes(x = root_volume_mm3, color = source, fill = source)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "root_volume_mm3",
       y = NULL,
       color = "source") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank()) +

ggplot(compare_plot_data, aes(x = BI, color = source, fill = source)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "BI_mm-1",
       y = NULL,
       color = "source") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
    theme(legend.title = element_blank()) +
  plot_layout(guides = 'collect') &
  plot_annotation(tag_levels = 'a', tag_suffix = ')')

ggsave('results/MethodsComparison/plotTraitsMethod_MeasuredVars.png', height = 9.38, width = 11.44, dpi = 320)