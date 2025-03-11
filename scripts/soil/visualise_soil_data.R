### Visualise soil texture and nutrients data

### 1. Set up ----
library(tidyverse)
library(plotrix)
library(corrplot)
library(patchwork)

# load in soil data
soil <- read_csv('data/soil/clean/clean_soil.csv')

### 2. Format and glimpse data ----
# pivot wider
soil_wide <- soil %>%
  pivot_wider(names_from = 'variable', values_from = 'value')
glimpse(soil_wide)

# summary vals for each elevation band
soil_sum <- soil_wide %>%
              group_by(elevation_m_asl) %>%
              summarise_at(vars(tc_perc:silt_perc),
                            list(mean = mean, 
                                 sd = sd,
                                 se = std.error), 
                           na.rm = T)

### 3. Correlation plots for each soil variable ----
# correlation matrix of soils across sites
# png('results/soil/soil_corrplot.png',
#     width = 7, height = 4.5,
#     units = 'in', res = 320)
corrplot(cor(soil_sum[,2:9]), 
         method = 'shade',
         type = 'lower',
         diag = F,
         order = 'AOE',
         tl.col = 'black')
# dev.off()

### 4. Line plots for each soil variable across elevation ----
ggplot(soil_sum, aes(x = elevation_m_asl, y = tp_mg_kg_mean)) +
  geom_smooth(method = 'loess', col = 'black') +
  geom_point(size = 3, col = 'red3') +
  geom_linerange(aes(ymin = tp_mg_kg_mean - tp_mg_kg_se, ymax = tp_mg_kg_mean + tp_mg_kg_se), col = 'red3') +
  labs(x = 'Elevation (m)', y = 'Total P (mg/kg)') +
  theme_minimal() +
  
ggplot(soil_sum,aes(x = elevation_m_asl, y = tn_perc_mean)) +
  geom_smooth(method = 'loess', col = 'black') +
  geom_point(size = 3, col = 'skyblue') +
  geom_linerange(aes(ymin = tn_perc_mean - tn_perc_se, ymax = tn_perc_mean + tn_perc_se), col = 'skyblue') +

  labs(x = 'Elevation (m)', y = 'Total N (%)') +  theme_minimal() +
  
ggplot(soil_sum,aes(x = elevation_m_asl, y = tc_perc_mean)) +
  geom_smooth(method = 'loess', col = 'black') +
  geom_point(size = 3, col = 'lightgreen') +
  geom_linerange(aes(ymin = tc_perc_mean - tc_perc_se, ymax = tc_perc_mean + tc_perc_se), col = 'lightgreen') +

  labs(x = 'Elevation (m)', y = 'Total C (%)') +  theme_minimal() +
  
ggplot(soil_sum, aes(x = elevation_m_asl, y = cec_cmol_kg_mean)) +
  geom_smooth(method = 'loess', col = 'black') +
  geom_point(size = 3, col = 'darkgreen') +
  geom_linerange(aes(ymin = cec_cmol_kg_mean - cec_cmol_kg_se, ymax = cec_cmol_kg_mean + cec_cmol_kg_se), col = 'darkgreen') +

  labs(x = 'Elevation (m)', y = 'CEC (cmol/kg)') +
  theme_minimal() +

ggplot(soil_sum, aes(x = elevation_m_asl, y = ph_mean)) +
  geom_smooth(method = 'loess', col = 'black') +
  geom_point(size = 3, col = 'purple3') +
  geom_linerange(aes(ymin = ph_mean - ph_se, ymax = ph_mean + ph_se), col = 'purple3') +

  labs(x = 'Elevation (m)', y = 'pH') + theme_minimal() +
  

ggplot(soil_sum,aes(x = elevation_m_asl, y = sand_perc_mean)) +
  geom_smooth(method = 'loess', col = 'black') +
  geom_point(size = 3, col = 'gold') +
  geom_linerange(aes(ymin = sand_perc_mean - sand_perc_se, ymax = sand_perc_mean + sand_perc_se), col = 'gold') +

  labs(x = 'Elevation (m)', y = 'Sand (%)') +  theme_minimal() +

  ggplot(soil_sum,aes(x = elevation_m_asl, y = clay_perc_mean)) +
  geom_smooth(method = 'loess', col = 'black') +
  geom_point(size = 3, col = 'brown1') +
  geom_linerange(aes(ymin = clay_perc_mean - clay_perc_se, ymax = clay_perc_mean + clay_perc_se), col = 'brown1') +

  labs(x = 'Elevation (m)', y = 'Clay (%)') +  theme_minimal() +
  
ggplot(soil_sum,aes(x = elevation_m_asl, y = silt_perc_mean)) +
  geom_smooth(method = 'loess', col = 'black') +
  geom_point(size = 3, col = 'brown4') +
  geom_linerange(aes(ymin = silt_perc_mean - silt_perc_se, ymax = silt_perc_mean + silt_perc_se), col = 'brown4') +

  labs(x = 'Elevation (m)', y = 'Silt (%)') +  theme_minimal() +
  plot_layout(nrow = 2, ncol = 4)

# save
ggsave('results/soil/soil_vars_by_elev.png',
       width = 8, height = 4, dpi = 320)
