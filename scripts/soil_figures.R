#### load libraries
library(tidyverse)
library(broom)
library(plotrix)
library(corrplot)
library(patchwork)

#### load in soil data
soil_chem <- read_csv('data/soil/soil_results.csv')
names(soil_chem)
unique(soil_chem$BlockNo)

# clean siteID names
soil_chem <- soil_chem %>%
  mutate(siteID = as.numeric(substr(BlockNo, 1, 1)))

# summary vals for each site
soil_sum <- soil_chem %>%
              group_by(siteID) %>%
              summarise_at(vars(Total_P:Sand),
                            list(mean = mean, 
                                 sd = sd,
                                 se = std.error), 
                           na.rm = T)

# correlation matrix of soils across sites
corrplot.mixed(cor(soil_sum[,2:10]))
png('results/soil/corrplot.png',
    width = 7.04, height = 3.94,
    units = 'in', res = 320)
corrplot(cor(soil_sum[,2:10]), 
         method = 'shade',
         col = '',
         type = 'lower',
         diag = F,
         order = 'AOE',
         tl.col = 'black')
dev.off()

# line plots for each soil variable
ggplot(soil_sum, aes(x = siteID, y = Total_P_mean)) +
  geom_smooth(method = 'loess', col = 'black') +
  geom_point(size = 3, col = 'red3') +
  geom_linerange(aes(ymin = Total_P_mean - Total_P_se, ymax = Total_P_mean + Total_P_se), col = 'red3') +
  scale_x_continuous(breaks = seq(1,5,1), limits = c(1,5), labels = c('2000','2200', '2400', '2600', '2800')) +
  labs(x = 'Elevation (m)', y = 'Total P (mg/kg)') +
  theme_minimal() +
  
ggplot(soil_sum,aes(x = siteID, y = TN_mean)) +
  geom_smooth(method = 'loess', col = 'black') +
  geom_point(size = 3, col = 'skyblue') +
  geom_linerange(aes(ymin = TN_mean - TN_se, ymax = TN_mean + TN_se), col = 'skyblue') +
  scale_x_continuous(breaks = seq(1,5,1), limits = c(1,5), labels = c('2000','2200', '2400', '2600', '2800')) +
  labs(x = 'Elevation (m)', y = 'Total N (%)') +  theme_minimal() +
  
ggplot(soil_sum,aes(x = siteID, y = C_mean)) +
  geom_smooth(method = 'loess', col = 'black') +
  geom_point(size = 3, col = 'lightgreen') +
  geom_linerange(aes(ymin = C_mean - C_se, ymax = C_mean + C_se), col = 'lightgreen') +
  scale_x_continuous(breaks = seq(1,5,1), limits = c(1,5), labels = c('2000','2200', '2400', '2600', '2800')) +
  labs(x = 'Elevation (m)', y = 'Total organic C (%)') +  theme_minimal() +
  
ggplot(soil_sum, aes(x = siteID, y = CEC_mean)) +
  geom_smooth(method = 'loess', col = 'black') +
  geom_point(size = 3, col = 'darkgreen') +
  geom_linerange(aes(ymin = CEC_mean - CEC_se, ymax = CEC_mean + CEC_se), col = 'darkgreen') +
  scale_x_continuous(breaks = seq(1,5,1), limits = c(1,5), labels = c('2000','2200', '2400', '2600', '2800')) +
  labs(x = 'Elevation (m)', y = 'CEC (cmol/kg)') +
  theme_minimal() +

ggplot(soil_sum, aes(x = siteID, y = pH_mean)) +
  geom_smooth(method = 'loess', col = 'black') +
  geom_point(size = 3, col = 'purple3') +
  geom_linerange(aes(ymin = pH_mean - pH_se, ymax = pH_mean + pH_se), col = 'purple3') +
  scale_x_continuous(breaks = seq(1,5,1), limits = c(1,5), labels = c('2000','2200', '2400', '2600', '2800')) +
  labs(x = 'Elevation (m)', y = 'pH') + theme_minimal() +
  

ggplot(soil_sum,aes(x = siteID, y = Sand_mean)) +
  geom_smooth(method = 'loess', col = 'black') +
  geom_point(size = 3, col = 'gold') +
  geom_linerange(aes(ymin = Sand_mean - Sand_se, ymax = Sand_mean + Sand_se), col = 'gold') +
  scale_x_continuous(breaks = seq(1,5,1), limits = c(1,5), labels = c('2000','2200', '2400', '2600', '2800')) +
  labs(x = 'Elevation (m)', y = 'Sand (%)') +  theme_minimal() +

  ggplot(soil_sum,aes(x = siteID, y = Clay_mean)) +
  geom_smooth(method = 'loess', col = 'black') +
  geom_point(size = 3, col = 'brown1') +
  geom_linerange(aes(ymin = Clay_mean - Clay_se, ymax = Clay_mean + Clay_se), col = 'brown1') +
  scale_x_continuous(breaks = seq(1,5,1), limits = c(1,5), labels = c('2000','2200', '2400', '2600', '2800')) +
  labs(x = 'Elevation (m)', y = 'Clay (%)') +  theme_minimal() +
  
ggplot(soil_sum,aes(x = siteID, y = Silt_mean)) +
  geom_smooth(method = 'loess', col = 'black') +
  geom_point(size = 3, col = 'brown4') +
  geom_linerange(aes(ymin = Silt_mean - Silt_se, ymax = Silt_mean + Silt_se), col = 'brown4') +
  scale_x_continuous(breaks = seq(1,5,1), limits = c(1,5), labels = c('2000','2200', '2400', '2600', '2800')) +
  labs(x = 'Elevation (m)', y = 'Silt (%)') +  theme_minimal() +

ggplot(soil_sum,aes(x = siteID, y = Stone_mean)) +
  geom_smooth(method = 'loess', col = 'black') +
  geom_point(size = 3, col = 'gray50') +
  geom_linerange(aes(ymin = Stone_mean - Stone_se, ymax = Stone_mean + Stone_se), col = 'gray50') +
  scale_x_continuous(breaks = seq(1,5,1), limits = c(1,5), labels = c('2000','2200', '2400', '2600', '2800')) +
  labs(x = 'Elevation (m)', y = 'Stone (%)') +  theme_minimal() +
  plot_annotation(tag_levels = 'a', tag_suffix = ')')

ggsave('results/soil/all_soil_vars.png',
       width = 8.36, height = 6.15, dpi = 320)
