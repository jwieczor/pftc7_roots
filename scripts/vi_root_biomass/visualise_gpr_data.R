# Analysis on GPR pixel count and amplitude versus root biomass
# created 18/10/2024 by Jakub
# modified 3/12/2024 by Jakub
# modified 01/07/2025 by Joe

### 1. Set up ----
library(tidyverse)
library(lme4)
library(ggeffects)
library(ggridges)
library(ggpubr)
library(patchwork)

### load clean transect and root biomass data
gpr_clean <- read_csv('vi_root_biomass/vi_PFTC7_clean_gpr_transect_root_biomass_2023.csv') 

### 2. Format and glimpse data ----
# keep only detections with corresponding root biomass data
gpr <- gpr_clean %>%
  filter(!is.na(root_to_soil_ratio))

glimpse(gpr)
hist(log(gpr$root_to_soil_ratio))

### 3. Density plots per variable by elevation ----
scl= 10
alp3 = 0.7
cols2<- c("#A72197",  "#DD5E66", "#EF7F4F", "#FDCB26") # 2800m: #6F00A8
lwd = 0.5

gpr$elevation_m_asl <- factor(gpr$elevation_m_asl)

biomass <- ggplot(gpr, aes(x = root_to_soil_ratio, y = elevation_m_asl, fill = elevation_m_asl)) +
  geom_density_ridges(alpha= alp3, scale = scl, linewidth=lwd)+
  scale_fill_manual(values = cols2, breaks = c(2600, 2400, 2200, 2000))+
  scale_y_discrete(expand = expansion(mult = c(0.1, 3.1)))+
  labs(x = "Root-to-soil ratio (g g-1)", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

amplitude <- ggplot(gpr, aes(x = amplitude, y = elevation_m_asl, fill = elevation_m_asl)) +
  geom_density_ridges(alpha= alp3, scale = scl, linewidth=lwd)+
  scale_fill_manual(values = cols2, breaks = c(2600, 2400, 2200, 2000))+
  scale_y_discrete(expand = expansion(mult = c(0.1, 3.1)))+
  labs(x = "Amplitude (dB)", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

pixel_count <- ggplot(gpr, aes(x = pixel_count, y = elevation_m_asl, fill = elevation_m_asl)) +
  geom_density_ridges(alpha= alp3, scale = scl, linewidth=lwd)+
  scale_fill_manual(values = cols2, breaks = c(2600, 2400, 2200, 2000))+
  scale_y_discrete(expand = expansion(mult = c(0.1, 3.1)))+
  labs(x = "Pixel count", color = "Elevation", fill ="Elevation")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

# Combine into single panel 
gpr_vars_elev <- ggarrange(biomass + rremove("y.text"), amplitude + rremove("y.text"), pixel_count + rremove("y.text"), common.legend = TRUE, legend = "right", align = "v", label.y = 1, nrow = 1) %>%
  annotate_figure(left = text_grob("Density",rot = 90))
gpr_vars_elev

# Save figure
ggsave("gpr_vars_elev.png", gpr_vars_elev, path= "results/vi_root_biomass", height = 1.5, width = 7 , bg= "white", units= "in")

### 4. Run regression models ----
# Custom min-max scaling ----
min_val <- min(gpr$pixel_count)
max_val <- max(gpr$pixel_count)
gpr$pixel_count_scaled <- (gpr$pixel_count - min_val) / (max_val - min_val)

# Models ----
model1 <- lm(log(root_to_soil_ratio) ~ pixel_count, data = gpr)
model2 <- lm(log(root_to_soil_ratio) ~ pixel_count_scaled, data = gpr)
model3 <- lm(log(root_to_soil_ratio) ~ amplitude, data = gpr)

summary(model1)
summary(model2) # No difference between model1 and model2
summary(model3)

# Predictions ----
pred_model1 <- ggpredict(model1, terms = "pixel_count")
pred_model3 <- ggpredict(model3, terms = "amplitude")

# Plot ----
# Plot with predictions for Pixel count (Model 1)
ggplot(data = gpr, aes(x = pixel_count, y = root_to_soil_ratio)) +
  geom_point() +
  geom_line(data = pred_model1, aes(x = x, y = predicted), color = "blue", inherit.aes = FALSE) +  # Prediction line
  geom_ribbon(data = pred_model1, aes(x = x, ymin = conf.low, ymax = conf.high), 
                fill = "blue", alpha = 0.2, inherit.aes = FALSE) +  # Confidence interval
  theme_bw() +
  labs(x = "\nPixel count", y = "\nRoot-to-soil ratio (g g-1)\n") +
  theme(panel.grid = element_blank()) +

# Plot with predictions for Amplitude (Model 3)
ggplot(aes(x = amplitude, y = root_to_soil_ratio), data = gpr) +
  geom_point() +
  geom_line(data = pred_model3, aes(x = x, y = predicted), color = "blue", inherit.aes = FALSE) +  # Prediction line
  geom_ribbon(data = pred_model3, aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "blue", alpha = 0.2, inherit.aes = FALSE) +  # Confidence interval
  theme_bw() +
  labs(x = "\nAmplitude (dB)", y = "\nRoot-to-soil ratio (g g-1)\n") +
  theme(panel.grid = element_blank())

# Save figure
ggsave("gpr_regression_models.png", path= "results/vi_root_biomass", height = 3, width = 7 , bg= "white", units= "in")
