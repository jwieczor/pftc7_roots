# GPR test
# Analysis on Pixel count and root density
# by Jakub
# created 18 Oct 2024
# modified 3 Dec 2024

# Load libraries ----
library(tidyverse)
library(lme4)
library(ggeffects)
library(patchwork)

# Data ----
gpr <- read.csv("data/GPR/gpr_transects_and_root_density.csv") %>%
  filter(!is.na(root_to_soil_ratio)) # keep only detections with corresponding soil data

glimpse(gpr)

hist(log(gpr$root_to_soil_ratio))

# Custom min-max scaling ----
min_val <- min(gpr$pixel_count)
max_val <- max(gpr$pixel_count)

gpr$pixel_count_scaled <- (gpr$pixel_count - min_val) / (max_val - min_val)

# Models ----
model1 <- lm(log(root_to_soil_ratio) ~ pixel_count, data = gpr)
model2 <- lm(log(root_to_soil_ratio) ~ pixel_count_scaled, data = gpr)
model3 <- lm(log(root_to_soil_ratio) ~ amplitude, data = gpr)

summary(model1)
summary(model2) # No difference between the two models
summary(model3)

# Predictions ----
pred_model1 <- ggpredict(model1, terms = "pixel_count")
pred_model3 <- ggpredict(model3, terms = "amplitude")

# Plot ----
RD_pixel <- ggplot(data = gpr, aes(x = pixel_count, y = root_to_soil_ratio)) +
  geom_point() +
  geom_line(data = pred_model1, aes(x = x, y = predicted), color = "blue", inherit.aes = FALSE) +  # Prediction line
  geom_ribbon(data = pred_model1, aes(x = x, ymin = conf.low, ymax = conf.high), 
                fill = "blue", alpha = 0.2, inherit.aes = FALSE) +  # Confidence interval
  theme_bw() +
  labs(x = "\nPixel count", y = "\nRoot-to-soil ratio (g g-1)\n") +
  theme(panel.grid = element_blank())

# Plot with predictions for Amplitude (Model 3)
RD_amplitude <- ggplot(aes(x = amplitude, y = root_to_soil_ratio), data = gpr) +
  geom_point() +
  geom_line(data = pred_model3, aes(x = x, y = predicted), color = "blue", inherit.aes = FALSE) +  # Prediction line
  geom_ribbon(data = pred_model3, aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "blue", alpha = 0.2, inherit.aes = FALSE) +  # Confidence interval
  theme_bw() +
  labs(x = "\nAmplitude (dB)", y = "\nRoot-to-soil ratio (g g-1)\n") +
  theme(panel.grid = element_blank())

RD_px_am <- RD_pixel + RD_amplitude & plot_annotation(tag_levels = 'a', tag_suffix = ')')

ggsave(RD_px_am, file = "results/gpr/RD_px_am.png", width = 210, height = 100, units = "mm", dpi = 320)

####
gpr$elevation <- factor(gpr$elevation)
gpr$elevation <- paste0(gpr$elevation, " m")

ggplot(data = gpr, aes(x = root_dens_soil, fill = elevation, colour = elevation)) +
  geom_density(alpha = 0.5) +
  labs(x = "Root-to-soil ratio (g g-1)",
       y = NULL) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +

ggplot(data = gpr, aes(x = amplitude, fill = elevation, colour = elevation)) +
  geom_density(alpha = 0.5) +
  labs(x = "Amplitude",
       y = NULL) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +

ggplot(data = gpr, aes(x = pixel_count, fill = elevation, colour = elevation)) +
  geom_density(alpha = 0.5) +
  labs(x = "Pixel intensity",
       y = NULL) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +
  plot_layout(nrow = 1, guides = 'collect') &
  plot_annotation(tag_levels = 'a', tag_suffix = ')')
