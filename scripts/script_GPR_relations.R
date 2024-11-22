# GPR test
# Analysis on Pixel count and root density
# by JDW
# created 18 Oct 2024

# Load libraries ----
library(tidyverse)
library(lme4)
library(ggeffects)

# Data ----
gpr <- read.csv("data/GPR/GPR_vs_root_density_cleaned.csv") %>%
  mutate(group = as.factor(Description))

glimpse(gpr)

hist(log(gpr$root_dens_soil))

# Custom min-max scaling ----
min_val <- min(gpr$Pixel_count)
max_val <- max(gpr$Pixel_count)

gpr$Pixel_count_scaled <- (gpr$Pixel_count - min_val) / (max_val - min_val)

# Models ----
model1 <- lm(log(root_dens_soil) ~ Pixel_count, data = gpr)
model2 <- lm(log(root_dens_soil) ~ Pixel_count_scaled, data = gpr)
model3 <- lm(log(root_dens_soil) ~ Amplitude, data = gpr)

summary(model1)
summary(model2) # No difference between the two models

summary(model3)

# Predictions ----
pred_model1 <- ggpredict(model1, terms = "Pixel_count")
pred_model3 <- ggpredict(model3, terms = "Amplitude")

# Plot ----
RD_pixel <- ggplot(data = gpr, aes(x = Pixel_count, y = root_dens_soil)) +
  geom_point() +
  geom_line(data = pred_model1, aes(x = x, y = predicted), color = "blue", inherit.aes = FALSE) +  # Prediction line
  geom_ribbon(data = pred_model1, aes(x = x, ymin = conf.low, ymax = conf.high), 
                fill = "blue", alpha = 0.2, inherit.aes = FALSE) +  # Confidence interval
  theme_bw() +
  labs(x = "\nPixel count", y = "\nRoot-to-soil ratio (g g-1)\n") +
  theme(panel.grid = element_blank())

# Plot with predictions for Amplitude (Model 3)
RD_amplitude <- ggplot(aes(x = Amplitude, y = root_dens_soil), data = gpr) +
  geom_point() +
  geom_line(data = pred_model3, aes(x = x, y = predicted), color = "blue", inherit.aes = FALSE) +  # Prediction line
  geom_ribbon(data = pred_model3, aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "blue", alpha = 0.2, inherit.aes = FALSE) +  # Confidence interval
  theme_bw() +
  labs(x = "\nAmplitude", y = "\nRoot-to-soil ratio (g g-1)\n") +
  theme(panel.grid = element_blank())

RD_px_am <- gridExtra::grid.arrange(RD_pixel, RD_amplitude, nrow = 1)

ggsave(RD_px_am, file = "plots/RD_px_am.png", width = 210, height = 100, units = "mm", dpi = 320)
