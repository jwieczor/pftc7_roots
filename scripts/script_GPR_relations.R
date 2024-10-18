# GPR
# Analysis on Pixel count and root density
# by JDW
# created 18 Oct 2024

# Load libraries ----
library(tidyverse)
library(lme4)

# Data ----
gpr <- read.csv("data/GPR_vs_root_density_cleaned.csv") %>%
  mutate(group = as.factor(Description))

glimpse(gpr)

hist(log(gpr$root_dens_soil))

# Custom min-max scaling
min_val <- min(gpr$Pixel_count)
max_val <- max(gpr$Pixel_count)

gpr$Pixel_count_scaled <- (gpr$Pixel_count - min_val) / (max_val - min_val)

# Models ----
model1 <- lm(log(root_dens_soil) ~ Pixel_count, data = gpr)
model2 <- lm(log(root_dens_soil) ~ Pixel_count_scaled, data = gpr)

summary(model1)
summary(model2) # No difference between the two models

# Plot ----
RD_pixel <- ggplot(aes(x = Pixel_count, y = log(root_dens_soil)), data = gpr) +
  geom_point() +
  stat_smooth(method=lm) +
  #facet_wrap(~group, nrow = 2) +
  theme_bw() +
  labs(x = "\nPixel count", y = "Root density (log)\n") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave(RD_pixel, file = "plots/RD_pixell.png", width = 210, height = 100, units = "mm", dpi = 320)
