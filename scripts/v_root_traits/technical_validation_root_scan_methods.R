# Technical validation comparisons between raw, rootpainter and GIMP scans
# created 26/11/2024 by Lina
# modified 01/07/2025 by Joe

### 1. Set up ----
library(tidyverse)
library(rcompanion)

### 2. Load data ----
#### load in root data
all_scan_methods <- read_csv('v_root_traits/v_PFCT7_clean_scan_methods_comparison_2023.csv')
head(all_scan_methods)

#### select key scan traits that are direct measurements from the root scans in RhizoVision
focal_scan_traits <- all_scan_methods %>% 
  dplyr::select("source", 'id', "total_root_length", "total_root_volume", "bi", "rd")

#### summary of all methods by key scan traits
focal_scan_traits %>% 
  group_by(source) %>% 
  summarise(across(c("total_root_length", "total_root_volume", "bi", "rd"),
                   .fns = list(mean = mean, sd = sd),
                   .names = "{.col}-{.fn}",
                   na.rm = TRUE)) %>%   
  pivot_longer(!source, names_sep= '-', names_to=c('variable', '.value'))

### 3. Raw vs. rootpainter scans ----
#### filter to target method comparison
raw_v_rp <- focal_scan_traits %>%
  dplyr::filter(source != "GIMP") %>%
  dplyr::select(-id) %>%
  as.data.frame()

#### test for normality across datasets
diff_raw_v_rp <- list()
shapiro_raw_v_rp <- list()

for (i in 2:ncol(raw_v_rp)) {
  diff_raw_v_rp[[i]] <- with(raw_v_rp, raw_v_rp[,i][source == "raw"] - raw_v_rp[,i][source == "rootpainter"])
  shapiro_raw_v_rp[[i]] <- shapiro.test(diff_raw_v_rp[[i]])
}
shapiro_raw_v_rp

#### transform 3 traits identified with shapiro tests
raw_v_rp_trans <- raw_v_rp %>% 
  mutate(trl_transform = transformTukey(total_root_length, plotit = F),
         vol_transform = transformTukey(total_root_volume, plotit = F),
         bi_transform = sqrt(bi)) %>%
  as.data.frame()

#### test for normality on new variables
diff_raw_v_rp_2 <- list()
shapiro_raw_v_rp_2 <- list()

for (i in 2:ncol(raw_v_rp_trans)) {
  diff_raw_v_rp_2[[i]] <- with(raw_v_rp_trans, raw_v_rp_trans[,i][source == "raw"] - raw_v_rp_trans[,i][source == "rootpainter"])
  shapiro_raw_v_rp_2[[i]] <- shapiro.test(diff_raw_v_rp_2[[i]])
}
names(shapiro_raw_v_rp_2) <- names(raw_v_rp_trans)
shapiro_raw_v_rp_2

#### run t-tests on target transformations
t_test_raw_v_rp <- raw_v_rp_trans %>% 
  select(-source) %>% 
  map_df(~ broom::tidy(t.test(Pair(.[raw_v_rp_trans$source=="raw"],.[raw_v_rp_trans$source=="rootpainter"])~1), .id = 'var'))
t_test_raw_v_rp$trait <- names(raw_v_rp_trans)[-1]
t_test_raw_v_rp <- t_test_raw_v_rp %>% dplyr::select(trait, estimate:alternative)
t_test_raw_v_rp %>%
  filter(trait %in% c('rd', 'trl_transform', 'vol_transform', 'bi_transform'))

#### check wilcox test
wilcox_test_raw_v_rp <- raw_v_rp_trans %>% 
  select(-source) %>% 
  map_df(~ broom::tidy(wilcox.test(Pair(.[raw_v_rp_trans$source=="raw"],.[raw_v_rp_trans$source=="rootpainter"])~1), .id = 'var'))
wilcox_test_raw_v_rp$trait <- names(raw_v_rp_trans)[-1]
wilcox_test_raw_v_rp <- wilcox_test_raw_v_rp %>% dplyr::select(trait, statistic:alternative)
wilcox_test_raw_v_rp %>%
  filter(trait %in% c('rd', 'trl_transform', 'vol_transform', 'bi_transform'))

#### calculate the % change in traits between different methods
raw_v_rp_trans %>%
  group_by(source) %>% 
  summarise_all(list(mean = mean),
                na.rm = TRUE) %>%
  pivot_longer(!source,names_to = "traits", values_to = "mean") %>% 
  arrange(traits) %>% 
  mutate(proportion = (mean*100)/lead(mean)) %>% 
  mutate(change = proportion - 100) %>% 
  filter(source == "raw") %>%
  filter(!str_detect(traits, 'transform')) ### Change in proportion of traits between two methods

### 4. Raw vs. GIMP scans ----
#### filter to target method comparison
raw_v_gimp <- focal_scan_traits %>%
  dplyr::filter(source != "rootpainter") %>%
  filter(id %in% focal_scan_traits$id[focal_scan_traits$source == "raw"] & id %in% focal_scan_traits$id[focal_scan_traits$source == "gimp"]) %>% 
  select(-id) %>%
  as.data.frame()

#### test for normality across datasets
diff_raw_v_gimp <- list()
shapiro_raw_v_gimp <- list()

for (i in 2:ncol(raw_v_gimp)) {
  diff_raw_v_gimp[[i]] <- with(raw_v_gimp, raw_v_gimp[,i][source == "raw"] - raw_v_gimp[,i][source == "gimp"])
  shapiro_raw_v_gimp[[i]] <- shapiro.test(diff_raw_v_gimp[[i]])
}
shapiro_raw_v_gimp

#### transform 1 trait identified with shapiro tests
raw_v_gimp_trans <- raw_v_gimp %>% 
  mutate(vol_transform = transformTukey(total_root_volume, plotit = F)) %>%
  as.data.frame()

#### test for normality on new variables
diff_raw_v_gimp_2 <- list()
shapiro_raw_v_gimp_2 <- list()

for (i in 2:ncol(raw_v_gimp_trans)) {
  diff_raw_v_gimp_2[[i]] <- with(raw_v_gimp_trans, raw_v_gimp_trans[,i][source == "raw"] - raw_v_gimp_trans[,i][source == "gimp"])
  shapiro_raw_v_gimp_2[[i]] <- shapiro.test(diff_raw_v_gimp_2[[i]])
}
names(shapiro_raw_v_gimp_2) <- names(raw_v_gimp_trans)
shapiro_raw_v_gimp_2

#### run t-tests on target transformations
t_test_raw_v_gimp <- raw_v_gimp_trans %>% 
  select(-source) %>% 
  map_df(~ broom::tidy(t.test(Pair(.[raw_v_gimp_trans$source=="raw"],.[raw_v_gimp_trans$source=="gimp"])~1), .id = 'var'))
t_test_raw_v_gimp$trait <- names(raw_v_gimp_trans)[-1]
t_test_raw_v_gimp <- t_test_raw_v_gimp %>% dplyr::select(trait, estimate:alternative)
t_test_raw_v_gimp %>%
  filter(trait %in% c('total_root_length', 'rd', 'bi', 'vol_transform'))

#### check wilcox test
wilcox_test_raw_v_gimp <- raw_v_gimp_trans %>% 
  select(-source) %>% 
  map_df(~ broom::tidy(wilcox.test(Pair(.[raw_v_gimp_trans$source=="raw"],.[raw_v_gimp_trans$source=="gimp"])~1), .id = 'var'))
wilcox_test_raw_v_gimp$trait <- names(raw_v_gimp_trans)[-1]
wilcox_test_raw_v_gimp <- wilcox_test_raw_v_gimp %>% dplyr::select(trait, statistic:alternative)
wilcox_test_raw_v_gimp %>%
  filter(trait %in% c('total_root_length', 'rd', 'bi', 'vol_transform'))

#### calculate the % change in traits between different methods
raw_v_gimp_trans %>%
  group_by(source) %>% 
  summarise_all(list(mean = mean),
                na.rm = TRUE) %>%
  pivot_longer(!source,names_to = "traits", values_to = "mean") %>% 
  arrange(traits) %>% 
  mutate(proportion = (lead(mean)*100)/mean) %>% 
  mutate(change = proportion - 100) %>% 
  filter(source == "gimp") ### Change in proportion of traits between two methods

### 5. Rootpainter vs. GIMP scans ----
#### filter to target method comparison
rp_v_gimp <- focal_scan_traits %>%
  dplyr::filter(source != "raw") %>%
  filter(id %in% focal_scan_traits$id[focal_scan_traits$source == "rootpainter"] & id %in% focal_scan_traits$id[focal_scan_traits$source == "gimp"]) %>% 
  select(-id) %>%
  as.data.frame()

#### test for normality across datasets
diff_rp_v_gimp <- list()
shapiro_rp_v_gimp <- list()

for (i in 2:ncol(rp_v_gimp)) {
  diff_rp_v_gimp[[i]] <- with(rp_v_gimp, rp_v_gimp[,i][source == "rootpainter"] - rp_v_gimp[,i][source == "gimp"])
  shapiro_rp_v_gimp[[i]] <- shapiro.test(diff_rp_v_gimp[[i]])
}
shapiro_rp_v_gimp

#### transform 1 trait identified with shapiro tests
rp_v_gimp_trans <- rp_v_gimp %>% 
  mutate(trl_transform = sqrt(total_root_length)) %>%
  as.data.frame()

#### test for normality on new variables
diff_rp_v_gimp_2 <- list()
shapiro_rp_v_gimp_2 <- list()

for (i in 2:ncol(rp_v_gimp_trans)) {
  diff_rp_v_gimp_2[[i]] <- with(rp_v_gimp_trans, rp_v_gimp_trans[,i][source == "rootpainter"] - rp_v_gimp_trans[,i][source == "gimp"])
  shapiro_rp_v_gimp_2[[i]] <- shapiro.test(diff_rp_v_gimp_2[[i]])
}
names(shapiro_rp_v_gimp_2) <- names(rp_v_gimp_trans)
shapiro_rp_v_gimp_2

#### run t-tests on target transformations
t_test_rp_v_gimp <- rp_v_gimp_trans %>% 
  select(-source) %>% 
  map_df(~ broom::tidy(t.test(Pair(.[rp_v_gimp_trans$source=="rootpainter"],.[rp_v_gimp_trans$source=="gimp"])~1), .id = 'var'))
t_test_rp_v_gimp$trait <- names(rp_v_gimp_trans)[-1]
t_test_rp_v_gimp <- t_test_rp_v_gimp %>% dplyr::select(trait, estimate:alternative)
t_test_rp_v_gimp %>%
  filter(trait %in% c('total_root_volume', 'rd', 'bi', 'trl_transform'))

#### check wilcox test
wilcox_test_rp_v_gimp <- rp_v_gimp_trans %>% 
  select(-source) %>% 
  map_df(~ broom::tidy(wilcox.test(Pair(.[rp_v_gimp_trans$source=="rootpainter"],.[rp_v_gimp_trans$source=="gimp"])~1), .id = 'var'))
wilcox_test_rp_v_gimp$trait <- names(rp_v_gimp_trans)[-1]
wilcox_test_rp_v_gimp <- wilcox_test_rp_v_gimp %>% dplyr::select(trait, statistic:alternative)
wilcox_test_rp_v_gimp %>%
  filter(trait %in% c('total_root_volume', 'rd', 'bi', 'trl_transform'))

#### calculate the % change in traits between different methods
rp_v_gimp_trans %>%
  group_by(source) %>% 
  summarise_all(list(mean = mean),
                na.rm = TRUE) %>%
  pivot_longer(!source,names_to = "traits", values_to = "mean") %>% 
  arrange(traits) %>% 
  mutate(proportion = (mean*100)/lead(mean)) %>% 
  mutate(change = proportion - 100) %>% 
  filter(source == "gimp") ### Change in proportion of traits between two methods

