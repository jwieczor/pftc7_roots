# Technical validation comparisons between raw, rootpainter and GIMP scans
# created 26/11/2024 by Lina
# modified 01/07/2025 by Joe

### 1. Set up ----
library(tidyverse)
library(rcompanion)

### 2. Load data ----
#### load in root data, pivot wider, select root mass measurements
roots_select <- read_csv('v_root_traits/v_PFCT7_clean_root_traits_2023.csv') %>%
  dplyr::select(id, traits, value) %>%
  pivot_wider(names_from = 'traits', values_from = 'value') %>%
  dplyr::select(id, root_dry_mass, root_wet_mass)

### load in scans
raw_scans <- read_csv('raw_data/v_raw_root_traits/PFTC7_SA_raw_scans_2023.csv')
gimp_scans <- read_csv('raw_data/v_raw_root_traits/PFTC7_SA_raw_gimp_scans_2023.csv')
rootpainter_scans <- read_csv('raw_data/v_raw_root_traits/PFTC7_SA_raw_rootpainter_scans_2023.csv')

# select id and key root scan metrics
# raw
raw_scans_select <- raw_scans %>%
  dplyr::select(id = File.Name, total_root_length = Total.Root.Length.mm, total_root_volume = Volume.mm3, rd = Average.Diameter.mm, bi = Branching.frequency.per.mm)

# gimp
gimp_scans_select <- gimp_scans %>%
  dplyr::select(id = File.Name, total_root_length = Total.Root.Length.mm, total_root_volume = Volume.mm3, rd = Average.Diameter.mm, bi = Branching.frequency.per.mm)

# rootpainter
rootpainter_scans_select <- rootpainter_scans %>%
  dplyr::select(id = File.Name, total_root_length = Total.Root.Length.mm, total_root_volume = Volume.mm3, rd = Average.Diameter.mm, bi = Branching.frequency.per.mm)

# combine root scans together
all_scans <- bind_rows(raw_scans_select, gimp_scans_select, rootpainter_scans_select, .id = 'source') %>%
  mutate(source = case_when(
    source == 1 ~ 'raw',
    source == 2 ~ 'gimp',
    source == 3 ~ 'rootpainter'
  ))

# join together with key root trait metrics and calculate derived traits (SRL, RTD, RDMC)
methods_comparison <- all_scans %>%
  left_join(roots_select, by = 'id') %>% 
  mutate(srl = (total_root_length*0.001)/root_dry_mass,
         rtd = root_dry_mass/(total_root_volume*0.001),
         rdmc = (root_dry_mass*1000)/root_wet_mass) %>%
  dplyr::select(source:bi, srl:rdmc)

#### select key scan traits that are direct measurements from the root scans in RhizoVision
focal_scan_traits <- methods_comparison %>% 
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

