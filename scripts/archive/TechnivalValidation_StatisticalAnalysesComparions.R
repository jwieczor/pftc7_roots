### Technical Validation Comparisons Between RawScanners, RootPainter and GIMP
# Lina Aragón
# 2024/11/26

#### load libraries
library(dplyr)
library(tidyverse)
library(stringr)
library(tidyr)

#### load in root data
tvalidation.table <- read.csv('data/roots/processed/TechnicalValidationRootScannersData.csv', h = T)
head(tvalidation.table)

### Summary statistics for traits
summary.table.all <- tvalidation.table %>% 
  group_by(source) %>% 
  summarise(across(c("Total.Root.Length.mm", "Volume.mm3", "BI", "RD"),
                   list(mean = mean, sd = sd),
                   na.rm = TRUE)) %>%   
  pivot_longer(!source, names_sep='_', names_to=c('variable', '.value'))

### Difference between RawScanners and RootPainter for each trait, shapiro test for normality of the difference and paired t-test

CUTvRP2 <- tvalidation.table %>% 
  select("source", "Total.Root.Length.mm", "Volume.mm3", "BI", "RD") %>% 
  dplyr::filter(source != "GIMP")

difference_CUTvRP <- list()
shapiro_CUTvRP <- list()

for (i in 2:ncol(CUTvRP2)) {
  difference_CUTvRP[[i]] <- with(CUTvRP2, CUTvRP2[,i][source == "RawScanners"] - CUTvRP2[,i][source == "RootPainter"])
  shapiro_CUTvRP[[i]] <- shapiro.test(difference_CUTvRP[[i]])
}
shapiro_CUTvRP 


CUTvRP3 <- CUTvRP2 %>% 
  mutate(TRL_transform = transformTukey(Total.Root.Length.mm),
         Volume_transform = transformTukey(Volume.mm3),
         BI_transform = sqrt(BI))

difference_CUTvRP3 <- list()
shapiro_CUTvRP3 <- list()

for (i in 2:ncol(CUTvRP3)) {
  difference_CUTvRP3[[i]] <- with(CUTvRP3, CUTvRP3[,i][source == "RawScanners"] - CUTvRP3[,i][source == "RootPainter"])
  shapiro_CUTvRP3[[i]] <- shapiro.test(difference_CUTvRP3[[i]])
}
shapiro_CUTvRP3 

t.TEST_CUTvRP <- CUTvRP3 %>% 
  select(-source) %>% 
  map_df(~ broom::tidy(t.test(Pair(.[CUTvRP3$source=="RawScanners"],.[CUTvRP3$source=="RootPainter"])~1), .id = 'var'))
t.TEST_CUTvRP

wilcox.TEST_CUTvRP <- CUTvRP3 %>% 
  select(-source) %>% 
  map_df(~ broom::tidy(wilcox.test(Pair(.[CUTvRP3$source=="RawScanners"],.[CUTvRP3$source=="RootPainter"])~1), .id = 'var'))
wilcox.TEST_CUTvRP


change_CUTvRP <- CUTvRP3 %>%
  group_by(source) %>% 
  summarise_all(list(mean = mean),
                na.rm = TRUE) %>%
  pivot_longer(!source,names_to = "Traits", values_to = "Mean") %>% 
  arrange(Traits) %>% 
  mutate(proportion = (Mean*100)/lead(Mean)) %>% 
  mutate(change = proportion - 100) %>% 
  filter(source == "RawScanners") ### Change in proportion of traits between CUT and RP


### Difference between RawScanners and GIMP for each trait, shapiro test for normality of the difference and paired t-test
CUTvGIMP1 <- tvalidation.table %>% 
  select("source", "ID", "Total.Root.Length.mm", "Volume.mm3", "BI", "RD") %>% 
  dplyr::filter(source != "RootPainter")

summary.table.CUTvGIMP <- CUTvGIMP1 %>% 
  group_by(source) %>% 
  summarise(across(c("Total.Root.Length.mm", "Volume.mm3", "BI", "RD"),
                   list(mean = mean, sd = sd),
                   na.rm = TRUE)) %>%   
  pivot_longer(!source, names_sep='_', names_to=c('variable', '.value'))


#Filter from ID in CUTvGIMP1 values that are not in both CUT and GIMP
CUTvGIMP2 <- CUTvGIMP1 %>% 
  filter(ID %in% tvalidation.table$ID[tvalidation.table$source == "RawScanners"] & ID %in% tvalidation.table$ID[tvalidation.table$source == "GIMP"]) %>% 
  select(-ID)

difference_CUTvGIMP <- list()
shapiro_CUTvGIMP <- list()

for (i in 2:ncol(CUTvGIMP2)) {
  difference_CUTvGIMP[[i]] <- with(CUTvGIMP2, CUTvGIMP2[,i][source == "RawScanners"] - CUTvGIMP2[,i][source == "GIMP"])
  shapiro_CUTvGIMP[[i]] <- shapiro.test(difference_CUTvGIMP[[i]])
}
shapiro_CUTvGIMP 


CUTvGIMP3 <- CUTvGIMP2 %>% 
  mutate(Volume_transform = transformTukey(Volume.mm3))

difference_CUTvGIMP3 <- list()
shapiro_CUTvGIMP3 <- list()

for (i in 2:ncol(CUTvGIMP3)) {
  difference_CUTvGIMP3[[i]] <- with(CUTvGIMP3, CUTvGIMP3[,i][source == "RawScanners"] - CUTvGIMP3[,i][source == "GIMP"])
  shapiro_CUTvGIMP3[[i]] <- shapiro.test(difference_CUTvGIMP3[[i]])
}
shapiro_CUTvGIMP3 

t.TEST_CUTvGIMP <- CUTvGIMP3 %>% 
  select(-source) %>% 
  map_df(~ broom::tidy(t.test(Pair(.[CUTvGIMP3$source=="RawScanners"],.[CUTvGIMP3$source=="GIMP"])~1), .id = 'var'))
t.TEST_CUTvGIMP


change_CUTvGIMP <- CUTvGIMP3 %>%
  group_by(source) %>% 
  summarise_all(list(mean = mean),
                na.rm = TRUE) %>%
  pivot_longer(!source,names_to = "Traits", values_to = "Mean") %>% 
  arrange(Traits) %>% 
  mutate(proportion = (lead(Mean)*100)/Mean) %>% 
  mutate(change = proportion - 100) %>% 
  filter(source == "GIMP") ### Change in proportion of traits between CUT and GIMP



### Difference between RawScanners and GIMP for each trait, shapiro test for normality of the difference and paired t-test
RPvsGIMP1 <- tvalidation.table %>% 
  select("source", "ID", "Total.Root.Length.mm", "Volume.mm3", "BI", "RD") %>% 
  dplyr::filter(source != "RawScanners")

summary.table.RPvsGIMP <- RPvsGIMP1 %>% 
  group_by(source) %>% 
  summarise(across(c("Total.Root.Length.mm", "Volume.mm3", "BI", "RD"),
                   list(mean = mean, sd = sd),
                   na.rm = TRUE)) %>%   
  pivot_longer(!source, names_sep='_', names_to=c('variable', '.value'))


#Filter from ID in RPvsGIMP1 values that are not in both CUT and GIMP
RPvsGIMP2 <- RPvsGIMP1 %>% 
  filter(ID %in% tvalidation.table$ID[tvalidation.table$source == "RootPainter"] & ID %in% tvalidation.table$ID[tvalidation.table$source == "GIMP"]) %>% 
  select(-ID)

difference_RPvsGIMP <- list()
shapiro_RPvsGIMP <- list()

for (i in 2:ncol(RPvsGIMP2)) {
  difference_RPvsGIMP[[i]] <- with(RPvsGIMP2, RPvsGIMP2[,i][source == "RootPainter"] - RPvsGIMP2[,i][source == "GIMP"])
  shapiro_RPvsGIMP[[i]] <- shapiro.test(difference_RPvsGIMP[[i]])
}
shapiro_RPvsGIMP 


RPvsGIMP3 <- RPvsGIMP2 %>% 
  mutate(TRL_transform = sqrt(Total.Root.Length.mm))

difference_RPvsGIMP3 <- list()
shapiro_RPvsGIMP3 <- list()

for (i in 2:ncol(RPvsGIMP3)) {
  difference_RPvsGIMP3[[i]] <- with(RPvsGIMP3, RPvsGIMP3[,i][source == "RootPainter"] - RPvsGIMP3[,i][source == "GIMP"])
  shapiro_RPvsGIMP3[[i]] <- shapiro.test(difference_RPvsGIMP3[[i]])
}
shapiro_RPvsGIMP3 

t.TEST_RPvsGIMP <- RPvsGIMP3 %>% 
  select(-source) %>% 
  map_df(~ broom::tidy(t.test(Pair(.[RPvsGIMP3$source=="RootPainter"],.[RPvsGIMP3$source=="GIMP"])~1), .id = 'var'))
t.TEST_RPvsGIMP


wilcox.TEST_RPvsGIMP <- RPvsGIMP3 %>% 
  select(-source) %>% 
  map_df(~ broom::tidy(wilcox.test(Pair(.[RPvsGIMP3$source=="RootPainter"],.[RPvsGIMP3$source=="GIMP"])~1), .id = 'var'))
wilcox.TEST_RPvsGIMP

change_RPvsGIMP <- RPvsGIMP3 %>%
  group_by(source) %>% 
  summarise_all(list(mean = mean),
                na.rm = TRUE) %>%
  pivot_longer(!source,names_to = "Traits", values_to = "Mean") %>% 
  arrange(Traits) %>% 
  mutate(proportion = (Mean*100)/lead(Mean)) %>% 
  mutate(change = proportion - 100) %>% 
  filter(source == "GIMP") ### Change in proportion of traits between CUT and GIMP
