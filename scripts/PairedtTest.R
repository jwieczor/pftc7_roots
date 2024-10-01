## Paired T-test analyses

## Load packages
pkgs <- c("dplyr", "tidyverse", "stringr", "tidyr")
lapply(pkgs, library, character.only = TRUE)
remove(pkgs)

##Load tables
full.tableCUT1<- read.csv("D:/OneDrive - University of Miami/UMiami/PFTC7/DATA/pftc7_roots/data/roots/processed/20240825_FullTableCUT.csv",
                          h = T)
full.tableRP1 <- read.csv("D:/OneDrive - University of Miami/UMiami/PFTC7/DATA/pftc7_roots/data/roots/processed/20240825_FullTableRP1.csv",
                          h = T)
full.tableGIMP1 <- read.csv("D:/OneDrive - University of Miami/UMiami/PFTC7/DATA/pftc7_roots/data/roots/processed/20240825_FullTableGIMP.csv",
                            h = T)


## CUT avz## CUT vs RP
##1. Bind the CUT and RP tables
CUTvRP <- bind_rows(list(CUT = full.tableCUT1, RP = full.tableRP1), .id = "source")


##2. Summary statistics by group

ssCUTvRP <- group_by(CUTvRP, source) %>% 
  summarise(across(c("SRL", "RTD", "Branching.frequency.per.mm", "Average.Diameter.mm"),
                   list(mean = mean, sd = sd),
                   na.rm = TRUE))

##3. Difference between CUT and RV for each trait, shapiro test for normality of the difference and paired t-test

CUTvRP2 <- CUTvRP %>% 
  select("source", "SRL", "RTD", "Branching.frequency.per.mm", "Average.Diameter.mm") %>% 
  mutate(RTD_log = log(RTD)) %>% 
  mutate(BF_sqrt = sqrt(Branching.frequency.per.mm))
 
difference_CUTvRP <- list()
shapiro_CUTvRP <- list()

for (i in 2:ncol(CUTvRP2)) {
  difference_CUTvRP[[i]] <- with(CUTvRP2, CUTvRP2[,i][source == "CUT"] - CUTvRP2[,i][source == "RP"])
  shapiro_CUTvRP[[i]] <- shapiro.test(difference_CUTvRP[[i]])
}
shapiro_CUTvRP #SRL_log: 0.035, BF_log: 0.09


t.TEST_CUTvRP <- CUTvRP2 %>% 
  select("SRL", "RTD", "RTD_log", "Branching.frequency.per.mm", "BF_sqrt", "Average.Diameter.mm") %>%
  map_df(~ broom::tidy(t.test(. ~ CUTvRP2$source, paired = TRUE)), .id = 'var')
t.TEST_CUTvRP

wilcox.TEST_CUTvRP <- CUTvRP2 %>% 
  select("SRL", "RTD", "Branching.frequency.per.mm", "Average.Diameter.mm") %>%
  map_df(~ broom::tidy(wilcox.test(. ~ CUTvRP2$source, paired = TRUE)), .id = 'var')
wilcox.TEST_CUTvRP

write.csv(t.TEST_CUTvRP, "D:/OneDrive - University of Miami/UMiami/PFTC7/DATA/pftc7_roots/results/roots/processed/20240930_t.TEST_CUTvRP.csv")
write.csv(wilcox.TEST_CUTvRP, "D:/OneDrive - University of Miami/UMiami/PFTC7/DATA/pftc7_roots/results/roots/processed/20240930_wilcox.TEST_CUTvRP.csv")

## CUT vs GIMP

##1. Filter out from the CUT table the samples that were not processed using GIMP
CUTfiltered <- full.tableCUT1 %>% 
  filter(File.Name %in% full.tableGIMP1$File.Name)

##2. Bind the CUT and GIMP tables
CUTvGIMP <- bind_rows(list(CUT = CUTfiltered, RP = full.tableGIMP1), .id = "source")


##2. Summary statistics by group

ssCUTvGIMP <- group_by(CUTvGIMP, source) %>% 
  summarise(across(c("SRL", "RTD", "Branching.frequency.per.mm", "Average.Diameter.mm"),
                   list(mean = mean, sd = sd),
                   na.rm = TRUE))

##3. Difference between CUT and RV for each trait, shapiro test for normality of the difference and paired t-test

CUTvGIMP2 <- CUTvGIMP %>% 
  select("source", "SRL", "RTD", "Branching.frequency.per.mm", "Average.Diameter.mm") %>% 
  mutate(SRL_log = log(SRL))

difference_CUTvGIMP <- list()
shapiro_CUTvGIMP <- list()

for (i in 2:ncol(CUTvGIMP2)) {
  difference_CUTvGIMP[[i]] <- with(CUTvGIMP2, CUTvGIMP2[,i][source == "CUT"] - CUTvGIMP2[,i][source == "RP"])
  shapiro_CUTvGIMP[[i]] <- shapiro.test(difference_CUTvGIMP[[i]])
}
shapiro_CUTvGIMP #SRL_log: 0.049



t.TEST_CUTvGIMP <- CUTvGIMP2 %>% 
  select("SRL", "SRL_log", "RTD", "Branching.frequency.per.mm", "Average.Diameter.mm") %>%
  map_df(~ broom::tidy(t.test(. ~ CUTvGIMP2$source, paired = TRUE)), .id = 'var')
t.TEST_CUTvGIMP

wilcox.TEST_CUTvGIMP <- CUTvGIMP2 %>% 
  select("SRL", "RTD", "Branching.frequency.per.mm", "Average.Diameter.mm") %>%
  map_df(~ broom::tidy(wilcox.test(. ~ CUTvGIMP2$source, paired = TRUE)), .id = 'var')
wilcox.TEST_CUTvGIMP

write.csv(t.TEST_CUTvGIMP, "D:/OneDrive - University of Miami/UMiami/PFTC7/DATA/pftc7_roots/results/roots/processed/20240930_t.TEST_CUTvGIMP.csv")
write.csv(wilcox.TEST_CUTvGIMP, "D:/OneDrive - University of Miami/UMiami/PFTC7/DATA/pftc7_roots/results/roots/processed/20240930_wilcox.TEST_CUTvGIMP.csv")


## RP vs GIMP

##1. Filter out from the RP table the samples that were not processed using GIMP
RPfiltered <- full.tableRP1 %>% 
  filter(File.Name %in% full.tableGIMP1$File.Name)

##2. Bind the CUT and GIMP tables
RPvGIMP <- bind_rows(list(CUT = RPfiltered, RP = full.tableGIMP1), .id = "source")


##2. Summary statistics by group

ssRPvGIMP <- group_by(RPvGIMP, source) %>% 
  summarise(across(c("SRL", "RTD", "Branching.frequency.per.mm", "Average.Diameter.mm"),
                   list(mean = mean, sd = sd),
                   na.rm = TRUE))

##3. Difference between CUT and RV for each trait, shapiro test for normality of the difference and paired t-test

RPvGIMP2 <- RPvGIMP %>% 
  select("source", "SRL", "RTD", "Branching.frequency.per.mm", "Average.Diameter.mm") %>% 
  mutate(SRL_log = log(SRL)) %>% 
  mutate(RTD_log = log(RTD))

difference_RPvGIMP <- list()
shapiro_RPvGIMP <- list()

for (i in 2:ncol(RPvGIMP2)) {
  difference_RPvGIMP[[i]] <- with(RPvGIMP2, RPvGIMP2[,i][source == "CUT"] - RPvGIMP2[,i][source == "RP"])
  shapiro_RPvGIMP[[i]] <- shapiro.test(difference_RPvGIMP[[i]])
}
shapiro_RPvGIMP #SRL_log and RTD_log non normal


t.TEST_RPvGIMP <- RPvGIMP2 %>% 
  select("SRL", "RTD", "Branching.frequency.per.mm", "Average.Diameter.mm") %>%
  map_df(~ broom::tidy(t.test(. ~ RPvGIMP2$source, paired = TRUE)), .id = 'var')
t.TEST_RPvGIMP

wilcox.TEST_RPvGIMP <- RPvGIMP2 %>% 
  select("SRL", "RTD", "Branching.frequency.per.mm", "Average.Diameter.mm") %>%
  map_df(~ broom::tidy(wilcox.test(. ~ RPvGIMP2$source, paired = TRUE)), .id = 'var')
wilcox.TEST_RPvGIMP

write.csv(t.TEST_RPvGIMP, "D:/OneDrive - University of Miami/UMiami/PFTC7/DATA/pftc7_roots/results/roots/processed/20240930_t.TEST_RPvGIMP.csv")
write.csv(wilcox.TEST_RPvGIMP, "D:/OneDrive - University of Miami/UMiami/PFTC7/DATA/pftc7_roots/results/roots/processed/20240930_wilcox.TEST_RPvGIMP.csv")
