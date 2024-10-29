ft <- read.csv("H:/My Drive/data/FinalDatasets/20241016_FullTableRP1.csv")

sumsta_ALL <- ft %>% 
  summarise(across(c("Average.Diameter.mm",, "Branching.frequency.per.mm", "SRL", "RTD", "RDMC"),
                   .fns = list(mean = mean, sd = sd),
                   na.rm = TRUE)) %>% 
  pivot_longer(everything(), names_sep='_', names_to=c('variable', '.value'))
sumsta_ALL
