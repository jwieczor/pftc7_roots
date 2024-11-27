##Final Table Technical Validation Root Scanners

fulltableTV <- bind_rows(list(RawScanners = full.tableCUT1, RootPainter = full.tableRP1, GIMP = full.tableGIMP1), .id = "source")

fullTableTV2 <- fulltableTV %>% 
  select(-c(X,date:Region.of.Interest)) %>% 
  rename(ID = File.Name) %>% 
  rename(RD = Average.Diameter.mm) %>% 
  rename(BI = Branching.frequency.per.mm) %>% 
  relocate(RD, BI, .before = SRL)


write.csv(fullTableTV2, "D:/OneDrive - University of Miami/UMiami/PFTC7/DATA/pftc7_roots/data/roots/processed/TechnicalValidationRootScannersData.csv")
write.csv(fullTableTV2, "H:/My Drive/data/FinalDatasets/TechnicalValidationRootScannersData.csv")
