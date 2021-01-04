library(data.table)
library(tidyverse)
D3<-fread('./preprocessing/export_table_coh.csv', header = TRUE, sep = ',', check.names = FALSE)


D3 <- D3 %>% select(-contains("parietooccipital_central_A>"))
D3 <- D3 %>% select(-contains(">parietooccipital_central_A"))
D3 <- D3 %>% select(-contains("parietooccipital_cen>"))
D3 <- D3 %>% select(-contains(">parietooccipital_cen"))


D3 <- D3 %>% select(-contains("frontal_links_A>"))
D3 <- D3 %>% select(-contains(">frontal_links_A"))
D3 <- D3 %>% select(-contains("frontal_rechts_A>"))
D3 <- D3 %>% select(-contains(">frontal_rechts_A"))


D3 <- D3 %>% select(-contains("frontocentral_l_A>"))
D3 <- D3 %>% select(-contains(">frontocentral_l_A"))
D3 <- D3 %>% select(-contains("frontocentral_r_A>"))
D3 <- D3 %>% select(-contains(">frontocentral_r_A"))


D3 <- D3 %>% select(-contains("temporal_links_A>"))
D3 <- D3 %>% select(-contains(">temporal_links_A"))
D3 <- D3 %>% select(-contains("temporal_rechts_A>"))
D3 <- D3 %>% select(-contains(">temporal_rechts_A"))


D3 <- D3 %>% select(-contains("parietal_links_A>"))
D3 <- D3 %>% select(-contains(">parietal_links_A"))
D3 <- D3 %>% select(-contains("parietal_rechts_A>"))
D3 <- D3 %>% select(-contains(">parietal_rechts_A"))

D3 <- D3 %>% select(-contains("parietooccipital_l_A>"))
D3 <- D3 %>% select(-contains(">parietooccipital_l_A"))
D3 <- D3 %>% select(-contains("parietooccipital_r_A>"))
D3 <- D3 %>% select(-contains(">parietooccipital_r_A"))

D3 <- D3 %>% select(-contains("1."))
D3 <- D3 %>% select(-contains("2."))
D3 <- D3 %>% select(-contains("3."))
D3 <- D3 %>% select(-contains("4."))
D3 <- D3 %>% select(-contains("5."))
D3 <- D3 %>% select(-contains("6."))
D3 <- D3 %>% select(-contains("7."))
D3 <- D3 %>% select(-contains("8."))
D3 <- D3 %>% select(-contains("9."))
D3 <- D3 %>% select(-contains("10."))
D3 <- D3 %>% select(-contains("20."))
D3 <- D3 %>% select(-contains("30."))
D3 <- D3 %>% select(-contains("40."))
D3 <- D3 %>% select(-contains("8832"))
D3 <- D3 %>% select(-contains("9344"))
D3 <- D3 %>% select(-contains("12928"))
D3 <- D3 %>% select(-contains("13440"))
D3 <- D3 %>% select(-contains("13952"))
D3 <- D3 %>% select(-contains("14464"))
D3 <- D3 %>% select(-contains("29824"))
D3 <- D3 %>% select(-contains("30336"))

write.csv(D3,"./preprocessing/test_coh.csv", row.names = FALSE)
write.csv(D3,"./app/tests/testthat/data/test_coh.csv", row.names = FALSE)
