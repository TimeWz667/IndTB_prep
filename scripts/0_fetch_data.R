library(tidyverse)
library(dpdt)
library(glue)


dir.create("data_raw/", showWarnings = F)



#### Demography
raw_pop <- dpdt::fetch_demography("ByCountry/India")

save(raw_pop, file = here::here("data_raw", "wpp2019.rdata"))


raw_wup <- readxl::read_xls("../PublicData/WUP/WUP2018-F21-Proportion_Urban_Annual.xls", skip = 16) %>% 
  select(Country = `Region, subregion, country or area`, `1960`:`2050`) %>% 
  filter(Country == "India") %>% 
  pivot_longer(-Country, names_to = "Year", values_to = "PrUrban")

save(raw_wup, file = here("data_raw", "wup2018.rdata"))


#### WHO 
data_list <- c(
  "TB_burden_age_sex" = "who_burden_age_sex", 
  "TB_burden_countries" = "who_burden", 
  "TB_dr_surveillance" = "who_dr",           
  "TB_expenditure_utilisation" = "who_exp",
  "TB_notifications" = "who_cases", 
  "TB_outcomes" = "who_tx" 
)


for (src in names(data_list)) {
  src <- as_glue(src)
  tar <- as_glue(data_list[src])
  
  obj <- "raw_" + tar
  assign(obj, read_csv("../DataPublic/WHO/" + src + ".csv") %>% 
           filter(iso3 == "IND") %>% 
           filter(year > 2013))
 
  save(list = obj, file = here::here("data_raw", tar + ".rdata"))
}


#### India TB report
d_itr_notif <- read_csv("../DataPublic/India TB report/ITR_Notif.csv")
d_itr_acf <- read_csv("../DataPublic/India TB report/ITR_ACF.csv")
d_itr_ct <- read_csv("../DataPublic/India TB report/ITR_CT.csv")
d_itr_tx <- read_csv("../DataPublic/India TB report/ITR_Tx.csv")

save(d_itr_notif, d_itr_acf, d_itr_ct, d_itr_tx, file = here::here("data_raw", "itr.rdata"))

