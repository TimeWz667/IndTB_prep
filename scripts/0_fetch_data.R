library(tidyverse)
library(dpdt)
library(glue)
library(here)


dir.create("data_raw/", showWarnings = F)



#### Demography


raw_pop <- dpdt::fetch_demography("ByCountry/India")

save(raw_pop, file = here("data_raw", "wpp2019.rdata"))


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
  assign(obj, read_csv("../PublicData/WHO/" + src + ".csv") %>% 
           filter(iso3 == "IND") %>% 
           filter(year > 2013))
 
  save(list = obj, file = here("data_raw", tar + ".rdata"))
}


#### India TB report

raw_report <- read_csv("../PublicData/India TB report/TB_india_report.csv")

save(raw_report, file = here("data_raw", "report.rdata"))



#### TBPS

raw_tbps <- read_csv("../PublicData/Gujarat TBPS/TBPSGU2011.csv") %>% 
  filter(Reasonforexclusion == "Not applicable" & Particpation == "Yes")


raw_tbps <- raw_tbps %>%  
  mutate(
    Agp = cut(Age, c(15, 25, 35,45, 55, 65, Inf), right = F),
    BacPos = case_when(
      FinalDiagnosis == "Smear positive TB" ~ 1,
      FinalDiagnosis == "Smear Negative / Culture positive TB" ~ 1,
      T ~ 0
    ),
    Sym = (TBsymptoms == "Yes") + 0,
    CareSeeking = case_when(
      IfcurrentTBsuspectthenmedicalcare == "Sought consultation / care" ~ "Sought",
      IfcurrentTBsuspectthenmedicalcare == "Self treated" ~ "Aware",
      IfcurrentTBsuspectthenmedicalcare %in% c("Ignored symptoms", "Not recognized as illness") ~ "Unaware",
      T ~ IfcurrentTBsuspectthenmedicalcare
    ),
    Tx = (CurrentTBTreatment == "Yes") + 0,
    Agp = cut(Age, c(15, 25, 35,45, 55, 65, Inf), right = F),
    State = case_when(
      CurrentTreatmentunderRNTCP == "Yes" ~ "TxPub", 
      CurrentTreatmentunderRNTCP == "No" ~ "TxPri",
      CareSeeking == "Sought" ~ "CS",
      CareSeeking == "Aware" ~ "CS",
      Sym == 1 ~ "Sym",
      BacPos == 1 ~ "Asym",
      T ~ "NonTB"
    ),
    State = factor(State, c("NonTB", "Asym", "Sym", "CS", "TxPub", "TxPri")),
    Area = ifelse(Stratum == "Urban", "Urban", "Rural")
  )


save(raw_tbps, file = here("data_raw", "tbps2011.rdata"))

