
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

