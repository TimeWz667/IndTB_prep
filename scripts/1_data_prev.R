library(tidyverse)




### India Prevalence survey -----
df <- read_csv("../Data/Gujarat TBPS/TBPSGU2011.csv") %>% 
  filter(Reasonforexclusion == "Not applicable" & Particpation == "Yes")


df <- df %>%  
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
  )# %>% 
  #select(Age, Agp, Sex, Area = Stratum, BacPos, Sym, CareSeeking, Tx, Cascade)


prev_summary <- bind_rows(
  df %>% 
    summarise(
      Group = "All",
      N = n(), 
      TB = sum(BacPos * (Tx == 0)),
      Prev_M = mean(BacPos * (Tx == 0)), 
      Prev_L = qbinom(0.025, N, Prev_M) / N,
      Prev_U = qbinom(0.975, N, Prev_M) / N),
  df %>% 
    group_by(Group = Sex) %>% 
    summarise(
      N = n(), 
      TB = sum(BacPos * (Tx == 0)),
      Prev_M = mean(BacPos * (Tx == 0)), 
      Prev_L = qbinom(0.025, N, Prev_M) / N,
      Prev_U = qbinom(0.975, N, Prev_M) / N),
  df %>% 
    group_by(Group = Agp) %>% 
    summarise(
      N = n(), 
      TB = sum(BacPos * (Tx == 0)),
      Prev_M = mean(BacPos * (Tx == 0)), 
      Prev_L = qbinom(0.025, N, Prev_M) / N,
      Prev_U = qbinom(0.975, N, Prev_M) / N),
  df %>% 
    group_by(Group = Area) %>% 
    summarise(
      N = n(), 
      TB = sum(BacPos * (Tx == 0)),
      Prev_M = mean(BacPos * (Tx == 0)), 
      Prev_L = qbinom(0.025, N, Prev_M) / N,
      Prev_U = qbinom(0.975, N, Prev_M) / N)
)



d_prev <- bind_rows(
  df %>% 
    group_by(State) %>% 
    summarise(Tag = "All", N_Subject = n(), N_Prev = sum(BacPos == 1 & Tx == 0)) %>% 
    mutate(N_Subject = sum(N_Subject), Prop = N_Prev / sum(N_Prev)) %>% 
    filter(N_Prev > 0),
  df %>% 
    group_by(Tag = Area, State) %>% 
    summarise(N_Subject = n(), N_Prev = sum(BacPos == 1 & Tx == 0)) %>% 
    mutate(N_Subject = sum(N_Subject), Prop = N_Prev / sum(N_Prev)) %>% 
    filter(N_Prev > 0)
) %>% mutate(Year = 2016, Country = "India")



save(d_prev, prev_summary, file = here::here("data", "d_prev.rdata"))
