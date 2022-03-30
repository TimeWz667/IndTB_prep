library(tidyverse)


load(here::here("data_raw", "report.rdata"))


d_report <- report %>% 
  mutate(
    Amp_Private = (17 / 9.3) / 1.5, # from drug sale data
    N_Case_ACF = N_Case_acf,
    N_Case_Public = N_Case_public - N_Case_ACF,
    Pr_Case_ACF = N_Case_ACF / N_Case,
    Pr_Case_Public = N_Case_Public / N_Case,
    Pr_Case_Private = 1 - Pr_Case_ACF - Pr_Case_Public,
    N_Det_Private = N_Case_Public * Amp_Private,
    Engage = N_Case_private / N_Det_Private, 
    N_Det = N_Case_ACF + N_Case_Public + N_Det_Private,
    Pr_Det_ACF = N_Case_ACF / N_Det,
    Pr_Det_Public = N_Case_Public / N_Det,
    Pr_Det_Private = 1 - Pr_Det_ACF - Pr_Det_Public,
  ) %>% 
  select(Year, Amp_Private, Engage, starts_with("Pr_"))


d_report


save(d_report, file = here::here("data", "d_report.rdata"))

