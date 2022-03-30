library(tidyverse)
library(jsonlite)


folder <- "serve/Vanilla"
dir.create(folder, showWarnings = F)


### Calibration targets

load(here::here("data", "d_pop_all.rdata"))
load(here::here("data", "d_prev_adj_rus.rdata"))
load(here::here("data", "d_burden_all.rdata"))
load(here::here("data", "d_tx.rdata"))
load(here::here("data", "d_cases_all.rdata"))
load(here::here("data", "d_report.rdata"))
load(here::here("data", "d_arti_rus.rdata"))

load(here::here("pars", "pars_demo_all.rdata"))
load(here::here("pars", "pars_cascade_all.rdata"))




calc_mlu <- function(df, pci = 0.95) {
  df %>% 
    mutate(
      M = X / N,
      L = qbinom((1 - pci) / 2, size = round(N), prob = M) / N,
      U = qbinom(1 - (1 - pci) / 2, size = round(N), prob = M) / N
    )
}


# Validation

Valid <- bind_rows(
  d_pop_all %>% 
    filter(Year >= 2010 & Year <= 2021) %>% 
    mutate(Index = "Pop") %>% 
    select(Year, Tag, Index, M = N_Pop) %>% 
    mutate(L = M, U = M),
  d_prev_adj_rus %>% 
    filter(Tag == "All") %>% 
    mutate(
      Index = paste0("Prev", State)
    ) %>% 
    rename(X = N_Prev, N = N_Subject) %>% 
    calc_mlu() %>% 
    select(Year, Tag, Index, M, L, U),
  d_prev_adj_rus %>% 
    filter(Tag == "All") %>% 
    group_by(Tag, Year, Country) %>% 
    summarise(
      X = sum(N_Prev),
      N = mean(N_Subject)
    ) %>% 
    mutate(
      Index = "Prev"
    ) %>% 
    calc_mlu() %>% 
    select(Year, Tag, Index, M, L, U) %>% 
    ungroup(),
  d_prev_adj_rus %>% 
    filter(Tag == "All") %>% 
    group_by(Year) %>% 
    mutate(
      X = N_Prev,
      N = sum(X),
      Index = paste0("Pr", State)
    ) %>% 
    calc_mlu() %>% 
    select(Year, Tag, Index, M, L, U) %>% 
    ungroup(),
  d_burden %>% 
    pivot_longer(-c(Year, Country)) %>% 
    separate(name, c("Index", "Stat")) %>% 
    pivot_wider(c(Year, Index), names_from = Stat, values_from = value) %>% 
    mutate(Tag = "All"),
  d_tx %>% 
    select(Year, Tag, Index = Outcome, N, X = Count) %>% 
    calc_mlu() %>% 
    ungroup() %>% 
    select(- Country, - N, - X),
  d_case_all %>% 
    select(- starts_with("Amp")) %>% 
    left_join(d_report) %>% 
    left_join(d_pop_all) %>% 
    rename(N = N_Pop) %>% 
    mutate(
      N_CasePublic = N_Case * Pr_Case_Public,
      N_CaseACF = N_Case * Pr_Case_ACF,
      N_CasePrivate = N_Case - N_CasePublic - N_CaseACF,
      N_DetPublic = N_CasePublic,
      N_DetACF = N_CaseACF,
      N_DetPrivate = N_DetPublic * Amp_Private,
      N_Det = N_DetPublic + N_DetPrivate + N_DetACF
    ) %>% 
    select(Year, Tag, starts_with("N"), N_Case) %>% 
    pivot_longer(-c(Year, Tag, N), values_to = "X") %>% 
    extract(name, "Index", "N_(\\w+)") %>% 
    calc_mlu()%>% 
    select(- N, - X),
  d_case_all %>% 
    filter(Year >= 2018 & !is.na(MDR)) %>% 
    select(Year, Tag, X = MDR, N = N_Case) %>% 
    mutate(Index = "PrMDR") %>% 
    calc_mlu() %>% 
    select(- N, - X),
  d_arti_rus %>% filter(Tag == "All")
) %>% 
  arrange(Tag, Index, Year) %>% 
  filter(!is.na(M))





# Outputs

write_csv(Valid, here::here(folder, "Valid.csv"))

Targets <- Valid %>% 
  filter(Index %in% c("ARTI", "Prev", "Inc", "Mor", "PrMDR")) %>% 
  select(Year, Index, Tag, M)

write_csv(Targets, here::here(folder, "Targets.csv"))


list(
  All = Targets %>% 
    filter(Tag == "All")
) %>% 
  jsonlite::write_json(here::here(folder, "Targets.json"), auto_unbox = T, digits = 10)



list(
  Demo = pars_demo_all,
  Cascade = pars_cascade_all
) %>% 
  jsonlite::write_json(here::here(folder, "pars.json"), auto_unbox = T, digits = 10)


