library(tidyverse)
library(jsonlite)


folder <- "serve/CHAI"
dir.create(folder, showWarnings = F)


### Calibration targets

load(here::here("data", "d_pop_all.rdata"))
load(here::here("data", "d_pop_ru.rdata"))

load(here::here("data", "d_tbps_asc_RU.rdata"))
load(here::here("data", "d_burden_all.rdata"))
load(here::here("data", "d_tx.rdata"))
load(here::here("data", "d_itr.rdata"))
load(here::here("data", "d_arti_ru.rdata"))

load(here::here("pars", "pars_demo_ru.rdata"))



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
  d_pop_ru %>% 
    filter(Year >= 2010 & Year <= 2021) %>% 
    mutate(Index = "Pop") %>% 
    select(Year, Tag, Index, M = N_Pop) %>% 
    mutate(L = M, U = M),
  d_prev_asc_ru %>% 
    mutate(Year = 2020) %>% 
    select(Year, Tag = Type, Index = Gp, M, L, U) %>% 
    mutate(
      Index = case_when(
        Index == "TB" ~ "Prev",
        Index == "TBLike" ~ "PrevSym",
        T ~ paste0("Prop", Index)
      )
    ),
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
  d_pop_all %>% select(Year, Tag, N = N_Pop) %>% 
    inner_join(
      d_itr_notif %>% filter(State == "India") %>% 
        select(Year, CNR_Pub = N_Noti_Pub, CNR_Pri = N_Noti_Pri)) %>% 
    inner_join(
      d_itr_acf %>% filter(State == "India") %>% 
        select(Year, CNR_ACF = N_ACF_Detected) %>% 
        mutate(CNR_ACF = ifelse(CNR_ACF <= 0, mean(CNR_ACF[CNR_ACF > 0]), CNR_ACF))) %>% 
    mutate(CNR_Pub = CNR_Pub - CNR_ACF) %>% 
    pivot_longer(starts_with("CNR_"), values_to = "X", names_to = "Index") %>% 
    calc_mlu() %>% 
    select(- N, - X),
  d_arti_ru
) %>% 
  arrange(Tag, Index, Year) %>% 
  filter(!is.na(M))



# Outputs

write_csv(Valid, here::here(folder, "Valid.csv"))

Targets <- Valid %>% 
  filter(Index %in% c("ARTI", "Prev", "PrevSym", "PropAsym", "PropExCS", "CNR_Pub", "CNR_Pri", "CNR_ACF", "Inc", "Mor")) %>% 
  select(Year, Index, Tag, M)

write_csv(Targets, here::here(folder, "Targets.csv"))


list(
  All = Targets %>% 
    filter(Tag == "All"),
  Rural = Targets %>% 
    filter(Tag == "Rural"),
  Urban = Targets %>% 
    filter(Tag == "Urban")
) %>% 
  jsonlite::write_json(here::here(folder, "Targets.json"), auto_unbox = T, digits = 10)



jsonlite::write_json(pars_demo_ru, here::here(folder, "pars_ru.json"), auto_unbox = T, digits = 10)


