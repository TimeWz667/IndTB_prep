library(tidyverse)



ds <- dir("data_raw")
ds <- ds[endsWith(ds, ".rdata")]

for (d in ds) {
  load(here::here("data_raw", d))
}


locs <- unique(state_map$Region)
locs <- locs[locs != "India"]

for (loc in locs) {
  dir.create(here::here("inputs", glue::glue("Region_") + loc), showWarnings = F)
}



t_cases <- d_itr_notif %>% 
  left_join(state_map %>% select(State = State_Itr, Region)) %>% 
  select(-State) %>% 
  group_by(Year, Region) %>% 
  summarise(across(everything(), sum)) %>% 
  mutate(
    Index = "PrDetPub",
    Tag = "All",
    N = (N_Noti_Pub + N_Noti_Pri),
    M = N_Noti_Pub / N
  ) %>% 
  select(Year, Region, Index, Tag, N, M) %>% 
  filter(!is.na(M)) %>% 
  mutate(
    Std = sqrt(N * M * (1 - M)),
    Error = Std / sqrt(N),
    L = qbinom(0.025, N, M) / N,
    U = qbinom(0.975, N, M) / N
  )


amp <- d_wpp %>% 
  filter(Year == 2019) %>% 
  mutate(
    amp = N_Pop0 / (O + M)
  ) %>% 
  pull(amp)


t_prev_region <- d_prev_asc_region %>% 
  mutate(
    Year = 2019,
    S_PrevUt = N * amp,
    S_PrevAsym = S_PrevUt,
    S_PrevSym = S_PrevUt,
    S_PrevExCS = S_PrevUt,
    M_PrevAsym = N_Asym / N * AmpAll / amp,
    M_PrevSym = N_Sym / N * AmpAll / amp,
    M_PrevExCS = N_ExCS / N * AmpAll / amp,
    M_PrevUt = M_PrevAsym + M_PrevSym + M_PrevExCS,
    S_PrAsym = (N_Asym + N_Sym + N_ExCS) * AmpAll,
    S_PrSym = (N_Asym + N_Sym + N_ExCS) * AmpAll,
    S_PrExCS = (N_Asym + N_Sym + N_ExCS) * AmpAll,
    M_PrAsym = N_Asym / (N_Asym + N_Sym + N_ExCS),
    M_PrSym = N_Sym / (N_Asym + N_Sym + N_ExCS),
    M_PrExCS = N_ExCS / (N_Asym + N_Sym + N_ExCS),
    S_TBLikeUt = N,
    M_TBLikeUt = N_TBLikeUt / N,
    S_PrCSIPub = S_PrevUt,
    M_PrCSIPub = Pr_Pub_CSI,
    S_PrTxiPub = N_OnATT_Pub + N_OnATT_Pri,
    M_PrTxiPub = Pr_Pub_TB,
    S_PrPastPub = N_Past_Pub + N_Past_Pri,
    M_PrPastPub = Pr_Pub_Past
  ) %>% 
  select(Region, Year, starts_with(c("S_", "M_"))) %>% 
  pivot_longer(starts_with(c("S_", "M_"))) %>% 
  extract(name, c("name", "Index"), "(S|M)_(\\S+)") %>% 
  pivot_wider() %>% 
  rename(N = S) %>% 
  mutate(Tag = "All", N = ceiling(N)) %>% 
  mutate(
    Std = sqrt(N * M * (1 - M)),
    Error = Std / N,
    L = qbinom(0.025, N, M) / N,
    U = qbinom(0.975, N, M) / N
  )


save(t_prev_region, file = here::here("data", "t_prev_region.rdata"))


t_txi <- d_itr_notif %>% 
  left_join(state_map %>% select(State, Region, Pop)) %>% 
  select(-State) %>% 
  group_by(Year, Region) %>% 
  summarise(across(everything(), sum, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(
    TxI_Pub = N_Txi_Pub / N_Noti_Pub,
    TxI_Pri = N_Txi_Pri / N_Noti_Pri
  ) %>% 
  pivot_longer(starts_with(c("TxI_")), values_to = "M") %>% 
  mutate(
    N = case_when(
      endsWith(name, "Pub") ~ N_Noti_Pub,
      T ~ N_Noti_Pri
    )
  ) %>% 
  separate(name, c("Index", "Tag"), "_") %>% 
  select(Year, Region, Index, Tag, N, M) %>% 
  filter(M > 0) %>% 
  mutate(
    Std = sqrt(N * M * (1 - M)),
    Error = Std / sqrt(N),
    L = qbinom(0.025, N, M) / N,
    U = qbinom(0.975, N, M) / N,
    Std = ifelse(is.na(Std), 0, Std),
    Error = ifelse(is.na(Error), 0, Error)
  )


t_txo <- d_itr_tx %>% 
  left_join(state_map %>% select(State, Region, Pop)) %>% 
  select(-State) %>% 
  group_by(Year, Region) %>% 
  summarise(across(everything(), sum)) %>% 
  ungroup() %>% 
  pivot_longer(starts_with(c("N_Tx_Succ", "N_Tx_Die"))) %>% 
  mutate(
    N = case_when(
      endsWith(name, "_Pub") ~ N_Tx_Ini_Pub,
      T ~ N_Tx_Ini_Pri
    )
  ) %>% 
  extract(name, c("Index", "Tag"), "N_Tx_(\\w+)_(\\w+)") %>% 
  mutate(
    M = value / N,
    Index = paste0("Tx", Index)
  ) %>% 
  select(Year, Region, Index, Tag, N, M) %>% 
  mutate(
    Std = sqrt(N * M * (1 - M)),
    Error = Std / sqrt(N),
    L = qbinom(0.025, N, M) / N,
    U = qbinom(0.975, N, M) / N
  )


t_drugsale <- bind_rows(
  d_drug_ts %>% 
    filter(State != "North_East"),
  state_map %>% 
    filter(Region == "Northeastern") %>% 
    select(State, Region) %>% 
    left_join(d_drug_ts %>% 
                filter(State == "North_East") %>% 
                mutate(Region = "Northeastern") %>% select(-State))
) %>% 
  mutate(
    Tag = "All"
  ) %>% 
  left_join(state_map %>% select(State, Region, N = Pop)) %>% 
  mutate(
    Error = (U - L) / 2 / 1.96,
    Std = Error * sqrt(N),
    Index = "DrugTime"
  ) %>% 
  group_by(Region, Tag, Year, Index) %>% 
  summarise(
    M = weighted.mean(M, N),
    Std = sqrt(sum(Std ^ 2 * N) / sum(N)),
    N = sum(N)
  ) %>% 
  mutate(
    Error = Std / sqrt(N),
    U = M + Error * 1.96,
    L = M - Error * 1.96
  )


targets_all <- bind_rows(
  t_prev_region,
  t_txi,
  t_txo,
  t_drugsale
) %>% 
  filter(!is.na(M))


for (loc in locs) {
  targets <- targets_all %>% filter(Region == loc)
  write_csv(targets, here::here("Inputs", glue::glue("Region_") + loc, "targets.csv"))
}
