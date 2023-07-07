library(tidyverse)



ds <- dir("data_raw")
ds <- ds[endsWith(ds, ".rdata")]

for (d in ds) {
  load(here::here("data_raw", d))
}

load(here::here("data", "t_arti_ru.rdata"))
load(here::here("data", "t_arti_rus.rdata"))


locs <- state_map$State

for (loc in locs) {
  if (loc == "India") {
    path <- loc
  } else {
    path <- glue::glue("State_") + loc
  }
  
  dir.create(here::here("inputs", path), showWarnings = F)
}


t_cases <- bind_rows(
  d_cases %>% 
    left_join(d_wpp %>% select(Year, N_Pop)) %>% 
    mutate(
      M = N_Case / N_Pop,
      State = "India",
      Index = "CNR"
    ) %>% 
    select(Year, Tag, State, Index, N = N_Pop, M),
  d_cases %>% 
    mutate(State = "India", Index = "PrDR") %>% 
    select(Year, Tag, State, Index, N = N_Case, M = PrDR),
  d_itr_notif %>% 
    mutate(
      Index = "PrDetPub",
      Tag = "All",
      N = (N_Noti_Pub + N_Noti_Pri),
      M = N_Noti_Pub / N
    ) %>% 
    select(Year, State, Index, Tag, N, M)
) %>% 
  filter(!is.na(M)) %>% 
  mutate(
    Std = sqrt(N * M * (1 - M)),
    Error = Std / sqrt(N),
    L = qbinom(0.025, N, M) / N,
    U = qbinom(0.975, N, M) / N
  )



save(t_cases, file = here::here("data", "t_cases.rdata"))


t_burden <- d_burden %>% 
  pivot_longer(starts_with(c("Inc", "Mor"))) %>% 
  separate(name, c("Index", "name"), "_") %>% 
  pivot_wider() %>% 
  mutate(Tag = "All") %>% 
  rename(State = Country) %>% 
  left_join(d_wpp %>% select(Year, N = N_Pop))%>% 
  mutate(
    Error = (U - L) / 2 / 1.96,
    Std = Error * sqrt(N),
    Index = paste0(Index, "R")
  )


d_burden_a <- d_burden_as %>% 
  group_by(Year, Country, Agp) %>% 
  summarise(across(c(M, L, U), sum)) %>% 
  mutate(Sex = "A") %>% 
  ungroup() %>% 
  mutate(
    Sex = "A",
    Agp = ifelse(Agp == "65plus", "65+", Agp)
  )


t_burden_a <- bind_rows(
  d_burden_a %>% 
    left_join(d_wpp_as) %>% 
    mutate(
      M = M / N,
      L = L / N,
      U = U / N,
      Error = (U - L) / 2 / 1.96,
      Std = Error * sqrt(N),
      Index = "IncR"
    ) %>% 
    select(Year, State = Country, Index, Tag = Agp, M:U, N, Error, Std),
  d_burden_a %>% 
    mutate(
      N = sum(M),
      M = M / N,
      L = qbinom(0.025, size = N, prob = M) / N,
      U = qbinom(0.975, size = N, prob = M) / N,
      Std = sqrt(N * M * (1 - M)),
      Error = Std / sqrt(N),
      Index = "PrInc"
    ) %>% 
    select(Year, State = Country, Index, Tag = Agp, M:U, N, Error, Std)
)


save(t_burden, file = here::here("data", "t_burden.rdata"))
save(t_burden_a, file = here::here("data", "t_burden_a.rdata"))

d_itr_acf


amp <- d_wpp %>% 
  filter(Year == 2019) %>% 
  mutate(
    amp = N_Pop0 / (O + M)
  ) %>% 
  pull(amp)


t_prev_state <- d_prev_asc_state %>% 
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
    M_PrTxiPub = Pr_Pub_TB
  ) %>% 
  select(State, Year, starts_with(c("S_", "M_"))) %>% 
  pivot_longer(starts_with(c("S_", "M_"))) %>% 
  extract(name, c("name", "Index"), "(S|M)_(\\S+)") %>% 
  pivot_wider() %>% 
  rename(N = S) %>% 
  mutate(Tag = "All", N = ceiling(N)) %>% 
  mutate(
    Std = sqrt(N * M * (1 - M)),
    Error = Std / sqrt(N),
    L = qbinom(0.025, N, M) / N,
    U = qbinom(0.975, N, M) / N
  )




save(t_prev_state, file = here::here("data", "t_prev_state.rdata"))



t_txi <- d_itr_notif %>% 
  left_join(state_map %>% select(State, Pop)) %>% 
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
  select(Year, State, Index, Tag, N, M) %>% 
  mutate(
    Std = sqrt(N * M * (1 - M)),
    Error = Std / sqrt(N),
    L = qbinom(0.025, N, M) / N,
    U = qbinom(0.975, N, M) / N
  )


save(t_txi, file = here::here("data", "t_txi.rdata"))


t_txo <- d_itr_tx %>% 
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
  select(Year, State, Index, Tag, N, M) %>% 
  mutate(
    Std = sqrt(N * M * (1 - M)),
    Error = Std / sqrt(N),
    L = qbinom(0.025, N, M) / N,
    U = qbinom(0.975, N, M) / N
  )

save(t_txo, file = here::here("data", "t_txo.rdata"))


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
  select(-Region, -X) %>% 
  mutate(
    Tag = "All"
  ) %>% 
  left_join(state_map %>% select(State, N = Pop)) %>% 
  mutate(
    Error = (U - L) / 2 / 1.96,
    Std = Error * sqrt(N),
    Index = "DrugTime"
  )

save(t_drugsale, file = here::here("data", "t_drugsale.rdata"))



targets_all <- bind_rows(
  t_burden,
  t_burden_a,
  t_cases,
  t_prev_state,
  t_txi,
  t_txo,
  t_arti_ru,
  t_drugsale
) %>% 
  filter(!is.na(M))

targets_all %>% filter(State == "India") %>% filter(Year > 2018) %>% data.frame()


save(targets_all, file = here::here("data", "targets_all.rdata"))
write_csv(targets_all, here::here("data", "targets_all.csv"))


for (loc in locs) {
  if (loc == "India") {
    path <- loc
  } else {
    path <- glue::glue("State_") + loc
  }
  
  targets <- targets_all %>% filter(State == loc)
  
  write_csv(targets, here::here("Inputs", path, "targets.csv"))
}

