library(tidyverse)



d_tx <- read_csv("../Data/WHO/TB_outcomes.csv") %>% 
  filter(iso3 == "IND") %>% 
  filter(year >= 2014) %>% 
  select(Year = year, 
         Country = country,
         starts_with("mdr_"),
         starts_with("xdr_"),
         starts_with("newrel_")) %>% 
  pivot_longer(-c(Year, Country), values_to = "Count") %>% 
  extract(name, c("Tag", "Outcome"), "(newrel|mdr|xdr)_([a-z]+)$") %>% 
  filter(!is.na(Count)) %>% 
  filter(!is.na(Tag)) %>% 
  mutate(
    Tag = ifelse(Tag == "newrel", "All", "DR")
  ) %>% 
  (function(ds) {
    ds %>% filter(Outcome != "coh") %>% 
      left_join(ds %>% 
                  filter(Outcome == "coh") %>% 
                  select(Year, Country, TrI = Count, Tag))
  })() %>% 
  mutate(Outcome = case_when(
    Outcome %in% c("cmplt", "cur", "succ") ~ "Succ",
    Outcome == "died" ~ "Death",
    T ~ "LTFU"
  )) %>% 
  mutate(pr = Count / TrI) %>% 
  group_by(Country, Year, Outcome, Tag) %>%
  summarise(Count = sum(Count), TrI = max(TrI)) %>% 
  pivot_wider(c(Country, Year, Outcome), names_from = Tag, values_from = Count) %>% 
  mutate(DS = All - DR) %>% 
  pivot_longer(-c(Country, Year, Outcome), names_to = "Tag", values_to = "Count") %>% 
  group_by(Country, Year, Tag) %>% 
  mutate(N = sum(Count))


save(d_tx, file = here::here("data", "d_tx.rdata"))


d_tx_all <- d_tx %>% 
  filter(Tag == "All") %>% 
  mutate(
    Stat = paste0("TxOut", Outcome),
    value_M = Count / N,
  ) %>% 
  mutate(
    value_L = qbinom(0.025, N, value_M) / N,
    value_U = qbinom(0.975, N, value_M) / N
  ) %>% 
  select(Year, Tag, Stat, N, starts_with("value_"))



save(d_tx_all, file = here::here("data", "d_tx_all.rdata"))


d_tx_dr <- d_tx %>% 
  filter(Tag != "All") %>% 
  mutate(
    Stat = paste0("TxOut", Outcome),
    value_M = Count / N,
  ) %>% 
  mutate(
    value_L = qbinom(0.025, N, value_M) / N,
    value_U = qbinom(0.975, N, value_M) / N
  ) %>% 
  select(Year, Tag, Stat, N, starts_with("value_"))

save(d_tx_dr, file = here::here("data", "d_tx_dr.rdata"))
