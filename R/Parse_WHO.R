library(tidyverse)


## Burden estimates ----

db <- read_csv("../DataPublic/WHO/TB_burden_countries.csv") %>% 
  filter(iso3 == "IND")

d_burden <- db %>% 
  filter(iso3 == "IND") %>%
  select(
    Year = year, Country = country,
    Inc_M = e_inc_100k, Inc_L = e_inc_100k_lo, Inc_U = e_inc_100k_hi,
    Mor_M = e_mort_100k, Mor_L = e_mort_100k_lo, Mor_U = e_mort_100k_hi
  ) %>% 
  mutate(
    across(Inc_M:Mor_U, function(x) x * 1e-5)
  )


db <- read_csv("../DataPublic/WHO/TB_burden_age_sex.csv") %>% 
  filter(iso3 == "IND")

d_burden_as <- db %>% 
  filter(risk_factor == "all") %>% 
  filter(sex != "a") %>% 
  filter(!(age_group %in% c("0-4", "5-14", "15plus", "all"))) %>% 
  select(
    Year = year, Country = country,
    M = best, L = lo, U = hi, Sex = sex, Agp = age_group
  )



save(d_burden, file = here::here("data_raw", "WHO_Burden.rdata"))
save(d_burden_as, file = here::here("data_raw", "WHO_Burden_as.rdata"))



## Case notification ----

db <- read_csv("../DataPublic/WHO/TB_notifications.csv") %>% 
  filter(iso3 == "IND")


cases <- db %>% 
  select(Country = country, Year = year, matches("newrel_(f|m)014"), matches("newrel_(f|m)15plus")) %>% 
  pivot_longer(-c(Country, Year), values_to = "Case") %>% 
  extract(name, c("Sex", "Agp"), "newrel_(\\w)(\\S+)") %>% 
  mutate(Sex = ifelse(Sex == "f", "Female", "Male"),
         Agp = ifelse(Agp == "014", "[0,15)", "[15,Inf)")) %>% 
  filter(!is.na(Case))


mdr <- db %>% 
  select(Country = country, Year = year, MDR = conf_rrmdr) %>% 
  filter(!is.na(MDR))


amp <- db %>% 
  select(Country = country, Year = year, matches("newrel_(f|m)014"), matches("newrel_(f|m)15plus"), new_ep) %>% 
  mutate(
    case_all = rowSums(across(starts_with("newrel"))),
    case_u = rowSums(across(ends_with("15plus"))),
    Amp_age = case_all / case_u,
    Amp_ep = case_all / (case_all - new_ep),
    Amp_cases = Amp_age * Amp_ep
  )  %>% 
  filter(!is.na(case_all)) %>% 
  select(Year, Country, starts_with("Amp"))


d_cases <- cases %>% 
  filter(Country == "India") %>% 
  group_by(Year, Country) %>% 
  summarise(
    Tag = "All",
    N_Case = sum(Case)
  ) %>% ungroup() %>% 
  left_join(amp) %>% 
  left_join(mdr) %>% 
  mutate(
    PrDR = MDR / N_Case
  )


save(d_cases, file = here::here("data_raw", "WHO_Cases.rdata"))


## Case notification, Q ----

db <- read_csv("../DataPublic/WHO/TB_provisional_notifications.csv") %>% 
  filter(iso3 == "IND")



d_cases_month <- db %>% 
  select(Country = country, Year = year, starts_with("m_")) %>% 
  pivot_longer(starts_with("m_")) %>% 
  extract(name, "month", "m_(\\d+)") %>% 
  mutate(
    month = as.numeric(month),
    q = ceiling(month / 3),
    Time = Year + (month - 1) / 12 + 1 / 24
  )


save(d_cases_month, file = here::here("data_raw", "WHO_Cases_Month.rdata"))


## Treatment outcome ----

db <- read_csv("../DataPublic/WHO/TB_outcomes.csv") %>% 
  filter(iso3 == "IND") %>% 
  filter(year >= 2014)


d_tx <- db %>% 
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



save(d_tx, file = here::here("data_raw", "WHO_TxOut.rdata"))


d_tx_all <- d_tx %>% 
  filter(Tag == "All") %>% 
  mutate(
    Stat = paste0("TxOut", Outcome),
    M = Count / N,
  ) %>% 
  mutate(
    L = qbinom(0.025, N, M) / N,
    U = qbinom(0.975, N, M) / N
  ) %>% 
  select(Year, Tag, Stat, N, M, L, U)



save(d_tx_all, file = here::here("data_raw", "WHO_TxOut_all.rdata"))


d_tx_dr <- d_tx %>% 
  filter(Tag != "All") %>% 
  mutate(
    Stat = paste0("TxOut", Outcome),
    M = Count / N,
  ) %>% 
  mutate(
    L = qbinom(0.025, N, M) / N,
    U = qbinom(0.975, N, M) / N
  ) %>% 
  select(Year, Tag, Stat, N, M, L, U)

save(d_tx_dr, file = here::here("data_raw", "WHO_TxOut_dr.rdata"))


## DR surv

db <- read_csv("../DataPublic/WHO/TB_dr_surveillance_2023-08-18.csv") %>% 
  filter(iso3 == "IND") %>% 
  filter(year >= 2014)


db


