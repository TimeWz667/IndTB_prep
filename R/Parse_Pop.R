library(tidyverse)



## Fetch population data

d_wpp <- read_csv("../DataPublic/WPP/WPP2022_Demographic_Indicators_Medium.csv")


d_wpp <- d_wpp %>% filter(ISO3_code == "IND") %>% 
  select(Year = Time, 
         N_Pop = TPopulation1July, R_Dea = CDR, R_Bir = CBR, R_Mig = CNMR,
         PopDensity, PopSexRatio, MedianAgePop, LE = LEx) %>% 
  mutate(
    N_Pop = N_Pop * 1000,
    R_Dea = R_Dea / 1000,
    R_Bir = R_Bir / 1000
  )


d_wpp_jan <- read_csv("../DataPublic/WPP/WPP2022_Population1JanuaryByAge5GroupSex_Medium.csv") %>% 
  filter(ISO3_code == "IND")


d_wpp_as <- d_wpp_jan %>% 
  filter(Time >= 2020) %>% 
  select(Year = Time, AgeGrpStart, N = PopTotal, N_F = PopFemale, N_M = PopMale) %>% 
  mutate(
    N_F = N_F * 1000,
    N_M = N_M * 1000,
    N = N_F + N_M,
    Agp = case_when(
      AgeGrpStart < 15 ~ "0-14",
      AgeGrpStart < 25 ~ "15-24",
      AgeGrpStart < 35 ~ "25-34",
      AgeGrpStart < 45 ~ "35-44",
      AgeGrpStart < 55 ~ "45-54",
      AgeGrpStart < 65 ~ "55-64",
      T ~ "65+"
    )
  ) %>% 
  group_by(Year, Agp) %>% 
  summarise(
    N_A = sum(N),
    N_F = sum(N_F),
    N_M = sum(N_M)
  ) %>% 
  pivot_longer(-c(Year, Agp), values_to = "N") %>% 
  extract(name, "Sex", "N_(\\w+)")


d_wpp_jan <- d_wpp_jan %>% 
  select(Year = Time, AgeGrpStart, N = PopTotal) %>% 
  mutate(
    N = N * 1000,
    YMO = case_when(
      AgeGrpStart < 15 ~ "Y",
      AgeGrpStart < 65 ~ "M",
      T ~ "O"
    )
  ) %>% 
  group_by(Year, YMO) %>% 
  summarise(N = sum(N)) %>% 
  pivot_wider(names_from = YMO, values_from = N) %>% 
  ungroup() %>% 
  mutate(
    N_Pop0 = Y + M + O
  )


d_wpp <- d_wpp %>% 
  left_join(d_wpp_jan) %>% 
  filter(!is.na(N_Pop))

save(d_wpp_as, file = here::here("data_raw", "WPP_Pop_AgeSex.rdata"))

save(d_wpp, file = here::here("data_raw", "WPP_Pop.rdata"))




d_wup <- readxl::read_xls("../DataPublic/WUP/WUP2018-F21-Proportion_Urban_Annual.xls", skip = 16) %>% 
  select(Country = `Region, subregion, country or area`, `1960`:`2050`) %>% 
  filter(Country == "India") %>% 
  pivot_longer(-Country, names_to = "Year", values_to = "PrUrban") %>% 
  mutate(
    PrUrban = PrUrban / 100
  )

save(d_wup, file = here::here("data_raw", "WUP_PrUrban.rdata"))



state_map1 <- read_csv(here::here("data_raw", "StateMap1.csv"))
state_map2 <- read_csv(here::here("data_raw", "StateMap2.csv"))
state_map3 <- read_csv(here::here("data_raw", "StateMap3.csv"))

state_map <- state_map1 %>% 
  left_join(state_map2) %>% 
  left_join(state_map3) %>% 
  relocate(State, State_Pop, State_Itr, StateGroup, Region) %>% 
  mutate(
    State_Itr = gsub("\\s+", "_", State_Itr),
    State_Itr = gsub("&", "and", State_Itr),
    State = gsub("\\s+", "_", State),
    State = gsub("&", "and", State)
  )


save(state_map, file = here::here("data_raw", "state_map.rdata"))
