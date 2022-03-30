library(tidyverse)
library(jsonlite)


## Pr urban slum
pr_s <- 0.35



## Load data
load(here::here("data_raw", "wpp2019.rdata"))
load(here::here("data_raw", "wup2018.rdata"))

raw_pop

pop <- bind_rows(
  raw_pop$PopN_F %>% 
    pivot_longer(-Time, names_to = "Age", values_to = "N") %>% 
    left_join(raw_pop$DeaR_F %>% 
                pivot_longer(-Time, names_to = "Age", values_to = "dr")) %>% 
    mutate(Sex = "Female"),
  raw_pop$PopN_M %>% 
    pivot_longer(-Time, names_to = "Age", values_to = "N") %>% 
    left_join(raw_pop$DeaR_M %>% 
                pivot_longer(-Time, names_to = "Age", values_to = "dr")) %>% 
    mutate(Sex = "Male")
) %>% 
  mutate(Age = as.numeric(Age)) %>% 
  group_by(Year = Time) %>% 
  summarise(
    N_Pop = sum(N),
    N_Pop_Y = sum(N * (Age < 15)),
    N_Pop_U = sum(N * (Age >=15)),
    R_Die = sum(N * dr) / N_Pop,
    R_Age = sum(N * (Age == 14)) / N_Pop_Y,
    Amp_adult = N_Pop / N_Pop_U
  ) %>% 
  ungroup()
  


## Urban / Rural
pr_ru <- raw_wup %>% 
  mutate(
    Year = as.numeric(Year),
    PrUrban = PrUrban / 100,
    PrRural = 1 - PrUrban
  )

pr_rus <- raw_wup %>% 
  mutate(
    Year = as.numeric(Year),
    PrUrban = PrUrban / 100,
    PrSlum = PrUrban * pr_s,
    PrUrban = PrUrban - PrSlum,
    PrRural = 1 - PrUrban - PrSlum
  )


## Output

d_pop_all <- pop %>% 
  mutate(Tag = "All", Country = "India") %>% 
  select(Year, Country, Tag, N_Pop, N_Pop_Y, N_Pop_U, R_Die, R_Age, Amp_adult)


d_pop_ru <- pop %>% inner_join(pr_ru) %>% 
  mutate(
    N_Pop_Rural = N_Pop * PrRural,
    N_Pop_Urban = N_Pop * PrUrban
  ) %>% 
  select(Year, Country, R_Die, R_Age, N_Pop_Rural, N_Pop_Urban, Amp_adult) %>% 
  pivot_longer(c(N_Pop_Urban, N_Pop_Rural), values_to = "N_Pop") %>% 
  extract(name, "Tag", "N_Pop_(\\w+)") %>% 
  select(Year, Country, Tag, N_Pop, R_Die, R_Age, Amp_adult)


d_pop_rus <- pop %>% inner_join(pr_rus) %>% 
  mutate(
    N_Pop_Rural = N_Pop * PrRural,
    N_Pop_Urban = N_Pop * PrUrban,
    N_Pop_Slum = N_Pop * PrSlum
  ) %>% 
  select(Year, Country, R_Die, R_Age, N_Pop_Rural, N_Pop_Urban, N_Pop_Slum, Amp_adult) %>% 
  pivot_longer(c(N_Pop_Urban, N_Pop_Rural, N_Pop_Slum), values_to = "N_Pop") %>% 
  extract(name, "Tag", "N_Pop_(\\w+)") %>% 
  select(Year, Country, Tag, N_Pop, R_Die, R_Age, Amp_adult)



  
  
save(d_pop_all, file = here::here("data", "d_pop_all.rdata"))
save(d_pop_ru, file = here::here("data", "d_pop_ru.rdata"))
save(d_pop_rus, file = here::here("data", "d_pop_rus.rdata"))
