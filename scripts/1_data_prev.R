library(tidyverse)



load(here::here("data_raw", "tbps2011.rdata"))


prev_summary <- bind_rows(
  raw_tbps %>% 
    summarise(
      Group = "All",
      N = n(), 
      TB = sum(BacPos * (Tx == 0)),
      Prev_M = mean(BacPos * (Tx == 0)), 
      Prev_L = qbinom(0.025, N, Prev_M) / N,
      Prev_U = qbinom(0.975, N, Prev_M) / N),
  raw_tbps %>% 
    group_by(Group = Sex) %>% 
    summarise(
      N = n(), 
      TB = sum(BacPos * (Tx == 0)),
      Prev_M = mean(BacPos * (Tx == 0)), 
      Prev_L = qbinom(0.025, N, Prev_M) / N,
      Prev_U = qbinom(0.975, N, Prev_M) / N),
  raw_tbps %>% 
    group_by(Group = Agp) %>% 
    summarise(
      N = n(), 
      TB = sum(BacPos * (Tx == 0)),
      Prev_M = mean(BacPos * (Tx == 0)), 
      Prev_L = qbinom(0.025, N, Prev_M) / N,
      Prev_U = qbinom(0.975, N, Prev_M) / N),
  raw_tbps %>% 
    group_by(Group = Area) %>% 
    summarise(
      N = n(), 
      TB = sum(BacPos * (Tx == 0)),
      Prev_M = mean(BacPos * (Tx == 0)), 
      Prev_L = qbinom(0.025, N, Prev_M) / N,
      Prev_U = qbinom(0.975, N, Prev_M) / N)
)



d_prev <- bind_rows(
  raw_tbps %>% 
    group_by(State) %>% 
    summarise(Tag = "All", N_Subject = n(), N_Prev = sum(BacPos == 1 & Tx == 0)) %>% 
    mutate(N_Subject = sum(N_Subject), Prop = N_Prev / sum(N_Prev)) %>% 
    filter(N_Prev > 0),
  raw_tbps %>% 
    group_by(Tag = Area, State) %>% 
    summarise(N_Subject = n(), N_Prev = sum(BacPos == 1 & Tx == 0)) %>% 
    mutate(N_Subject = sum(N_Subject), Prop = N_Prev / sum(N_Prev)) %>% 
    filter(N_Prev > 0)
) %>% mutate(Year = 2011, Country = "India")



save(d_prev, prev_summary, file = here::here("data", "d_prev.rdata"))
