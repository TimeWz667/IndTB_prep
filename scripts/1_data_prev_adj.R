library(tidyverse)


#### Goals
### - 



load(here::here("data", "d_prev.rdata"))
load(here::here("data", "d_pop_all.rdata"))
load(here::here("data", "d_pop_ru.rdata"))
load(here::here("data", "d_pop_rus.rdata"))
load(here::here("data", "d_cases_all.rdata"))
load(here::here("data", "d_burden_all.rdata"))



year_baseline = 2014
year_target = 2019



d_prev_adj_ru <- local({
  
  adr <- mean(- diff(log(d_burden$Inc_M)))
  
  d_p <- d_prev %>% 
    mutate(YearBase = year_baseline) %>% 
    left_join(d_case_all %>% select(YearBase = Year, Amp_all)) %>% 
    mutate(X = N_Prev * Amp_all, N = N_Subject) %>% 
    select(- YearBase) %>% 
    left_join(bind_rows(
      d_pop_all,
      d_pop_ru
    )) %>% 
    mutate(N = round(N * Amp_adult)) %>% 
    select(- R_Die, - Amp_adult, - N_Pop_Y, - N_Pop_U, - R_Age) %>% 
    mutate(
      X = X * exp(- adr * (year_target - Year)),
      Year = year_target
    ) %>% 
    group_by(Tag) %>% 
    mutate(
      Prop = X / sum(X)
    ) %>% 
    select(- N_Subject, - N_Prev) %>% 
    rename(
      N_Subject = N,
      N_Prev = X
    ) %>% 
    select(State, Tag, Year, Country, N_Subject, N_Prev, Prop)
})


d_prev_adj_rus <- local({
  
  adr <- mean(- diff(log(d_burden$Inc_M)))
  
  d_p <- d_prev %>% 
    mutate(YearBase = year_baseline) %>% 
    left_join(d_case_all %>% select(YearBase = Year, Amp_all)) %>% 
    mutate(X = N_Prev * Amp_all, N = N_Subject) %>% 
    select(- YearBase)
  
  
  d_p <- bind_rows(
    d_p, 
    d_p %>% filter(Tag == "Urban") %>% mutate(Tag = "Slum")
  ) %>% 
    left_join(bind_rows(
      d_pop_all,
      d_pop_rus
    )) %>% 
    mutate(N = round(N * Amp_adult)) %>% 
    select(- R_Die, - Amp_adult) %>% 
    group_by(State) %>% 
    mutate(
      N_U = sum(N_Pop * (Tag %in% c("Urban", "Slum"))),
      N = case_when(
        Tag %in% c("Urban", "Slum") ~ N * N_Pop / N_U,
        T ~ N
      ),
      X0 = case_when(
        Tag == "Urban" ~ N,
        Tag == "Slum" ~ 3 / 2 * N,
        T ~ 1
      ),
      X0 = case_when(
        Tag %in% c("Urban", "Slum") ~ X0 / sum(X0 * (Tag %in% c("Urban", "Slum"))),
        T ~ 1
      ),
      X = X * X0 * exp(- adr * (year_target - Year)),
      Year = year_target
    ) %>% 
    ungroup() %>% 
    group_by(Tag) %>% 
    mutate(
      Prop = X / sum(X)
    ) %>% 
    select(- N_U, - X0, - N_Subject, - N_Prev) %>% 
    rename(
      N_Subject = N,
      N_Prev = X
    ) %>% 
    select(State, Tag, Year, Country, N_Subject, N_Prev, Prop)
})



d_prev %>% 
  group_by(Tag) %>% 
  summarise(prev = sum(N_Prev) / N_Subject[1])

d_prev_adj_ru %>% 
  group_by(Tag) %>% 
  summarise(prev = sum(N_Prev) / N_Subject[1])

d_prev_adj_rus %>% 
  group_by(Tag) %>% 
  summarise(prev = sum(N_Prev) / N_Subject[1])




save(d_prev_adj_ru, file = here::here("data", "d_prev_adj_ru.rdata"))

save(d_prev_adj_rus, file = here::here("data", "d_prev_adj_rus.rdata"))

