library(tidyverse)


load(here::here("data", "d_prev.rdata"))
load(here::here("data", "d_pop_all.rdata"))
load(here::here("data", "d_pop_rus.rdata"))
load(here::here("data", "d_cases_all.rdata"))
load(here::here("data", "d_burden_all.rdata"))


i_area <- c("Rural", "Urban", "Slum")


year0 = 2011



d_prev_adj <- local({
  
  adr <- mean(- diff(log(d_burden$Inc_M)))
  
  d_p <- d_prev %>% 
    left_join(d_case_all %>% select(Year, Amp_all)) %>% 
    mutate(X = N_Prev * Amp_all, N = N_Subject)
  
  
  d_p <- bind_rows(
    d_p, 
    d_p %>% filter(Tag == "Urban") %>% mutate(Tag = "Slum")
  ) %>% 
    left_join(bind_rows(
      d_pop_all,
      d_pop_rus
    )) %>% 
    mutate(N = round(N * Amp)) %>% 
    select(- R_Die, - Amp) %>% 
    group_by(State) %>% 
    mutate(
      N_U = sum(N_Pop * (Tag %in% c("Urban", "Slum"))),
      N = case_when(
        Tag %in% c("Urban", "Slum") ~ N * N_Pop / N_U,
        T ~ N
      ),
      X0 = case_when(
        Tag == "Urban" ~ 3 / 2 * N,
        Tag == "Slum" ~ N,
        T ~ 1
      ),
      X0 = case_when(
        Tag %in% c("Urban", "Slum") ~ X0 / sum(X0 * (Tag %in% c("Urban", "Slum"))),
        T ~ 1
      ),
      X = X * X0 * exp(- adr * (Year - year0))
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
    )
})





d_prev_adj %>% 
  group_by(Tag) %>% 
  summarise(prev = sum(N_Prev) / N_Subject[1])




save(d_prev_adj, file = here::here("data", "d_prev_adj.rdata"))
