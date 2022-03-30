library(tidyverse)


load(here::here("data", "d_pop_all.rdata"))
load(here::here("data", "d_pop_ru.rdata"))
load(here::here("data", "d_pop_rus.rdata"))
load(here::here("data", "d_burden_all.rdata"))
load(here::here("data", "d_prev_adj_ru.rdata"))
load(here::here("data", "d_prev_adj_rus.rdata"))
load(here::here("data", "d_tx.rdata"))


year0 <- 2019

burden <- d_burden %>% 
  filter(Year <= year0) %>% 
  mutate(
    adr = - mean(diff(log(Inc_M))),
    fact = exp(- adr * (Year - year0)),
    inc = Inc_M / fact,
    mor = Mor_M / fact
  ) %>% 
  summarise(across(c(adr, inc, mor), mean)) %>% 
  as.list()


tx <- lapply(c(All = "All", DS = "DS", DR = "DR"), function(dr) {
  d <- d_tx %>% 
    filter(Tag == dr & !is.na(Count)) %>% 
    group_by(Outcome) %>%
    summarise(pr = mean(Count / N, na.rm = T))
  
  as.list(setNames(d$pr, d$Outcome))
})


pars_cascade_all <- local({
  list(
    Year = year0,
    ADR = burden$adr,
    Inc = burden$inc,
    Mor = burden$mor,
    Prev = d_prev_adj_ru %>% 
      filter(Tag == "All") %>% 
      mutate(prev = N_Prev / N_Subject) %>% 
      (function(df) { as.list(setNames(df$prev, df$State))}) (),
    Pop = d_pop_ru %>% 
      filter(Year == year0) %>% 
      (function(df) { as.list(setNames(df$N_Pop, df$Tag))}) (),
    Tx = tx
  )
})


pars_cascade_ru <- local({
  list(
    Year = year0,
    ADR = burden$adr,
    Inc = burden$inc,
    Mor = burden$mor,
    Prev = lapply(c(All = "All", Rural = "Rural", Urban = "Urban"), function(tag) {
      d_prev_adj_ru %>% 
        filter(Tag == tag) %>% 
        mutate(prev = N_Prev / N_Subject) %>% 
        (function(df) { as.list(setNames(df$prev, df$State))}) ()
    }),
    Pop = d_pop_ru %>% 
      filter(Year == year0) %>% 
      (function(df) { as.list(setNames(df$N_Pop, df$Tag))}) (),
    Tx = tx
  )
})


pars_cascade_rus <- local({
  list(
    Year = year0,
    ADR = burden$adr,
    Inc = burden$inc,
    Mor = burden$mor,
    Prev = lapply(c(All = "All", Rural = "Rural", Urban = "Urban", Slum = "Slum"), function(tag) {
      d_prev_adj_rus %>% 
        filter(Tag == tag) %>% 
        mutate(prev = N_Prev / N_Subject) %>% 
        (function(df) { as.list(setNames(df$prev, df$State))}) ()
    }),
    Pop = d_pop_rus %>% 
      filter(Year == year0) %>% 
      (function(df) { as.list(setNames(df$N_Pop, df$Tag))}) (),
    Tx = tx
  )
})


save(pars_cascade_all, file = here::here("pars", "pars_cascade_all.rdata"))
save(pars_cascade_ru, file = here::here("pars", "pars_cascade_ru.rdata"))
save(pars_cascade_rus, file = here::here("pars", "pars_cascade_rus.rdata"))
