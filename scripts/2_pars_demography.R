library(tidyverse)


load(here::here("data", "d_pop_all.rdata"))
load(here::here("data", "d_pop_ru.rdata"))
load(here::here("data", "d_pop_rus.rdata"))


pars_demo_all <- local({
  gr <- (d_pop_all %>% 
           filter(Year >= 1969 & Year < 2060) %>% 
           mutate(gr = log(N_Pop) - lag(log(N_Pop))) %>% 
           pull(gr))[-1]
  
  yrs <- 1970 + 1:length(gr) - 1
  
  list(
    N = d_pop_rus %>% filter(Year %in% yrs) %>% pull(N_Pop),
    Gr = gr,
    Dr = d_pop_rus %>% filter(Year %in% yrs) %>% pull(R_Die),
    Year = yrs
  )
})



pars_demo_ru <- lapply(c(Rural = "Rural", Urban = "Urban"), function(area) {
  gr <- (d_pop_ru %>% 
           filter(Year >= 1969 & Year < 2060 & Tag == area) %>% 
           mutate(gr = log(N_Pop) - lag(log(N_Pop))) %>% 
           pull(gr))[-1]
  
  yrs <- 1970 + 1:length(gr) - 1
  
  list(
    N = d_pop_rus %>% filter(Year %in% yrs & Tag == area) %>% pull(N_Pop),
    Gr = gr,
    Dr = d_pop_rus %>% filter(Year %in% yrs & Tag == area) %>% pull(R_Die),
    Year = yrs
  )
})



pars_demo_rus <- lapply(c(Rural = "Rural", Urban = "Urban", Slum = "Slum"), function(area) {
  gr <- (d_pop_rus %>% 
           filter(Year >= 1969 & Year < 2060 & Tag == area) %>% 
           mutate(gr = log(N_Pop) - lag(log(N_Pop))) %>% 
           pull(gr))[-1]
  
  yrs <- 1970 + 1:length(gr) - 1
  
  list(
    N = d_pop_rus %>% filter(Year %in% yrs & Tag == area) %>% pull(N_Pop),
    Gr = gr,
    Dr = d_pop_rus %>% filter(Year %in% yrs & Tag == area) %>% pull(R_Die),
    Year = yrs
  )
})


save(pars_demo_all, file = here::here("pars", "pars_demo_all.rdata"))
save(pars_demo_ru, file = here::here("pars", "pars_demo_ru.rdata"))
save(pars_demo_rus, file = here::here("pars", "pars_demo_rus.rdata"))




