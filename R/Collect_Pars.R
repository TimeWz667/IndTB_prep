library(tidyverse)



load(here::here("data_raw", "state_map.rdata"))
load(here::here("data_raw", "WPP_Pop.rdata"))
load(here::here("data", "t_txo.rdata"))
load(here::here("data", "t_txi.rdata"))


locs <- state_map$State


## Population dynamics ----



pars0 <- d_wpp %>% 
  mutate(
    Time = Year + 0.5
  ) %>% 
  select(Time, Pop = N_Pop, r_die = R_Dea, r_bir = R_Bir) %>% 
  as.list()


mod_resid <- odin::odin({
  ## ODEs -----
  deriv(Y) <- (br_t - dr_t - mr_t) * Y
  
  
  ## Initial values -----
  initial(Y) <- N0
  N0 <- user()
  
  ## Output -----
  output(N) <- Y
  output(dr) <- dr_t
  output(br) <- br_t
  output(mr) <- mr_t

  
  ## dims -----
  Time[] <- user() # data times for AIM by 1
  dim(Time) <- user()
  
  
  ## lengths -----
  n_years <- length(Time)
  
  
  ## demography -----
  r_bir[] <- user()
  dim(r_bir) <- n_years
  br_t <- interpolate(Time, r_bir, "linear")
  
  r_die[] <- user()
  dim(r_die) <- n_years
  dr_t <- interpolate(Time, r_die, "linear")
  
  
  Pop[] <- user()
  dim(Pop) <- n_years
  pop_t <- interpolate(Time, Pop, "linear")
  
  resid <- 10 * (pop_t - Y) / pop_t
  mr_t <- br_t - dr_t - resid
  
})


cm_resid <- mod_resid$new(user = c(
  pars0,
  N0 = d_wpp %>% 
    filter(Year == 1970) %>% 
    pull(N_Pop0)
)
)


ys <- cm_resid$run(seq(1970, 2080, 0.25)) %>% data.frame() %>% as_tibble()

ys %>%  
  ggplot() +
  geom_line(aes(x = t, y = N)) +
  geom_point(data = d_wpp %>% filter(Year > 1980 & Year < 2080), aes(x = Year, y = N_Pop0), alpha = 0.3)



## Reform pars ----
pars_pop <- bind_rows(ys[1, ] %>% mutate(t = 0), ys) %>% 
  select(Time = t, Pop = N, r_die = dr, r_bir = br, r_mig = mr) %>% 
  as.list()



mod <- odin::odin({
  ## ODEs -----
  deriv(Y) <- (br_t - dr_t - mr_t) * Y
  
  
  ## Initial values -----
  initial(Y) <- N0
  N0 <- user()
  
  ## dims -----
  Time[] <- user() # data times for AIM by 1
  dim(Time) <- user()
  
  
  ## lengths -----
  n_years <- length(Time)
  
  
  ## demography -----
  r_bir[] <- user()
  dim(r_bir) <- n_years
  br_t <- interpolate(Time, r_bir, "linear")
  
  r_die[] <- user()
  dim(r_die) <- n_years
  dr_t <- interpolate(Time, r_die, "linear")
  
  r_mig[] <- user()
  dim(r_mig) <- n_years
  mr_t <- interpolate(Time, r_mig, "linear")
  
  Pop[] <- user()
  dim(Pop) <- n_years
  pop_t <- interpolate(Time, Pop, "linear")
})


cm <- mod$new(user = c(
  pars_pop %>% as.list(),
  N0 = d_wpp %>% filter(Year == 1970) %>% pull(N_Pop0)
))


ys <- cm$run(seq(1970, 2080, 0.5)) %>% data.frame() %>% as_tibble()

ys %>% 
  ggplot() +
  geom_line(aes(x = t, y = Y)) +
  geom_point(data = d_wpp %>% filter(Year > 1990 & Year < 2080), aes(x = Year, y = N_Pop0))



pars_pop

pars_pop$N0s <- local({
  x <- d_wpp %>% 
    select(Year, N_Pop0) %>% 
    filter(Year %in% seq(1970, 2010, 5))
  
  set_names(x$N_Pop0, paste0("N", x$Year)) %>% as.list()
})

jsonlite::write_json(pars_pop, path = here::here("Inputs", "India", "pars_pop.json"), digits = 8, auto_unbox = T)


## Treatments

tx <- bind_rows(
  t_txi %>% 
    group_by(State, Index, Tag) %>% 
    summarise(V = weighted.mean(M, N)),
  t_txi %>% 
    group_by(State, Index) %>% 
    summarise(V = weighted.mean(M, N)) %>% 
    mutate(Tag = "All"),
  t_txo %>% 
    group_by(State, Index, Tag) %>% 
    summarise(V = weighted.mean(M, N)),
  t_txo %>% 
    group_by(State, Index) %>% 
    summarise(V = weighted.mean(M, N)) %>% 
    mutate(Tag = "All")
)
  

for (loc in locs) {
  pars_tx <- lapply(c(Pri="Pri", Pub="Pub", All="All"), function(tag) {
    temp = tx %>% filter(Tag == tag & State == loc)
    as.list(setNames(temp$V, temp$Index))
  })
  jsonlite::write_json(pars_tx, path = here::here("Inputs", loc, "pars_tx.json"), digits = 8, auto_unbox = T)
  
}



