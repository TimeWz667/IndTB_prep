library(tidyverse)
library(jsonlite)


## Pr urban slum
pr_s <- 0.35



## Load data
dat_pop <- dpdt::fetch_demography("ByCountry/India")

dat_wup <- readxl::read_xls("../Data/WUP/WUP2018-F21-Proportion_Urban_Annual.xls", skip = 16)



pop <- bind_rows(
  dat_pop$PopN_F %>% 
    pivot_longer(-Time, names_to = "Age", values_to = "N") %>% 
    left_join(dat_pop$DeaR_F %>% 
                pivot_longer(-Time, names_to = "Age", values_to = "dr")) %>% 
    mutate(Sex = "Female"),
  dat_pop$PopN_M %>% 
    pivot_longer(-Time, names_to = "Age", values_to = "N") %>% 
    left_join(dat_pop$DeaR_M %>% 
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
    Amp = N_Pop / N_Pop_U
  ) %>% 
  ungroup()
  


## Urban / Rural
pr_urban <- dat_wup %>% 
  select(Country = `Region, subregion, country or area`, `1960`:`2050`) %>% 
  filter(Country == "India") %>% 
  pivot_longer(-Country, names_to = "Year", values_to = "PrUrban") %>% 
  mutate(
    Year = as.numeric(Year),
    PrUrban = PrUrban / 100,
    PrSlum = PrUrban * pr_s,
    PrUrban = PrUrban - PrSlum
  )



## Output

d_pop_all <- pop %>% 
  mutate(Tag = "All", Country = "India") %>% 
  select(Year, Country, Tag, N_Pop, N_Pop_Y, N_Pop_U, R_Die, R_Age, Amp)


d_pop_rus <- pop %>% inner_join(pr_urban) %>% 
  mutate(
    N_Pop_Urban = N_Pop * PrUrban,
    N_Pop_Slum = N_Pop * PrSlum,
    N_Pop_Rural = N_Pop - N_Pop_Urban - N_Pop_Slum
  ) %>% 
  select(Year, Country, R_Die, R_Age, N_Pop_Rural, N_Pop_Urban, N_Pop_Slum, Amp) %>% 
  pivot_longer(c(N_Pop_Urban, N_Pop_Rural, N_Pop_Slum), values_to = "N_Pop") %>% 
  extract(name, "Tag", "N_Pop_(\\w+)") %>% 
  select(Year, Country, Tag, N_Pop, R_Die, R_Age, Amp)
  
  
save(d_pop_all, file = here::here("data", "d_pop_all.rdata"))
save(d_pop_rus, file = here::here("data", "d_pop_rus.rdata"))
  
  
