library(tidyverse)



d_burden <- read_csv("../Data/WHO/TB_burden_countries.csv") %>% 
  filter(iso3 == "IND") %>% 
  filter(year >= 2014) %>% 
  mutate(
    Inc_M = e_inc_100k * 1E-5,
    Inc_L = e_inc_100k_lo * 1E-5,
    Inc_U = e_inc_100k_hi * 1E-5,
    Mor_M = e_mort_100k * 1E-5,
    Mor_L = e_mort_100k_lo * 1E-5,
    Mor_U = e_mort_100k_hi * 1E-5,
  ) %>% 
  select(Year = year, Country = country, starts_with("Inc"), starts_with("Mor"))


dat_burden_as <- read_csv("../Data/WHO/TB_burden_age_sex.csv") %>% 
  filter(iso3 == "IND") %>% 
  filter(year >= 2014)

dat_burden_as %>% 
  filter(sex == "a")



save(d_burden, file = here::here("data", "d_burden_all.rdata"))

