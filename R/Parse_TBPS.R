library(tidyverse)



load("data_raw/state_map.rdata") 


d_prev_asc_region <- read_csv(here::here("data_raw", "TBPS_ASC_Region.csv"))
save(d_prev_asc_region, file = here::here("data_raw", "TBPS_region.rdata"))


d_prev_asc_state <- read_csv(here::here("data_raw", "TBPS_ASC_State.csv"))
save(d_prev_asc_state, file = here::here("data_raw", "TBPS_state.rdata"))


d_prev_asc_ru <- read_csv(here::here("data_raw", "TBPS_RU.csv"))
save(d_prev_asc_ru, file = here::here("data_raw", "TBPS_RU.rdata"))

