library(tidyverse)


#### India TB report
d_itr_notif <- read_csv("../DataPublic/India TB report/ITR_Notif.csv")
d_itr_acf <- read_csv("../DataPublic/India TB report/ITR_ACF.csv")
d_itr_ct <- read_csv("../DataPublic/India TB report/ITR_CT.csv")
d_itr_tx <- read_csv("../DataPublic/India TB report/ITR_Tx.csv")


load("data_raw/state_map.rdata") 

state_map <- state_map %>% select(State_New = State, State = State_Itr)


d_itr_notif <- d_itr_notif %>% 
  mutate(
    State = stringr::str_to_title(State),
    State = gsub(" ", "_", State),
    State = gsub("&", "and", State),
    State = gsub("And_", "and_", State),
    across(-c(State, Year), function(x) as.numeric(gsub(" \\(\\S+\\)", "", x))),
    across(-c(State, Year), function(x) ifelse(is.na(x), 0, x))
  ) %>% 
  left_join(state_map) %>% 
  select(-State) %>% 
  rename(State = State_New) %>% 
  filter(!is.na(State))


d_itr_tx <- d_itr_tx %>% 
  mutate(
    State = stringr::str_to_title(State),
    State = gsub(" ", "_", State),
    State = gsub("&", "and", State),
    State = gsub("And_", "and_", State),
    across(-c(State, Year), function(x) as.numeric(gsub(" \\(\\S+\\)", "", x))),
    across(-c(State, Year), function(x) ifelse(is.na(x), 0, x))
  ) %>% 
  left_join(state_map) %>% 
  select(-State) %>% 
  rename(State = State_New) %>% 
  filter(!is.na(State))


d_itr_acf <- d_itr_acf %>% 
  mutate(
    State = stringr::str_to_title(State),
    State = gsub(" ", "_", State),
    State = gsub("&", "and", State),
    State = gsub("And_", "and_", State),
    across(-c(State, Year), function(x) as.numeric(gsub(" \\(\\S+\\)", "", x))),
    across(-c(State, Year), function(x) ifelse(is.na(x), 0, x))
  ) %>% 
  left_join(state_map) %>% 
  select(-State) %>% 
  rename(State = State_New) %>% 
  filter(!is.na(State))


d_itr_ct <- d_itr_ct %>% 
  mutate(
    State = stringr::str_to_title(State),
    State = gsub(" ", "_", State),
    State = gsub("&", "and", State),
    State = gsub("And_", "and_", State),
    across(-c(State, Year), function(x) as.numeric(gsub(" \\(\\S+\\)", "", x))),
    across(-c(State, Year), function(x) ifelse(is.na(x), 0, x))
  ) %>% 
  left_join(state_map) %>% 
  select(-State) %>% 
  rename(State = State_New) %>% 
  filter(!is.na(State))


save(d_itr_notif, file = here::here("data_raw", "ITR_Cases.rdata"))
save(d_itr_tx, file = here::here("data_raw", "ITR_TxOut.rdata"))
save(d_itr_acf, file = here::here("data_raw", "ITR_ACF.rdata"))
save(d_itr_ct, file = here::here("data_raw", "ITR_CT.rdata"))

