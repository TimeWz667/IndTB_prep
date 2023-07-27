library(tidyverse)



load(here::here("data_raw", "itr.rdata"))



d_itr_notif <- d_itr_notif %>% 
  mutate(
    State = stringr::str_to_title(State),
    State = gsub(" ", "_", State),
    State = gsub("&", "and", State),
    State = gsub("And_", "and_", State),
    across(-c(State, Year), function(x) as.numeric(gsub(" \\(\\S+\\)", "", x))),
    across(-c(State, Year), function(x) ifelse(is.na(x), 0, x))
  )


d_itr_tx <- d_itr_tx %>% 
  mutate(
    State = stringr::str_to_title(State),
    State = gsub(" ", "_", State),
    State = gsub("&", "and", State),
    State = gsub("And_", "and_", State),
    across(-c(State, Year), function(x) as.numeric(gsub(" \\(\\S+\\)", "", x))),
    across(-c(State, Year), function(x) ifelse(is.na(x), 0, x))
  )


d_itr_acf <- d_itr_acf %>% 
  mutate(
    State = stringr::str_to_title(State),
    State = gsub(" ", "_", State),
    State = gsub("&", "and", State),
    State = gsub("And_", "and_", State),
    across(-c(State, Year), function(x) as.numeric(gsub(" \\(\\S+\\)", "", x))),
    across(-c(State, Year), function(x) ifelse(is.na(x), 0, x))
  )


d_itr_ct <- d_itr_ct %>% 
  mutate(
    State = stringr::str_to_title(State),
    State = gsub(" ", "_", State),
    State = gsub("&", "and", State),
    State = gsub("And_", "and_", State),
    across(-c(State, Year), function(x) as.numeric(gsub(" \\(\\S+\\)", "", x))),
    across(-c(State, Year), function(x) ifelse(is.na(x), 0, x))
  )


d_itr_notif
d_itr_tx
d_itr_acf
d_itr_ct

save(d_itr_notif, d_itr_tx, d_itr_acf, d_itr_ct, file = here::here("data", "d_itr.rdata"))

