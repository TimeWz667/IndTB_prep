library(tidyverse)



db <- read_csv("../DataPublic/drug_sale.csv")


d_drug <- read_csv("../DataPublic/drug_sale.csv") %>% 
  extract(Tx_month_pri, c("DrugTime_M", "DrugTime_L", "DrugTime_U"), "(\\S+) \\((\\S+), (\\S+)\\)", convert = T) %>% 
  mutate(
    across(starts_with("DrugTime"), function(x) x * 1e-5 / 12),
    State = stringr::str_to_title(State),
    State = gsub(" ", "_", State),
    State = gsub("&", "and", State),
    State = gsub("And_", "and_", State),
    Tag = "All",
    Index = "DrugTime"
  )  %>% 
  select(Region, State, M = DrugTime_M, L = DrugTime_L, U = DrugTime_U)

save(d_drug, file = here::here("data_raw", "DS_DrugTime.rdata"))



db <- read_csv("../DataPublic/drug_sale_ts.csv")


db_state <- db %>% 
  pivot_longer(-State) %>% 
  extract(name, c("name", "Year"), "(N|P)(\\d+)", convert = T) %>% 
  pivot_wider() %>% 
  mutate(
    N = N * 1e3,
    Pop = N / P * 1e5
  ) %>% 
  select(State, Year, N = Pop, X = N) %>% 
  filter(State != "India")
  # group_by(Year) %>% 
  # summarise(N = sum(N), Pop = sum(Pop)) %>% 
  


db_all <- db_state %>% group_by(Year) %>% 
  summarise(N = sum(N), X = sum(X)) %>% 
  mutate(
    State = "India"
  )
  

d_drug_ts <- d_drug %>% 
  mutate(
    Error = (U - L) / 2 / M
  ) %>% 
  select(Region, State, Error) %>% 
  left_join(bind_rows(db_state, db_all)) %>% 
  mutate(
    M = X / N,
    L = M * (1 - Error),
    U = M * (1 + Error),
    L2 = qbinom(0.025, size = N, prob = M) / N,
    U2 = qbinom(0.975, size = N, prob = M) / N
  ) %>% 
  select(Region, State, Year, M, L = L2, U = U2, N, X)

save(d_drug_ts, file = here::here("data_raw", "DS_DrugTime.rdata"))



# %>% 
#   full_join(dat_pop) %>% 
#   mutate(
#     DrugTime_M = ifelse(is.na(DrugTime_M), DrugTime_M[State == "North East"], DrugTime_M),
#     DrugTime_L = ifelse(is.na(DrugTime_L), DrugTime_L[State == "North East"], DrugTime_L),
#     DrugTime_U = ifelse(is.na(DrugTime_U), DrugTime_U[State == "North East"], DrugTime_U)
#   ) %>% 
#   filter(State != "North East") %>% 
#   select(Region, State, starts_with("DrugTime"))

