library(tidyverse)


## Load data
noti_who <- read_csv("../Data/WHO/TB_notifications.csv") %>% 
  filter(year >= 2014) %>% 
  filter(iso3 == "IND")


cases <- noti_who %>% 
  select(Country = country, Year = year, matches("newrel_(f|m)014"), matches("newrel_(f|m)15plus")) %>% 
  pivot_longer(-c(Country, Year), values_to = "Case") %>% 
  extract(name, c("Sex", "Agp"), "newrel_(\\w)(\\S+)") %>% 
  mutate(Sex = ifelse(Sex == "f", "Female", "Male"),
         Agp = ifelse(Agp == "014", "[0,15)", "[15,Inf)"))


mdr <- noti_who %>% 
  select(Country = country, Year = year, MDR = conf_rrmdr)
  

amp <- noti_who %>% 
  select(Country = country, Year = year, matches("newrel_(f|m)014"), matches("newrel_(f|m)15plus"), new_ep) %>% 
  mutate(
    case_all = rowSums(across(starts_with("newrel"))),
    case_u = rowSums(across(ends_with("15plus"))),
    Amp_age = case_all / case_u,
    Amp_ep = case_all / (case_all - new_ep),
    Amp_all = Amp_age * Amp_ep
  ) %>% 
  select(Year, Country, starts_with("Amp"))


# Binding

d_case_all <- cases %>% 
  filter(Country == "India") %>% 
  group_by(Year, Country) %>% 
  summarise(
    Tag = "All",
    N_Case = sum(Case)
  ) %>% ungroup() %>% 
  left_join(amp) %>% 
  left_join(mdr) %>% 
  mutate(
    PrDR = MDR / N_Case
  )



# FP assumption and Drug sale


d_case_all <- d_case_all
  
# d_case_u <- cases %>% 
#   filter(Country == "India") %>% 
#   group_by(Year, Country) %>% 
#   summarise(
#     Tag = ">=15",
#     N_Case = sum(Case * (Agp == "[15,Inf)"))
#   ) %>% ungroup()


save(d_case_all, file = here::here("data", "d_cases_all.rdata"))
