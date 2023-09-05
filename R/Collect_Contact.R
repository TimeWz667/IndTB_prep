library(tidyverse)



pop <- read_csv("../DataPublic/WPP/WPP2022_PopulationByAge5GroupSex_Medium.csv") %>% 
  filter(ISO3_code == "IND") %>% 
  mutate(
    age_contactor = case_when(
      AgeGrpStart >= 75 ~ "75+",
      T ~ AgeGrp
    )
  ) %>% 
  select(Year = Time, age_contactor, N = PopTotal)



contacts <- read_csv("../DataPublic/synthetic_contacts_2020.csv")


cmx <- contacts %>% 
  filter(iso3c == "IND") %>% 
  filter(setting == "overall") %>% 
  filter(location_contact == "all") %>% 
  mutate(
    age_contactor = gsub(" to ", "-", age_contactor),
    age_cotactee = gsub(" to ", "-", age_cotactee)
  ) %>% 
  select(age_contactor, age_cotactee, contacts = mean_number_of_contacts) %>% 
  data.frame()




cmx


pop %>% 
  left_join(cmx %>% 
              group_by(age_contactor) %>% 
              summarise(Ctx = sum(contacts))) %>%
  group_by(Year) %>% 
  summarise(
    Ctx = sum(Ctx * N) / sum(N)
  ) %>% 
  ungroup() %>% 
  mutate(adr = c(0, - diff(log(Ctx)))) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = adr))
  

