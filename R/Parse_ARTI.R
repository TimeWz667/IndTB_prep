library(tidyverse)

t_arti_rus <- tibble(
  Year = 2016,
  Tag = c("All", "Rural", "Urban", "Slum"),
  Index = "ARTI",
  M = c(1.5, 1.3, 2.2, 2.2 * 5 / 3) / 100 * exp(-0.045 * (2016 - 2005)),
  L = c(1.4, 1.0, 1.8, 1.8 * 5 / 3) / 100 * exp(-0.045 * (2016 - 2005)),
  U = c(1.6, 1.5, 2.6, 2.6 * 5 / 3) / 100 * exp(-0.045 * (2016 - 2005)),
  N = c(2313, 1668, 645, 645)
) %>% 
  mutate(
    Error = (U - L) / 2 / 1.96,
    Std = Error * N
  )

t_arti_rus


save(t_arti_rus, file = here::here("data", "t_arti_rus.rdata"))



t_arti_ru <- tibble(
  Year = 2016,
  Tag = c("All", "Rural", "Urban"),
  Index = "ARTI",
  M = c(1.5, 1.3, 2.2) / 100 * exp(-0.045 * (2016 - 2005)),
  L = c(1.4, 1.0, 1.8) / 100 * exp(-0.045 * (2016 - 2005)),
  U = c(1.6, 1.5, 2.6) / 100 * exp(-0.045 * (2016 - 2005)),
  N = c(2313, 1668, 645)
) %>% 
  mutate(
    Error = (U - L) / 2 / 1.96,
    Std = Error * N
  )

t_arti_ru


save(t_arti_ru, file = here::here("data", "t_arti_ru.rdata"))

