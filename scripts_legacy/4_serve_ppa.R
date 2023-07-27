library(tidyverse)
library(jsonlite)


folder <- "serve/PPA"
dir.create(folder, showWarnings = F)


dir.create(folder + glue::as_glue("/TBPS"))



file.copy(here::here("data_raw", "TBPS_ASC_Region.csv"), here::here("serve", "PPA", "TBPS"))

file.copy(here::here("data_raw", "TBPS_ASC_State.csv"), here::here("serve", "PPA", "TBPS"))

file.copy(here::here("data_raw", "d_drug.csv"), here::here("serve", "PPA"))

file.copy(here::here("data", "d_itr.rdata"), here::here("serve", "PPA"))

file.copy(here::here("data_raw", "Population.csv"), here::here("serve", "PPA"))
