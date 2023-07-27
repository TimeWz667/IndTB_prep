library(tidyverse)

dir.create(here::here("serve", "WarRoom"), showWarnings = F)


list_locs <- tibble(Folder = dir("inputs")) %>% 
  mutate(
    Scale = case_when(
      startsWith(Folder, "Region") ~ "Region",
      startsWith(Folder, "State") ~ "State",
      T ~ "Nation"
    ),
    Location = Folder,
    Location = gsub("Region_", "", Location),
    Location = gsub("State_", "", Location),
    Availability = 0
  )


for (loc in dir("inputs")) {
  targets <- read_csv(here::here("inputs", loc, "targets.csv")) %>% 
    filter(Year >= 2015) %>% 
    arrange(Year, Index)

  
  if (all(c("DrugTime", "PrTxiPub", "TxI") %in% targets$Index)) {
    dir.create(here::here("serve", "WarRoom", loc), showWarnings = F)
    write_csv(targets, here::here("serve", "WarRoom", loc, "targets.csv"))
    
    list_locs[list_locs$Folder == loc, ]$Availability <- 1
  }
}


write_csv(list_locs, here::here("serve", "WarRoom", "list_locs.csv"))
