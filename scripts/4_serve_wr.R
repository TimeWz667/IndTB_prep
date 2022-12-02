library(tidyverse)
library(jsonlite)


folder <- glue::as_glue("serve/wr")
dir.create(folder, showWarnings = F)





d_pop <- local({
  read_csv(here::here("data_raw", "Population.csv")) %>% 
    filter(Sex == "Total", Year >= 2015) %>% 
    select(Year, Location, Pop) %>% 
    mutate(
      Location = gsub("\\s+", "_", Location),
      Location = gsub("&", "and", Location)
    ) %>% 
    rename(State = Location)
})


d_prev <- local({
  load(here::here("data", "d_tbps_asc_state.rdata"))
  d_prev_asc_state
})


locs <- d_prev$State %>% unique()


load(here::here("data", "d_itr.rdata"))





ds <- dir(here::here("pars"))
ds <- ds[startsWith(ds, "pars_demo_")]
ds_names <- gsub("pars_demo_", "", ds)
ds_names <- gsub(".json", "", ds_names)
ds_names <- gsub("\\s+", "_", ds_names)
ds_names <- gsub("&", "and", ds_names)
names(ds) <- ds_names



for (loc in locs) {
  dir.create(folder + "/" + loc, showWarnings = F)

  path_demo <- ds[loc]  
  file.copy(here::here("pars", path_demo), folder + "/" + loc)
  file.rename(folder + "/" + loc + "/" + path_demo, 
              folder + "/" + loc + "/" + "pars_demo.json")
}




