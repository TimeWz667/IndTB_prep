rm(list = ls())


scripts <- c(
  "Parse_ARTI.R", 
  "Parse_DrugSale.R",      
  "Parse_ITR.R", 
  "Parse_Pop.R", 
  "Parse_TBPS.R", 
  "Parse_WHO.R",
  "Collect_Pars.R", 
  "Collect_Targets_Region.R", 
  "Collect_Targets_State.R"
)


for (script in scripts) {
  source(here::here("R", script))
}

