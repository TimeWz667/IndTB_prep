rm(list = ls())


scripts <- c(
  "0_fetch_data.R", 
  "1_data_arti.R", 
  "1_data_burden.R", 
  "1_data_demography.R", 
  "1_data_ind_report.R", 
  "1_data_notification.R",
  "1_data_prev.R",
  "1_data_prev_adj.R",
  "1_data_tx.R",
  "2_pars_demography.R", 
  "2_pars_cascade.R"
)


for (script in scripts) {
  source(here::here("scripts", script))
}

