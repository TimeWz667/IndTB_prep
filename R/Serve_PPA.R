library(tidyverse)



dir.create(here::here("serve", "PPA"), showWarnings = F)


for (loc in dir("inputs")) {
  d <- read_csv(here::here("inputs", loc, "targets.csv"))

  if (all(c("DrugTime", "PrTxiPub", "TxI", "CNR", "TestR") %in% d$Index)) {
    file.copy(here::here("inputs", loc, "targets.csv"), here::here("serve", "PPA", "targets_" + glue::as_glue(loc) + ".csv"))
  }
}
