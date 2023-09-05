library(tidyverse)



dir.create(here::here("serve", "IndTBPort"), showWarnings = F)


for (d in dir("inputs/India")) {
  file.copy(here::here("inputs", "India", d), here::here("serve", "IndTBPort", d))
}
