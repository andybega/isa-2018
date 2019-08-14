#
#   Create a summary table of the directions of effects in the sensitivity 
#   analysis
#
#   14 August 2019
#

library("dplyr")
library("tidyr")
library("readr")
library("forcats")

coefs <- read_rds("output/all-models-coefs.rds")

tbl <- coefs %>%
  count(voi, y, direction) %>%
  mutate(direction = fct_rev(direction)) %>%
  spread(direction, n, fill = 0L) %>%
  mutate(voi = rename_terms(voi),
         y   = rename_dvs(y)) %>%
  rename(Variable = voi, Outcome = y)

write_csv(tbl, "output/all-models-direction-table.csv")
