
library("tidyverse")
library("states")

source("input/itt/itt.R")
cy <- itt()

source("input/wdi/wdi.R")
cy <- wdi("input/wdi") %>%
  rename(year = datestr) %>%
  left_join(cy, ., by = c("gwcode", "year", "date"))

source("input/v-dem/vdem.R")
cy <- vdem() %>%
  left_join(cy, ., by = c("gwcode", "year" = "datestr"))


saveRDS(cy, file = "input/cy.rds")
write_csv(cy, file = "input/cy.csv")

cy %>%
  filter(!is.na(LoTUnknown)) %>%
  gather(victim, lot, starts_with("LoT")) %>%
  mutate(victim = str_replace(victim, "LoT", ""))
