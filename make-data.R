
library("tidyverse")
library("states")

source("input/itt/itt.R")
cy <- itt_get()

source("input/wdi/wdi.R")
cy <- wdi_get("input/wdi") %>%
  dplyr::rename(year = datestr) %>%
  dplyr::select(-date) %>%
  dplyr::left_join(cy, ., by = c("gwcode", "year")) %>%
  dplyr::select(gwcode, year, date, everything())

source("input/v-dem/vdem.R")
cy <- vdem_get() %>%
  mutate(datestr = as.integer(datestr)) %>%
  left_join(cy, ., by = c("gwcode", "year" = "datestr"))

source("input/des/des.R")
cy <- des_get("yearly") %>%
  select(-date) %>%
  left_join(cy, ., by = c("gwcode", "year"))

source("input/lji/lji.R")
cy <- lji_get("yearly") %>%
  select(-date) %>%
  left_join(cy, ., by = c("gwcode", "year"))

# which cases drop out from missing ITT data?
data(gwstates)
cnames <- gwstates %>% 
  group_by(gwcode) %>% 
  summarize(country = tail(country_name, 1)) %>%
  select(gwcode, country)
Encoding(cnames$country) <- "latin1"
cy %>%
  filter(is.na(LoTUnknown)) %>%
  group_by(gwcode) %>%
  summarize(years = paste0(min(year), " to ", max(year))) %>%
  left_join(., cnames, by = "gwcode") %>%
  select(gwcode, country, years) %>%
  write_csv(., path = "output/missing-from-ITT.csv")

cy <- cy %>%
  mutate(gwcode = as.integer(gwcode),
         year = as.integer(year)) %>%
  filter(!is.na(LoTUnknown))

saveRDS(cy, file = "output/cy.rds")
write_csv(cy, path = "output/cy.csv")
