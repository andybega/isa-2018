
library("tidyverse")
library("states")

source("input/itt/itt.R")
cy <- itt_get() %>%
  mutate(RstrctAccess = as.integer(RstrctAccess=="Yes"))

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

source("input/mrs-legal/legal.R")
cy <- legal_get("yearly") %>%
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

# Construct DV versions for each victim type
yy_levels <- c("Routine", "Widespread", "Systematic")
yvars <- list(NULL)
for (yy in cy %>% select(starts_with("LoT")) %>% names()) {
  newyname <- yy %>% str_replace(., "LoT", "yy_")
  yvars <- c(yvars, newyname)
  cy[, newyname] <- cy[, yy] %in% yy_levels
}
yvars <- unlist(yvars)
attr(cy, "yvars") <- yvars


saveRDS(cy, file = "output/cy.rds")
write_csv(cy, path = "output/cy.csv")
