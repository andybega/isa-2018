#
# Rscript code/make-data.R
#

suppressMessages({
  library("tidyverse")
  library("states")
  library("lubridate")
  library("stringr")
})


source("data-modules/itt/itt.R")
itt1 <- itt_api("allegation count", "data-modules/itt")  %>% 
  # for some reason in the CY data version restricted access is more conservative
  # than in allegation data, but keep it there 
  select(-itt_RstrctAccess)
itt2 <- itt_api("by victim", "data-modules/itt")
cy <- left_join(itt1, itt2, by = c("gwcode", "year")) 

source("data-modules/wdi/wdi.R")
cy <- wdi_get("data-modules/wdi") %>%
  dplyr::rename(year = datestr) %>%
  dplyr::select(-date) %>%
  dplyr::left_join(cy, ., by = c("gwcode", "year")) %>%
  dplyr::select(gwcode, year, date, everything())

# devtools::install_github("xmarquez/vdem")
source("data-modules/v-dem/vdem.R")
cy <- vdem_get() %>%
  mutate(datestr = as.integer(datestr)) %>%
  left_join(cy, ., by = c("gwcode", "year" = "datestr"))

# source("data-modules/des/des.R")
# cy <- des_get("yearly") %>%
#   select(-date) %>%
#   left_join(cy, ., by = c("gwcode", "year"))

source("data-modules/dd/dd.R")
cy <- dd_api("yearly", "data-modules/dd") %>%
  select(-date) %>%
  left_join(cy, ., by = c("gwcode", "year"))
cy$dd_democracy <- str_detect(cy$regime, "democracy") %>% as.integer()

source("data-modules/lji/lji.R")
cy <- lji_api("yearly") %>%
  select(-date) %>%
  left_join(cy, ., by = c("gwcode", "year"))

source("data-modules/mrs-legal/legal.R")
cy <- legal_get("yearly", "data-modules/mrs-legal") %>%
  select(-date) %>%
  left_join(cy, ., by = c("gwcode", "year"))

source("data-modules/epr/epr.R")
cy <- epr_api(what = "yearly", "data-modules/epr") %>%
  select(-date) %>%
  left_join(cy, ., by = c("gwcode", "year"))

source("data-modules/ucdp-acd/ucdp-acd.R")
cy <- ucdp_acd_api(what = "yearly", "data-modules/ucdp-acd") %>%
  left_join(cy, ., by = c("gwcode", "year"))

source("data-modules/gtd/gtd.R")
cy <- gtd_api("yearly", "data-modules/gtd") %>%
  left_join(cy, ., by = c("gwcode", "year"))

# which cases drop out from missing ITT data?
data(gwstates)
cnames <- gwstates %>% 
  group_by(gwcode) %>% 
  summarize(country = tail(country_name, 1)) %>%
  select(gwcode, country)
cy %>%
  filter(is.na(itt_LoTUnknown)) %>%
  group_by(gwcode) %>%
  summarize(years = paste0(min(year), " to ", max(year))) %>%
  left_join(., cnames, by = "gwcode") %>%
  select(gwcode, country, years) %>%
  write_csv(., path = "output/missing-from-ITT.csv")

cy <- cy %>%
  mutate(gwcode = as.integer(gwcode),
         year = as.integer(year)) %>%
  filter(!is.na(itt_LoTUnknown))

# Construct DV versions for each victim type
yy_levels <- c("Routine", "Widespread", "Systematic")
yvars <- list(NULL)
for (yy in cy %>% select(starts_with("itt_LoT")) %>% names()) {
  newyname <- yy %>% str_replace(., "itt_LoT", "yy_")
  yvars <- c(yvars, newyname)
  cy[, newyname] <- cy[, yy] %in% yy_levels
}
yvars <- unlist(yvars)
attr(cy, "yvars") <- yvars


saveRDS(cy, file = "output/cy.rds")
write_csv(cy, path = "output/cy.csv")
