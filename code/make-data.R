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

# GDP/pop (mostly from WDI)
cy <- dir("data-modules/gdppop/output", full.names = TRUE) %>%
  map(., read_csv, col_types = cols()) %>%
  reduce(., full_join, by = c("gwcode", "year")) %>%
  dplyr::select(-date) %>%
  dplyr::left_join(cy, ., by = c("gwcode", "year")) %>%
  dplyr::select(gwcode, year, date, everything())

# V-Dem
# devtools::install_github("xmarquez/vdem")
cy <- read_rds("data-modules/vdem/output/vdem.rds") %>%
  select(-date) %>%
  left_join(cy, ., by = c("gwcode", "year"))

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

# CCP
cy <- read_rds("data-modules/ccp/output/ccp.rds") %>%
  select(-date) %>%
  rename_at(vars(-gwcode, -year), ~ paste0("ccp_", .)) %>%
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


#
#   Check missing values ----
#   _________________________

missing_by_col <- sapply(cy, function(x) sum(is.na(x))) %>% sort() %>% `[`(. > 0)

# Drop UAE because missing in V-Dem
cy <- cy %>%
  filter(gwcode!=696)

# Drop one of the V-Dem variables because of missingness
cy <- cy %>%
  select(-v2clsnlpct)

# Drop all the hensel colonial measures and h_indjudiciary
cy <- cy %>%
  select(-h_indjudiciary, -hensel_colonial)

any_mssng <- sum(sapply(cy, function(x) sum(is.na(x))))
if (any_mssng) stop("There are missing values in cy, something is wrong")

# 
#   Done, save
#   _________

saveRDS(cy, file = "output/cy.rds")
write_csv(cy, path = "output/cy.csv")
