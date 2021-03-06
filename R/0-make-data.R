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

# which cases drop out from missing ITT data?
# do this now, because below i will scale some variables and with these dropped
# cases the variables will not be mean 0 var 1 anymore (small countries drop out)
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

# GDP/pop (mostly from WDI)
cy <- dir("data-modules/gdppop/output", full.names = TRUE) %>%
  map(., read_csv, col_types = cols()) %>%
  reduce(., full_join, by = c("gwcode", "year")) %>%
  dplyr::select(-date) %>%
  dplyr::left_join(cy, ., by = c("gwcode", "year")) %>%
  dplyr::select(gwcode, year, date, everything())

cy <- cy %>%
  # Rescale GDP to billions, log, then normalize
  mutate(NY.GDP.MKTP.KD = NY.GDP.MKTP.KD / 1e9,
         ln_NY.GDP.MKTP.KD = log(NY.GDP.MKTP.KD),
         norm_ln_NY.GDP.MKTP.KD = scale(ln_NY.GDP.MKTP.KD)[, 1]) %>%
  # Log, then normalize pop
  mutate(ln_pop = log(pop),
         norm_ln_pop = scale(ln_pop)[, 1],
         ln_pop = NULL)

# V-Dem
# devtools::install_github("xmarquez/vdem")
cy <- read_rds("data-modules/vdem/output/vdem.rds") %>%
  select(-date) %>%
  left_join(cy, ., by = c("gwcode", "year"))
# v2asuffrage is quasi-binary, but with 0/100, make 0/1
cy <- cy %>%
  mutate(v2asuffrage = ifelse(v2asuffrage==100, 1L, v2asuffrage))

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

# log and normalize EPR
cy <- cy %>%
  mutate(norm_ln1p_epr_excluded_groups_count = scale(log1p(epr_excluded_groups_count))[, 1],
         norm_sqrt_epr_excluded_group_pop = scale(sqrt(epr_excluded_group_pop))[, 1])

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


# Add robustness vars -----------------------------------------------------


# Global Media Freedom 
cy <- read_csv("data-modules/global-media-freedom/output/gmfd.csv") %>%
  left_join(cy, ., by = c("gwcode", "year"))

# COW IGO stateunit (political globalization)
cy <- read_csv("data-modules/cow-igo-stateunit/output/igo.csv") %>%
  left_join(cy, ., by = c("gwcode", "year"))

# Trade as % GDP (social/economic globalization)
cy <- read_csv("data-modules/wdi-trade/output/trade.csv") %>%
  left_join(cy, ., by = c("gwcode", "year")) %>%
  mutate(norm_ln_NE.TRD.GNFS.ZS = scale(log(NE.TRD.GNFS.ZS))[, 1])

# HRO membership
cy <- read_csv("data-modules/hro/output/hro.csv") %>%
  left_join(cy, ., by = c("gwcode", "year"))
 
# Year; linear and squared
p <- poly(cy$year, degree = 2)
cy$year_poly1 <- p[, 1]
cy$year_poly2 <- p[, 2]


# Construct DV versions by victim type ------------------------------------

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

miss <- sapply(cy, function(x) sum(is.na(x)))
any_mssng <- sum(miss) > 0
if (any_mssng) {
  x <- miss[miss > 0]
  deets <- paste0(sprintf("%s (%s)", names(x), x), collapse = "; ")
  warning("There are missing values in cy:\n", deets)
}


# 
#   Done, save
#   _________

saveRDS(cy, file = "output/cy.rds")
write_csv(cy, path = "output/cy.csv")
