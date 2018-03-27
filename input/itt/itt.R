
library("tidyverse")
library("states")
library("haven")

PATH_ITT <- getSrcDirectory(function(x) x)
if (PATH_ITT=="") PATH_DES <- "input/itt"

itt_factor <- function(x) {
  factor(x, levels = c("No Allegations", "Infrequent", "Several", "Routinely", "Widespread", "Systematic", "Continued", "Improved", "Increased/Worsening", "Allegation, No LoT", "-777", "-999"))
}

itt_get <- function(what = "allegation count", path = PATH_ITT) {
  if (what=="allegation count") out <- itt_get_allegation_count(path)
  if (what=="by victim") out <- itt_get_by_victim(path)
  out
}

itt_get_allegation_count <- function(path) {
  raw <- read_dta(file.path(path, "data/RevisedSAFINAL_11Mar2013.dta"))
  alleg <- raw %>%
    # some begin years are larger than end years, swap them
    # this is an XOR swap algorigthm, it looks fugly, but a big for loop to do
    # the same is also ugly. 
    mutate(yearbegin = if_else(yearbegin > yearend, yearbegin + yearend, yearbegin),
           yearend   = if_else(yearbegin > yearend, yearbegin - yearend, yearend),
           yearbegin = if_else(yearbegin > yearend, yearbegin - yearend, yearbegin)) %>%
    # normalize country codes
    mutate(gwcode = case_when(
      cowccode==255 ~ 260L,
      cowccode==679 ~ 678L,
      TRUE ~ as.integer(cowccode)
    )) %>%
    select(-cowccode, -iso3numeric, -iso3alpha) 
  
  # Some allegations are multi-year periods, split into yearly
  by_year <- alleg %>%
    rowwise() %>%
    mutate(year = seq(yearbegin, yearend) %>% paste0(collapse = ",")) %>%
    ungroup() %>%
    separate_rows(year, sep = ",") %>%
    mutate(year = as.integer(year))
  
  # Split allegations with multiple victims
  by_victim <- by_year %>%
    gather(victim, value, vtcriminal:vtunst) %>%
    filter(value==1) %>%
    select(-value) 
  
  by_victim_cy <- by_victim %>%
    group_by(gwcode, year, victim) %>%
    summarize(allegations = n(),
              itt_RstrctAccess = max(RstrctAccess)) %>%
    mutate(itt_alleg_vtall = sum(allegations)) %>%
    ungroup()
  
  cy <- state_panel(start = paste0(min(by_victim_cy$year), "-12-31"),
                    end = paste0(max(by_victim_cy$year), "-12-31"),
                    by = "year") %>%
    mutate(year = lubridate::year(date)) %>%
    select(-date)
  by_cy <- by_victim_cy %>%
    # redo access so it's the same for each country-year
    group_by(gwcode, year) %>%
    mutate(itt_RstrctAccess = max(itt_RstrctAccess)) %>%
    ungroup() %>%
    # put victim type in columsn so we have country-year data
    mutate(victim = paste0("itt_alleg_", victim)) %>%
    spread(victim, allegations) %>%
    # normalize to GW
    left_join(cy, ., by = c("gwcode", "year")) %>%
    replace(is.na(.), 0) %>%
    arrange(gwcode, year)
  by_cy  
}

itt_get_by_victim <- function(path) {
  cyvt <- read_csv(file.path(path, "data/CYVT.csv"),
                   col_types = cols(
                     cowccode = col_integer(),
                     iso3numeric = col_integer(),
                     iso3alpha = col_character(),
                     year = col_integer(),
                     LoTUnknown = col_character(),
                     LoTCriminal = col_character(),
                     LoTDissident = col_character(),
                     LoTMarginalized = col_character(),
                     LoTStateAgent = col_character(),
                     RstrctAccess = col_character()
                   ))
  data(gwstates)
  
  cy   <- state_panel("1995-01-01", "2005-01-01", by = "year", partial = "any")
  cy$year <- lubridate::year(cy$date)
  
  cyvt <- cyvt %>% 
    mutate(gwcode = case_when(
      cowccode==255 ~ 260L,
      cowccode==679 ~ 678L,
      TRUE ~ as.integer(cowccode)
    )) %>%
    select(-cowccode, -iso3numeric, -iso3alpha) %>%
    mutate(RstrctAccess = as.integer(RstrctAccess=="Yes"))
  
  cyvt <- left_join(cy, cyvt, by = c("gwcode", "year"))
  missing_data <- cyvt %>% 
    filter(is.na(LoTUnknown)) %>% 
    group_by(gwcode) %>% 
    summarize(year1 = min(year), year2 = max(year)) %>%
    left_join(., gwstates, by = c("gwcode"))
  cyvt <- cyvt %>%
    rename_at(vars(starts_with("LoT"), RstrctAccess), function(x) paste0("itt_", x))
}
