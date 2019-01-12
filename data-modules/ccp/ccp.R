
# API function; everything else should not be called directly by user
ccp_api <- function(what = "yearly", module_path = "data-modules/ccp") {
  require("tidyverse")
  require("states")
  require("readxl")
  
  if (what=="yearly") return(ccp_get_yearly(module_path))
}

ccp_get_raw <- function(path) {
  raw <- readr::read_tsv(file.path(path, "data/ccpcnc_v2.txt"),
                         col_types = cols(.default = col_character(),
                                          cowcode = col_integer(),
                                          year = col_integer()))
  
  # convert 1/2/96/97 value columns into logical with NA
  logical_converter <- function(x) {
    xvals <- unique(x)
    has1and2 <- all(c(1, 2) %in% xvals)
    rightset <- all(xvals %in% c(1, 2, 96:99))
  }
}

ccp_get_yearly <- function(path) {
  raw <- ccp_get_raw(path)
  raw <- raw %>%
    rename(gwcode = cowcode) %>%
    mutate(gwcode = as.integer(gwcode),
           year = as.integer(year))
  
  raw <- raw %>%
    mutate(gwcode = case_when(
      gwcode==345 & year >= 2007 ~ 340L,
      TRUE ~ gwcode
    ))
  
  master <- state_panel(start = as.Date("1816-01-01"), end = as.Date(sprintf("%s-01-01", max(raw$year))),
                        partial = "any")
  master$year <- master$date %>% substr(1, 4) %>% as.integer()
  
  raw_not_in_master <- anti_join(raw, master, by = c("gwcode", "year")) %>%
    group_by(gwcode) %>%
    mutate(spell = id_date_sequence(as.Date(sprintf("%s-01-01", year)), "year")) %>%
    group_by(gwcode, spell) %>%
    summarize(years = paste0(range(year), collapse = "-"))
  
  master_not_in_raw <- anti_join(master, raw, by = c("gwcode", "year")) %>%
    group_by(gwcode) %>%
    mutate(spell = id_date_sequence(as.Date(sprintf("%s-01-01", year)), "year")) %>%
    group_by(gwcode, spell) %>%
    summarize(years = paste0(range(year), collapse = "-"))
}