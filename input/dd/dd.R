
library("tidyverse")
library("states")
library("readxl")

PATH_DD <- getSrcDirectory(function(x) x)
if (PATH_DD=="") PATH_DD <- "input/dd"

dd_get <- function(what = "yearly", path = PATH_DD) {
  if (what=="yearly") return(dd_get_yearly(path))
}

dd_get_yearly <- function(path = PATH_DD) {
  return(dd_get_raw())
}

dd_get_raw <- function(path = PATH_DD) {
  raw <- readxl::read_xls(file.path(path, "data/ddrevisited_data_v1.xls"))
  raw <- raw %>%
    select(cowcode, cowcode2, ctryname, year, regime) %>%
    mutate(regime = factor(
      regime, levels = 0:5, 
      labels = c("Parliamentary democracy", "Semi-presidential democracy",
                 "Presidential democracy", "Civilian dictatorship", 
                 "Military dictatorship", "Royal dictatorship"))) 
  
  # normalize country codes
  raw <- raw %>%
    mutate(gwcode = case_when(
      cowcode==255 ~ 260L,
      cowcode2==342 ~ 340L,
      cowcode==679 ~ 678L,
      cowcode==970 ~ 971L,
      cowcode==946 ~ 970L,
      cowcode==947 ~ 973L,
      cowcode2==947 ~ 973L,
      cowcode==955 ~ 972L,
      TRUE ~ as.integer(cowcode)
    ))
  
  cy <- state_panel(start = as.Date(paste0(min(raw$year), "-12-31")),
                    end = as.Date(paste0(max(raw$year), "-12-31")),
                    by = "year") %>%
    mutate(year = as.integer(substr(date, 1, 4)))
  mismatch <- anti_join(raw, cy, by = c("gwcode", "year"))
  missing  <- anti_join(cy, raw, by = c("gwcode", "year")) %>%
    group_by(gwcode) %>%
    arrange(gwcode, year) %>%
    mutate(date_id = id_date_sequence(date, pd = "year")) %>%
    group_by(gwcode, date_id) %>%
    summarize(years = paste0(range(year), collapse = " - "))
  
  cy <- raw %>%
    select(gwcode, year, regime) %>%
    left_join(cy, ., by = c("gwcode", "year"))
  cy
}