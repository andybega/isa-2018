
library("tidyverse")
library("readr")
library("states")
library("lubridate")

PATH_LEGAL <- getSrcDirectory(function(x) x)
if (PATH_LEGAL=="") PATH_LEGAL <- "input/mrs-legal"

legal_get <- function(what = "raw") {
  if (what=="raw") return(legal_get_raw())
  if (what=="yearly") return(legal_get_raw())
}

legal_get_raw <- function(path = PATH_LEGAL) {
  legal <- read_dta(file.path(path, "data/data072412.dta"))
  
  want <- c("cowcode", "year", "h_j", "ht_colonial", "legalsys", "hensel_colonial")
  legal <- legal %>% 
    select(want) %>%
    mutate(
      gwcode = case_when(
        cowcode==255 ~ 260L,
        cowcode==679 ~ 678L,
        cowcode==970 ~ 971L,
        cowcode==946 ~ 970L,
        cowcode==947 ~ 973L,
        cowcode==955 ~ 972L,
        TRUE ~ as.integer(cowcode) ),
      cowcode = NULL, 
      year = as.integer(year),
      mrs_legalsys = factor(legalsys, labels = attr(legalsys, "labels") %>% names()),
      legalsys = NULL,
      h_indjudiciary = as.integer(h_j),
      h_j = NULL,
      ht_colonial = factor(ht_colonial, labels = attr(ht_colonial, "labels") %>% names()) %>%
        fct_relabel(., str_replace, pattern = "^[0-9]+\\. ", "") %>%
        fct_recode(., `Never colonized` = "Never colonized by a Western overseas colonial power"),
      hensel_colonial = as.factor(hensel_colonial)) %>%
    # 678 has duplicated rows, aggregate those
    group_by(gwcode, year) %>%
    summarize_all(funs(unique(.)[1])) %>%
    ungroup() %>%
    select(gwcode, year, everything())
  
  cy <- state_panel(start = paste0(min(legal$year), "-12-31"), 
                    end = paste0(max(legal$year), "-12-31"), by = "year")
  cy$year <- lubridate::year(cy$date)
  
  data(cowstates)
  cnames <- cowstates %>% group_by(cowcode) %>% 
    summarize(country = tail(unique(country_name), 1),
              start = tail(unique(start), 1),
              end = tail(unique(end), 1))
  
  mismatch <- anti_join(legal, cy, by = c("gwcode", "year")) %>%
    group_by(gwcode) %>%
    summarize(n = n(), years = paste0(min(year), " - ", max(year))) %>%
    left_join(., cnames, by = c("gwcode" = "cowcode"))
  missing <- anti_join(cy, legal, by = c("gwcode", "year")) %>%
    group_by(gwcode) %>%
    summarize(n = n(), years = paste0(min(year), " - ", max(year))) %>%
    left_join(., cnames, by = c("gwcode" = "cowcode"))
  # use last value for Yugoslavia as values for Serbia, Montenegro in 2006
  drop_in <- legal %>%
    filter(gwcode==345 & year==2006) %>%
    slice(c(1, 1)) %>%
    mutate(gwcode = c(340, 341))
  legal <- bind_rows(legal, drop_in)
  cy <- left_join(cy, legal, by = c("gwcode", "year"))
  
  # set Yemen to islamic tradition
  cy$mrs_legalsys[cy$gwcode %in% c(678, 680) & is.na(cy$mrs_legalsys)] <- "Islamic"
  # set East Timor to Civil; based on Portugese civil law
  cy$mrs_legalsys[cy$gwcode==860 & is.na(cy$mrs_legalsys)] <- "Civil"
  
  cy
}

