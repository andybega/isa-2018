
library("dplyr")
library("readr")
library("states")
library("lubridate")

PATH_LJI <- getSrcDirectory(function(x) x)
if (length(PATH_LJI)==0) PATH_LJI <- "input/lji"
if (PATH_LJI=="") PATH_LJI <- "."

lji_api <- function(what = "raw", own_path = "data-modules/lji") {
  if (what=="raw") lji_get_raw(own_path)
  if (what=="yearly") lji_make_yearly(own_path)
}

lji_get_raw <- function(path = PATH_LJI) {
  raw <- read_csv(file.path(path, "data/LJI-estimates-20140422.csv"), 
                  col_types = cols(
                    country = col_character(),
                    abbr = col_character(),
                    ccode = col_integer(),
                    year = col_integer(),
                    LJI = col_double(),
                    post.sd = col_double()
                  ))
  
  foo <- raw %>% group_by(ccode) %>% 
    summarize(country = unique(country), 
              years = paste0(min(year),";", max(year))) 
  raw <- raw %>%
    mutate(gwcode = case_when(
      ccode==255 & year > 1990 ~ 260L,
      ccode==255 & year < 1991 ~ NA_integer_,
      ccode==345 & year > 2005 ~ 340L,
      ccode==679 & year > 1990 ~ 678L,
      ccode==970 ~ 971L,
      ccode==946 ~ 970L,
      ccode==955 ~ 972L,
      ccode==947 ~ 973L,
      ccode==364 ~ 365L,
      TRUE ~ ccode
    )) %>%
    filter(!is.na(gwcode))
  raw
}

lji_make_yearly <- function(path) {
  raw <- lji_get_raw(path)
  cy <- state_panel(start = "1948-12-31", end = "2012-12-31", by = "year") %>%
    mutate(year = year(date))
  
  yearly <- left_join(cy, raw, by = c("gwcode", "year")) %>%
    select(-country, -abbr, -ccode)
  
  mismatches <- anti_join(raw, cy, by = c("gwcode", "year"))
  data(gwstates)
  cnames <- gwstates %>%
    group_by(gwcode) %>% summarize(county = tail(country_name, 1))
  missing <- anti_join(cy, raw, by = c("gwcode", "year")) %>%
    left_join(cnames, by = "gwcode")
  
  yearly
}