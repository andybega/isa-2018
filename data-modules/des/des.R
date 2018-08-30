
library("haven")
library("dplyr")
library("stringr")
library("purrr")

PATH_DES <- getSrcDirectory(function(x) x)
if (PATH_DES=="") PATH_DES <- "."

des_get <- function(what = "raw") {
  if (what=="raw") des_get_raw()
  if (what=="yearly") des_make_yearly()
}

des_get_raw <- function(path = PATH_DES) {
  raw <- haven::read_dta(file.path(path, "data/es_data-v3.dta"))
  
  raw[raw$ccode2==345 & raw$year > 2006, "ccode2"] <- 340
  # COW Nauru to GW 
  raw[raw$ccode2==970, "ccode2"] <- 971
  # COW Tuvalu to GW
  raw[raw$ccode2==947, "ccode2"] <- 973
  # COW Kiribati to GW
  raw[raw$ccode2==946, "ccode2"] <- 970

  date_exceptions <- c("unopposed", "date unknown")
  raw <- raw %>%
    mutate_all(as.character) %>%
    mutate(date_orig = date) %>%
    mutate(month = ifelse(month %in% date_exceptions, 7, month),
           day = ifelse(day %in% date_exceptions, 1, day),
           date = ymd(paste(year, month, day, sep = "-"))) %>%
    select(-year, -month, -day) %>%
    rename(gwcode = ccode2)
  
  raw[raw==-99 | raw==-88] <- NA
  suppressWarnings({
    raw <- raw %>% 
      mutate_at(vars(presidential:regime), as.numeric)
    })
  raw <- raw %>%
    mutate(regime = factor(
      regime, levels = 0:5, 
      labels = c("Parliamentary democracy", "Semi-presidential democracy",
                 "Presidential democracy", "Civilian dictatorship", 
                 "Military dictatorship", "Royal dictatorship")))
  raw
}

des_make_yearly <- function(on_day = "12-31") {
  raw <- des_get_raw(PATH_DES) %>%
    dplyr::select(-elec_id, -country, -aclp_code, -ccode, -secondround, 
                  -thirdround, -date_orig, -region1, -region2, -region3)
  
  # make it so there is only one election for given year
  drop_elecs <- function(df, on_day) {
    foo <- df %>%
      # drop elections after on_day
      mutate(year = lubridate::year(date),
             on_day = as.Date(paste0(year, "-", on_day))) %>%
      filter(date <= on_day) %>%
      # keep only last election in multi-election year
      group_by(gwcode, year) %>%
      slice(n())
  }
  
  raw_p <- raw %>% 
    filter(presidential==1) %>%
    `[`(sapply(., function(x) sum(is.na(x))!=length(x))) %>%
    drop_elecs(., on_day) %>%
    dplyr::select(-on_day, -presidential, -date)

  raw_l <- raw %>%
    filter(presidential==0) %>%
    `[`(sapply(., function(x) sum(is.na(x))!=length(x))) %>%
    drop_elecs(., on_day) %>%
    dplyr::select(-on_day, -presidential, -date, -preselecrule) 
  
  raw_wide <- full_join(raw_p, raw_l, by = c("gwcode", "regime", "year")) %>%
    select(gwcode, year, everything())
  
  cy <- state_panel(start = "1945-12-31", end = "2016-12-31", by = "year")
  cy$year <- year(cy$date)
  
  mismatches <- anti_join(raw_wide, cy, by = c("gwcode", "year"))
  if (nrow(mismatches) > 0) stop("Merge problem, check code")
  
  cy <- left_join(cy, raw_wide, by = c("gwcode", "year"))
  cy
}