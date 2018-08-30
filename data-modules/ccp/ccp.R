

# don't attach packages unless in development
# assumes for development, wd is set to module path
if (basename(getwd())=="ccp") {
  library("tidyverse")
  library("states")
  library("readxl")
}

# Setup path to module directory, so we can access data stored locally
find_own_path <- function() {
  path <- getSrcDirectory(function(x) {x})
  # sources from RMD
  if (!length(path) > 0) path <- ""
  if (path == "") path <- "."
  path
}
CCP_PATH <- find_own_path()
rm(find_own_path)

# API function; everything else should not be called directly by user
ccp_api <- function(what = "yearly") {
  if (what=="yearly") return(ccp_get_yearly())
}

ccp_get_raw <- function(path = CCP_PATH) {
  raw <- readr::read_tsv(file.path(path, "data/ccpcnc_v2.txt"),
                         col_types = cols(.default = col_character()))
  
  # convert 1/2/96/97 value columns into logical with NA
  logical_converter <- function(x) {
    xvals <- unique(x)
    has1and2 <- all(c(1, 2) %in% xvals)
    rightset <- all(xvals %in% c(1, 2, 96:99))
  }
}

ccp_get_yearly <- function(path = CCP_PATH) {
  raw <- ccp_get_raw(path = path)
  raw <- raw %>%
    rename(gwcode = cowcode) %>%
    mutate(gwcode = as.integer(gwcode),
           year = as.integer(year))
  
  raw <- raw %>%
    mutate(gwcode = case_when(
      gwcode==345 & year >= 2007 ~ 340L,
      TRUE ~ gwcode
    ))
  
  master <- state_panel(start = "1816-01-01", end = as.Date(sprintf("%s-01-01", max(raw$year))),
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