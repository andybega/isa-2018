
# don't attach packages unless in development
# assumes for development, wd is set to module path
if (basename(getwd())=="epr") {
  library("dplyr")
  library("readr")
  library("tidyr")
  library("states")
}

# Setup path to module directory, so we can access data stored locally
find_own_path <- function() {
  path <- getSrcDirectory(function(x) {x})
  # sources from RMD
  if (!length(path) > 0) path <- ""
  if (path == "") path <- "."
  path
}
EPR_PATH <- find_own_path()
rm(find_own_path)

# API function; everything else should not be called directly by user
epr_api <- function(what = "raw", own_path = EPR_PATH) {
  
  if (!dir.exists(file.path(own_path, "data"))) {
    dir.create(file.path(own_path, "data"))
  }
  
  if (!file.exists(file.path(own_path, "data/EPR-2018.csv"))) {
    epr_download(own_path)
  }
  
  if (what=="raw") {
    return(epr_get_raw(own_path))
  }
  if (what=="yearly") {
    return(epr_make_yearly(own_path))
  }
  invisible(NULL)
}

epr_download <- function(own_path) {
  url <- "https://icr.ethz.ch/data/epr/core/EPR-2018.csv"
  download.file(url, destfile = file.path(own_path, "data", basename(url)))
  invisible(TRUE)
}

epr_get_raw <- function(own_path) {
  raw <- read_csv(file.path(own_path, "data/EPR-2018.csv"),
                  col_types = cols(
                    gwid = col_integer(),
                    statename = col_character(),
                    from = col_integer(),
                    to = col_integer(),
                    group = col_character(),
                    groupid = col_integer(),
                    gwgroupid = col_integer(),
                    umbrella = col_character(),
                    size = col_double(),
                    status = col_character(),
                    reg_aut = col_character()
                  ))
  raw <- raw %>%
    rename(gwcode = gwid) %>%
    mutate(reg_aut = case_when(
      is.na(reg_aut) ~ "missing",
      TRUE ~ reg_aut
    ))
  raw
}

epr_make_yearly <- function(own_path) {
  raw <- epr_get_raw(own_path)
  
  # convert to country-year
  epr <- raw %>%
    dplyr::mutate(year = 1946) %>%
    tidyr::complete(year = 1946:2013, nesting(gwcode, from, to, group, groupid, 
                                              size, status, reg_aut)) %>%
    dplyr::filter(year >= from & year <= to)
  
  # not clear about irrelevant and state collapse
  excl_val <- c("POWERLESS", "DISCRIMINATED", "SELF-EXCLUSION")
  incl_val <- c("MONOPOLY", "DOMINANT", "JUNIOR PARTNER", "SENIOR PARTNER", 
                "IRRELEVANT", "STATE COLLAPSE")
 
  epr <- epr %>%
    group_by(gwcode, year) %>%
    dplyr::summarize(
      epr_groups = n(),
      epr_elf    = sum(size^2),
      epr_excluded_groups_count = sum(status %in% excl_val),
      epr_excluded_group_pop    = sum(subset(size, status %in% excl_val)),
      epr_inpower_groups_count  = sum(status %in% incl_val),
      epr_inpower_groups_pop    = sum(subset(size, status %in% incl_val)),
      epr_regaut_groups_count = sum(reg_aut=="true"),
      epr_regaut_group_pop    = sum(subset(size, reg_aut=="true"))
    )
  
  master <- state_panel(as.Date(paste0(min(epr$year), "-01-01")),
                        as.Date(paste0(max(epr$year), "-01-01")),
                        by = "year", partial = "any")
  master$year = as.integer(substr(master$date, 1, 4))
  
  data(gwstates)
  cnames <- gwstates %>% 
    select(gwcode, country_name) %>% 
    group_by(gwcode) %>% 
    dplyr::slice(1) %>% ungroup()
  
  master_not_in_epr <- anti_join(master, epr, by = c("gwcode", "year")) %>%
    group_by(gwcode, date) %>%
    mutate(spell = id_date_sequence(date, "year")) %>%
    group_by(gwcode, spell) %>%
    summarize(years = paste(range(year), collapse = " - ")) %>%
    left_join(cnames, by = "gwcode")
  
  epr_not_in_master <-  anti_join(epr, master, by = c("gwcode", "year")) %>%
    mutate(date = as.Date(paste0(year, "-01-01"))) %>%
    group_by(gwcode, date) %>%
    mutate(spell = id_date_sequence(date, "year")) %>%
    group_by(gwcode, spell) %>%
    summarize(years = paste(range(year), collapse = " - "))
  
  master <- left_join(master, epr, by = c("gwcode", "year"))
  master
}

epr_legacy_csv <- function(own_path) {
  epr <- epr_make_yearly(own_path)
  epr$datestr <- epr$year
  epr$date <- as.Date(sprintf("%s-12-31", epr$datestr))
  epr$year <- NULL
  
  write.csv(epr, file = file.path(own_path, "data/epr.csv"), row.names = FALSE)
  invisible(NULL)
}
