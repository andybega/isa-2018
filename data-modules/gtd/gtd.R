
# don't attach packages unless in development
# assumes for development, wd is set to module path
if (basename(getwd())=="gtd") {
  library("dplyr")
  library("readxl")
  library("countrycode")
  library("states")
  library("tidyr")
}

# Setup path to module directory, so we can access data stored locally
find_own_path <- function() {
  path <- getSrcDirectory(function(x) {x})
  # sources from RMD
  if (!length(path) > 0) path <- ""
  if (path == "") path <- "."
  path
}
GTD_PATH <- find_own_path()
rm(find_own_path)

# API function; everything else should not be called directly by user
gtd_api <- function(what = "raw", own_path = GTD_PATH) {
  raw <- readxl::read_xlsx(file.path(own_path, "data/globalterrorismdb_0718dist.xlsx"))
  
  if (what=="raw") {
    return(raw)
  }
  
  if (what=="yearly") {
    raw <- raw %>%
      mutate(gwcode = countrycode::countrycode(country_txt, "country.name", "cown", 
                                               warn = FALSE)) %>%
      mutate(gwcode = case_when(
        gwcode==255 ~ 260L,
        gwcode==679 ~ 678L,
        country_txt=="Serbia-Montenegro" ~ 345L,
        country_txt=="Serbia" ~ 340L,
        country_txt=="French Guiana" ~ 220L,
        country_txt=="Guadeloupe" ~ 220L,
        country_txt=="Martinique" ~ 220L,
        country_txt=="New Caledonia" ~ 220L,
        country_txt=="French Polynesia" ~ 220L,
        country_txt=="Wallis and Futuna" ~ 220L,
        country_txt=="Western Sahara" ~ 600L,
        country_txt=="Falkland Islands" ~ 200L,
        gwcode==817 ~ 816L,
        # Papal States to Italy
        gwcode==327 ~ 325L,
        # Namibia pre-1990 to RSA
        gwcode==565 & iyear < 1990 ~ 560L,
        is.na(gwcode) ~ 9999L,
        TRUE ~ gwcode
      ))
    
    # # cases that have missing gwcode
    # unique(raw$country_txt[is.na(raw$gwcode)])
    
    yearly <- raw %>%
      rename(year = iyear) %>%
      group_by(gwcode, year) %>%
      summarize(gtd_events = n(),
                gtd_killed = sum(nkill, na.rm = TRUE))
    
    master <- state_panel(start = as.Date(sprintf("%s-01-01", min(yearly$year))),
                          end = as.Date(sprintf("%s-01-01", max(yearly$year))),
                          partial = "any", useGW = TRUE) %>%
      mutate(year = as.integer(substr(date, 1, 4)))
    
    # gtd_not_in_gw <- anti_join(yearly, master, by = c("gwcode", "year")) %>%
    #   group_by(gwcode) %>%
    #   arrange(year) %>%
    #   mutate(date = as.Date(sprintf("%s-01-01", year))) %>%
    #   mutate(seq_id = id_date_sequence(date, "year")) %>%
    #   group_by(gwcode, seq_id) %>%
    #   summarize(year = paste0(year, collapse = "; "),
    #             rows = n())
    # gtd_not_in_gw
    # 
    # gw_not_in_gtd <- anti_join(master, yearly, by = c("gwcode", "year")) %>%
    #   group_by(gwcode) %>%
    #   arrange(year) %>%
    #   mutate(seq_id = id_date_sequence(date, "year")) %>%
    #   group_by(gwcode, seq_id) %>%
    #   summarize(year = paste0(year, collapse = "; "),
    #             rows = n())
    # gw_not_in_gtd
    # # Can probably assume that cases where no GTD for a country-year, it was
    # # because of absence of attacks
    # gw_not_in_gtd %>%
    #   group_by(gwcode) %>%
    #   summarize(rows = sum(rows))
    
    master <- left_join(master, yearly, by = c("gwcode", "year")) %>%
      dplyr::select(-date) %>%
      tidyr::replace_na(list(gtd_events = 0L, gtd_killed = 0L))
    
    return(master)
  }
  
  invisible(NULL)
}
