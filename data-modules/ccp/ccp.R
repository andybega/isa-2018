

module_path <- "data-modules/ccp"
want_vars <- c("torture", "prerel", "habcorp", "dueproc", "speedtri")

raw <- readr::read_tsv(file.path(module_path, "input/ccpcnc_v2.txt"),
                         col_types = cols(.default = col_character(),
                                          cowcode = col_integer(),
                                          year = col_integer()))
cy <- raw %>%
  rename(gwcode = cowcode) %>%
  mutate(gwcode = as.integer(gwcode),
         year = as.integer(year),
         date = as.Date(paste0(year, "-12-31")))
  
cy <- cy %>%
  mutate(gwcode = case_when(
    gwcode==345 & year >= 2007 ~ 340L,
    TRUE ~ gwcode
  ))
serbia2006 <- filter(cy, gwcode==345 & year==2006) %>%
  mutate(gwcode = 340)
cy <- bind_rows(cy, serbia2006) %>% arrange(gwcode, year)

cy <- cy %>%
  filter(year %in% 1995:2005)
  
gw <- state_panel(start = min(cy$year), end = max(cy$year), useGW = TRUE)
gw$year <- gw$date %>% substr(1, 4) %>% as.integer()

cy_not_in_gw <- anti_join(cy, gw, by = c("gwcode", "year")) %>%
  group_by(gwcode) %>%
  mutate(spell = id_date_sequence(date, "year")) %>%
  group_by(gwcode, spell) %>%
  summarize(years = paste0(range(year), collapse = "-"))

gw_not_in_cy <- anti_join(gw, cy, by = c("gwcode", "year")) %>%
  group_by(gwcode) %>%
  mutate(spell = id_date_sequence(date, "year")) %>%
  group_by(gwcode, spell) %>%
  summarize(years = paste0(range(year), collapse = "-"))

gw$date <- NULL
ccp <- left_join(gw, cy, by = c("gwcode", "year")) %>%
  select("gwcode", "year", "date", want_vars)

# TORTURE - const prohibition on torture; p. 116; 
# 1=always prohibited, 
# 3=prohibited for extracting confession; 
# 96/98=other/not specified
table(is.na(ccp$torture))

ccp %>% 
  group_by(gwcode) %>%
  summarize(torture = paste0(unique(torture), collapse = "; ")) %>%
  group_by(torture) %>%
  summarize(n = n(), countries = paste0(gwcode, collapse = "; "))

ccp <- ccp %>%
  mutate(torture = as.integer(sapply(torture==1, isTRUE)))

# PREREL - pre-trial release; p. 97
# 1
# 2
# 96
table(ccp$prerel)
table(is.na(ccp$prerel))

ccp %>% 
  group_by(gwcode) %>%
  summarize(prerel = paste0(unique(prerel), collapse = "; ")) %>%
  group_by(prerel) %>%
  summarize(n = n(), countries = paste0(gwcode, collapse = "; "))

ccp <- ccp %>%
  mutate(prerel = as.integer(sapply(prerel==1, isTRUE)))

# HABCORP - habeas corpus; p. 97
# 1 Yes
# 2 No
# 96 Other
table(ccp$habcorp)
table(is.na(ccp$habcorp))

ccp %>% 
  group_by(gwcode) %>%
  summarize(habcorp = paste0(unique(habcorp), collapse = "; ")) %>%
  group_by(habcorp) %>%
  summarize(n = n(), countries = paste0(gwcode, collapse = "; "))

ccp <- ccp %>%
  mutate(habcorp = as.integer(sapply(habcorp==1, isTRUE)))

# DUEPROC - due process; p. 99
# 1 Yes
# 2 No
# 96 Other
table(ccp$dueproc)
table(is.na(ccp$dueproc))

ccp %>% 
  group_by(gwcode) %>%
  summarize(dueproc = paste0(unique(dueproc), collapse = "; ")) %>%
  group_by(dueproc) %>%
  summarize(n = n(), countries = paste0(gwcode, collapse = "; "))

ccp <- ccp %>%
  mutate(dueproc = as.integer(sapply(dueproc==1, isTRUE)))

# SPEEDTRI - speedy trial; p. 100
#
#
#
table(ccp$speedtri)
table(is.na(ccp$speedtri))

ccp %>% 
  group_by(gwcode) %>%
  summarize(speedtri = paste0(unique(speedtri), collapse = "; ")) %>%
  group_by(speedtri) %>%
  summarize(n = n(), countries = paste0(gwcode, collapse = "; "))

ccp <- ccp %>%
  mutate(speedtri = as.integer(sapply(speedtri==1, isTRUE)))

write_rds(ccp, path = file.path(module_path, "output/ccp.rds"))
