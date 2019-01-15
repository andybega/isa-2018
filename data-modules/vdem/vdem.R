
library("vdem")
library("dplyr")
library("tidyr")
library("ggplot2")
library("readr")

# v2x_elecoff - elected officials 
# v2xel_frefair - clean elections 
# v2asuffrage - suffrage 
# v2x_jucon - judicial constraints 
# v2xlg_legcon - legislative constraints 

# v2clacjust - poor/rich have same civil libs
# v2clsocgrp - social groups (ethnic) have same civil libs
# v2clsnlpct - % that live in geographic areas where civ lib protection is weaker
# v2pepwrses - poor/rich have same power/influence
# v2pepwrsoc - social groups have same power/influence

module_path <- "data-modules/vdem"
want_vars <- c(
  c("v2x_elecoff", "v2xel_frefair", "v2asuffrage", "v2x_jucon", "v2xlg_legcon"),
  c("v2clacjust", "v2clsocgrp", "v2clsnlpct", "v2pepwrses", "v2pepwrsoc")
)

vdem <- full_join(
  vdem::extract_vdem(section_number = 2),
  vdem::extract_vdem(section_number = 3),
) %>%
  full_join(vdem::extract_vdem(name_pattern = "v2cl")) %>%
  full_join(vdem::extract_vdem(name_pattern = "v2pep"))
want_vars %in% names(vdem)

# There are some missing GW codes; can disregard
filter(vdem, is.na(GWn)) %>% pull(vdem_country_name) %>% unique()

vdem <- vdem %>%
  filter(year %in% 1995:2005) %>%
  filter(!is.na(GWn)) %>%
  select(GWn, year, want_vars) %>%
  rename(gwcode = GWn) %>%
  mutate(date = as.Date(paste0(year, "-12-31")),
         year = as.integer(year))

gw <- state_panel(min(vdem$year), max(vdem$year), useGW = TRUE) %>%
  mutate(year = as.integer(substr(date, 1, 4)), date = NULL)

gw_not_in_vdem <- anti_join(gw, vdem, by = c("gwcode", "year")) %>%
  group_by(gwcode) %>%
  summarize(year = paste0(range(year), collapse = " - "))
gw_not_in_vdem

vdem_not_in_gw <- anti_join(vdem, gw, by = c("gwcode", "year")) %>%
  group_by(gwcode) %>%
  summarize(year = paste0(range(year), collapse = " - "))
vdem_not_in_gw

vdem <- left_join(gw, vdem, by = c("gwcode", "year")) %>%
  mutate(date = as.Date(paste0(year, "-12-31")))

plot_missing(vdem, "v2x_elecoff", partial = "any", statelist="GW")
plot_missing(vdem, "v2clsnlpct", partial = "any", statelist="GW")
ggplot(vdem, aes(x = date, group = gwcode, y = v2clsnlpct)) +
  geom_line() +
  theme_minimal()

# should all be missing same # of cases
sapply(vdem, function(x) sum(is.na(x)))

write_rds(vdem, path = file.path(module_path, "output/vdem.rds"))


