
library("tidyverse")
library("states")

itt <- function() {
  cyvt <- read_csv("input/itt/data/CYVT.csv")
  data(gwstates)
  
  cy   <- state_panel("1995-01-01", "2005-01-01", by = "year", partial = "any")
  cy$year <- lubridate::year(cy$date)
  
  cyvt <- cyvt %>% 
    mutate(gwcode = case_when(
      cowccode==255 ~ 260L,
      cowccode==679 ~ 678L,
      TRUE ~ as.integer(cowccode)
    )) %>%
    select(-cowccode, -iso3numeric, -iso3alpha) 
    
  cyvt <- left_join(cy, cyvt, by = c("gwcode", "year"))
  missing_data <- cyvt %>% 
    filter(is.na(LoTUnknown)) %>% 
    group_by(gwcode) %>% 
    summarize(year1 = min(year), year2 = max(year)) %>%
    left_join(., gwstates, by = c("gwcode"))
  cyvt
}


