
library("vdem")
library("dplyr")
library("tidyr")
library("ggplot2")
library("readr")

vdem_get <- function(path = "input/v-dem") {
  vdem <- vdem::extract_vdem(section_number = 1)
  
  vdem <- vdem %>%
    select(-vdem_country_id, -vdem_country_text_id, -extended_country_name, 
           -historical_date, -codingstart, -gapstart, -gapend, -codingend, 
           -vdem_cown, -cown, -GW_startdate, -GW_enddate, -GWc, -extended_region,
           -extended_continent, -microstate, -lat, -lon, -in_GW_system) %>%
    select(-contains("codelow"), -contains("codehigh"))
  
  vdem <- vdem %>%
    dplyr::rename(gwcode = GWn, datestr = year) %>%
    mutate(vdem_country_name = NULL)
  
  #write_csv(vdem, path = "output/vdem.csv")
  vdem
}