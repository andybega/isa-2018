

wdi <- function(path = "data-modules/wdi") {
  gdp_pop <- read_csv(file.path(path, "output/wdi-gdp-pop.csv"),
                      col_types = cols(
                        gwcode = col_integer(),
                        NY.GDP.PCAP.KD = col_double(),
                        NY.GDP.MKTP.KD = col_double(),
                        SP.POP.TOTL = col_double(),
                        NY.GDP.PCAP.KD_ln = col_double(),
                        NY.GDP.MKTP.KD_ln = col_double(),
                        SP.POP.TOTL_ln = col_double(),
                        datestr = col_integer()
                      )) %>%
    mutate(date = as.Date(paste0(datestr, "-01-01")))
  gdp_growth <- read_csv(file.path(path, "output/wdi-gdp-growth.csv"),
                         col_types = cols(
                           gwcode = col_integer(),
                           NY.GDP.MKTP.KD.ZG = col_double(),
                           datestr = col_double(),
                           date = col_date(format = "")
                         )) %>%
    mutate(datestr = as.integer(datestr))
  inflation <- read_csv(file.path(path, "output/wdi-inflation.csv"),
                        col_types = cols(
                          gwcode = col_integer(),
                          FP.CPI.TOTL.ZG = col_double(),
                          NY.GDP.DEFL.KD.ZG = col_double(),
                          datestr = col_double(),
                          date = col_date(format = "")
                        )) %>%
    mutate(datestr = as.integer(datestr))
  ict <- read_csv(file.path(path, "output/wdi-ict.csv"),
                  col_types = cols(
                    gwcode = col_integer(),
                    IT.NET.USER.ZS = col_double(),
                    IT.CEL.SETS.P2 = col_double(),
                    date = col_date(format = ""),
                    datestr = col_double()
                  )) %>%
    mutate(datestr = as.integer(datestr))
  infmort = read_csv(file.path(path, "output/wdi-infmort.csv"),
                     col_types = cols(
                       gwcode = col_integer(),
                       year = col_double(),
                       SP.DYN.IMRT.IN = col_double()
                     )) %>%
    mutate(datestr = as.integer(year), year = NULL, 
           date = as.Date(paste0(datestr, "-01-01")))
  wdi <- list(gdp_pop, gdp_growth, inflation, ict, infmort) %>%
    reduce(full_join, by = c("gwcode", "date", "datestr"))
  wdi
}