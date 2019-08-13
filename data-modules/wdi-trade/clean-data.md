WDI trade for economic globalizationt
================
13 August 2019

``` r
trade  <- read_csv("input/wdi-trade.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   gwcode = col_double(),
    ##   year = col_double(),
    ##   NE.TRD.GNFS.ZS = col_double()
    ## )

``` r
cy   <- read_csv("input/cy-target-set.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   gwcode = col_double(),
    ##   year = col_double()
    ## )

## Set overlap

Check country-year ID set overlap before merge.

``` r
trade <- trade %>%
  filter(year %in% unique(cy$year),
         !is.na(NE.TRD.GNFS.ZS)) 

trade_set <- trade[, c("gwcode", "year")] %>%
  mutate(in_trade = TRUE)
cy_set   <- cy %>%
  mutate(in_cy = TRUE)
full_set <- full_join(trade_set, cy_set, 
                      by = c("gwcode", "year")) %>%
  replace_na(list(in_cy = FALSE, in_trade = FALSE))

full_set %>% 
  group_by(in_trade, in_cy) %>%
  count()
```

    ## # A tibble: 3 x 3
    ## # Groups:   in_trade, in_cy [3]
    ##   in_trade in_cy     n
    ##   <lgl>    <lgl> <int>
    ## 1 FALSE    TRUE    102
    ## 2 TRUE     FALSE   284
    ## 3 TRUE     TRUE   1552

Don’t need to worry about (TRUE, FALSE), the (FALSE, TRUE) are a
problem. Cases in `cy` that are missing in trade. Check which countries
and years.

``` r
full_set %>%
  filter(in_trade==FALSE & in_cy==TRUE) %>%
  group_by(gwcode) %>%
  summarize(year = paste0(year, collapse = ";"))
```

    ## # A tibble: 13 x 2
    ##    gwcode year                                                  
    ##     <dbl> <chr>                                                 
    ##  1     52 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005
    ##  2    115 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005
    ##  3    450 1995;1996;1997;1998;1999                              
    ##  4    520 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005
    ##  5    530 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005
    ##  6    540 1995;1996;1997;1998;1999                              
    ##  7    570 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005
    ##  8    678 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005
    ##  9    700 1995;1996;1997;1998;1999;2000;2001                    
    ## 10    704 1995;1996                                             
    ## 11    731 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005
    ## 12    775 1995;1996;1997;1998;1999                              
    ## 13    910 2005

All good\!

``` r
trade <- trade %>%
  mutate(in_trade = TRUE) %>%
  full_join(cy_set, by = c("gwcode", "year")) %>%
  filter(in_trade | in_cy)
```

``` r
check <- trade %>%
  group_by(in_trade, in_cy) %>%
  count()

if (any(is.na(trade$in_trade))) {
  warning("trade is missing some needed cases")
}
```

    ## Warning: trade is missing some needed cases

We can drop the excess WDI cases.

``` r
trade <- trade %>%
  filter(in_cy) %>%
  select(-in_trade, -in_cy)
```

``` r
write_csv(trade, "output/trade.csv")
```
