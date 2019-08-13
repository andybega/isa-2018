Human rights orgs
================
13 August 2019

``` r
hro  <- read_csv("input/murdie_davis_isq_2010.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   NAMES_STD = col_character(),
    ##   country = col_character(),
    ##   countrycode = col_character(),
    ##   ctry = col_character(),
    ##   cname = col_character(),
    ##   ucdp_loc = col_character(),
    ##   ucdp_type1 = col_character(),
    ##   ucdp_type2 = col_character(),
    ##   ucdp_type3 = col_character(),
    ##   ucdp_type4 = col_character(),
    ##   GEOcow = col_character(),
    ##   GEOun = col_character(),
    ##   GEO = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
cy   <- read_csv("input/cy-target-set.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   gwcode = col_double(),
    ##   year = col_double()
    ## )

Use “hrfilled” and “HROsecretariatlocation”.

``` r
hro <- hro %>%
  select(cowcode, year, hrfilled, HROsecretariatlocation) %>%
  rename(gwcode = cowcode) %>%
  filter(year %in% unique(cy$year))
```

Fix some country code divergences. This dataset is tricky because it
tends to have all country codes, from both COW and G\&W, e.g. it has 678
and 679, but one has missing values for the observations.

``` r
hro <- hro %>%
  # Yemen
  filter(gwcode != 678) %>%
  mutate(gwcode = ifelse(gwcode==679, 678, gwcode)) 
```

## Set overlap

Check country-year ID set overlap before merge.

``` r
hro_set <- hro[, c("gwcode", "year")] %>%
  mutate(in_hro = TRUE)
cy_set   <- cy %>%
  mutate(in_cy = TRUE)
full_set <- full_join(hro_set, cy_set, 
                      by = c("gwcode", "year")) %>%
  replace_na(list(in_cy = FALSE, in_hro = FALSE))

full_set %>% 
  group_by(in_hro, in_cy) %>%
  count()
```

    ## # A tibble: 2 x 3
    ## # Groups:   in_hro, in_cy [2]
    ##   in_hro in_cy     n
    ##   <lgl>  <lgl> <int>
    ## 1 TRUE   FALSE   991
    ## 2 TRUE   TRUE   1654

Don’t need to worry about (TRUE, FALSE), the (FALSE, TRUE) are a
problem. Cases in `cy` that are missing in hro. Check which countries
and years.

``` r
full_set %>%
  filter(in_hro==FALSE & in_cy==TRUE) %>%
  group_by(gwcode) %>%
  summarize(year = paste0(year, collapse = ";"))
```

    ## # A tibble: 0 x 2
    ## # … with 2 variables: gwcode <dbl>, year <chr>

Cases are good, but there are hidden values.

``` r
hro <- hro %>%
  mutate(in_hro = TRUE) %>%
  full_join(cy_set, by = c("gwcode", "year")) %>%
  filter(in_hro | in_cy)
```

There are missing values for both indicators, aside from the
country-year set match. Check each. “hrfilled” is missing 2004 and 2005,
don’t consider those.

``` r
hro %>%
  filter(in_cy, is.na(hrfilled)) %>%
  filter(year < 2004) %>%
  group_by(gwcode) %>%
  summarize(years = paste0(year, collapse = ";")) %>%
  knitr::kable()
```

| gwcode | years                                        |
| -----: | :------------------------------------------- |
|     52 | 1995;1996;2003                               |
|     91 | 2003                                         |
|    140 | 2003                                         |
|    160 | 1995;1996;1997;1998;1999;2000;2001;2002;2003 |
|    165 | 2003                                         |
|    205 | 2003                                         |
|    230 | 2003                                         |
|    305 | 1999;2000;2001;2002                          |
|    339 | 2003                                         |
|    343 | 1995;1996;1997;1998;1999;2000;2001;2002;2003 |
|    345 | 2000                                         |
|    346 | 1995;1996;1997;1998;1999;2000;2001;2002;2003 |
|    350 | 2003                                         |
|    359 | 1995;1996;1997;1998;1999;2000;2001;2002;2003 |
|    368 | 2003                                         |
|    369 | 2003                                         |
|    373 | 2003                                         |
|    385 | 2003                                         |
|    450 | 1995                                         |
|    451 | 1997;1998;1999;2000                          |
|    490 | 1995;1996;1997;1998;1999;2000                |
|    520 | 1995;1996;1997;1998;1999;2000;2001;2002;2003 |
|    531 | 1995;1996;1997;1998;1999;2000;2001;2002;2003 |
|    570 | 1998                                         |
|    630 | 2003                                         |
|    645 | 2003                                         |
|    660 | 1995;1996;1997;1998;1999;2000                |
|    666 | 1995                                         |
|    701 | 1995;1996;1997;1998;1999;2000;2001;2002;2003 |
|    703 | 1995;1996;1997;1998;1999;2000;2001;2002;2003 |
|    732 | 1995;1996;1997;1998;1999;2000;2001;2002;2003 |
|    740 | 2003                                         |
|    775 | 2003                                         |
|    812 | 1995;1996;1997;1998;1999;2000;2001;2002;2003 |
|    816 | 1995;1996;1997;1998;1999;2000;2001;2002;2003 |
|    860 | 2002;2003                                    |

Now for “HROsecretariatlocation”:

``` r
hro %>%
  filter(in_cy, is.na(HROsecretariatlocation)) %>%
  group_by(gwcode) %>%
  summarize(years = paste0(year, collapse = ";")) %>%
  knitr::kable()
```

| gwcode | years                                                  |
| -----: | :----------------------------------------------------- |
|     52 | 1995;1996;2003                                         |
|     91 | 2003                                                   |
|     92 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    115 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    140 | 2003                                                   |
|    160 | 1995;1996;1997;1998;1999;2000;2001;2002;2003           |
|    165 | 2003                                                   |
|    205 | 2003;2004                                              |
|    230 | 2003                                                   |
|    305 | 1999;2000;2001;2002;2004                               |
|    339 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    345 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    346 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    350 | 2003;2004                                              |
|    359 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    366 | 2004                                                   |
|    368 | 2003                                                   |
|    369 | 2003                                                   |
|    371 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    373 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    380 | 2004                                                   |
|    385 | 2003                                                   |
|    390 | 2004                                                   |
|    435 | 2002;2003;2004;2005                                    |
|    450 | 1995                                                   |
|    451 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    483 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    490 | 1995;1996;1997;1998;1999;2000                          |
|    516 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    517 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    520 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    531 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    540 | 2002;2003;2004;2005                                    |
|    553 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    570 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    571 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    572 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    580 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    630 | 2003                                                   |
|    645 | 2003                                                   |
|    660 | 1995;1996;1997;1998;1999;2000                          |
|    666 | 1995                                                   |
|    678 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    694 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    698 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    700 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    701 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    702 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    703 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    704 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    705 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    712 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    731 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    740 | 2003                                                   |
|    775 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    811 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    812 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    816 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    860 | 2002;2003;2004;2005                                    |
|    910 | 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005 |
|    920 | 2004                                                   |

We can drop the excess HRO cases.

``` r
hro <- hro %>%
  filter(in_cy) %>%
  select(-in_hro, -in_cy) %>%
  rename(hro_n = hrfilled,
         hro_secloc = HROsecretariatlocation)
```

``` r
write_csv(hro, "output/hro.csv")
```
