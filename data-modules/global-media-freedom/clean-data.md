Check and clean Global Media Freedom data
================
12 August 2019

``` r
gmfd <- read_csv("input/GMFDv3.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   ccode = col_double(),
    ##   country = col_character(),
    ##   year = col_double(),
    ##   mediascore = col_double()
    ## )

``` r
cy   <- read_csv("input/cy-target-set.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   gwcode = col_double(),
    ##   year = col_double()
    ## )

``` r
dplyr::glimpse(gmfd)
```

    ## Observations: 10,492
    ## Variables: 4
    ## $ ccode      <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
    ## $ country    <chr> "United States", "United States", "United States", "U…
    ## $ year       <dbl> 1948, 1949, 1950, 1951, 1952, 1953, 1954, 1955, 1956,…
    ## $ mediascore <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…

``` r
range(gmfd$year)
```

    ## [1] 1948 2016

``` r
range(cy$year)
```

    ## [1] 1995 2005

We can drop the unneeded years right away before checking for
country-year set overlap.

``` r
gmfd <- gmfd %>%
  filter(year %in% unique(cy$year))

gmfd_set <- gmfd[, c("ccode", "year")] %>%
  mutate(in_gmfd = TRUE)
cy_set   <- cy %>%
  mutate(in_cy = TRUE)
full_set <- full_join(gmfd_set, cy_set, 
                      by = c("ccode" = "gwcode", "year" = "year")) %>%
  replace_na(list(in_cy = FALSE, in_gmfd = FALSE))

full_set %>% 
  group_by(in_gmfd, in_cy) %>%
  count()
```

    ## # A tibble: 3 x 3
    ## # Groups:   in_gmfd, in_cy [3]
    ##   in_gmfd in_cy     n
    ##   <lgl>   <lgl> <int>
    ## 1 FALSE   TRUE     22
    ## 2 TRUE    FALSE   468
    ## 3 TRUE    TRUE   1632

Don’t need to worry about (TRUE, FALSE), the (FALSE, TRUE) are a
problem. Cases in `cy` that are missing in GMFD. Check which countries
and years.

``` r
full_set %>%
  filter(in_gmfd==FALSE & in_cy==TRUE) %>%
  group_by(ccode) %>%
  summarize(year = paste0(year, collapse = ";"))
```

    ## # A tibble: 2 x 2
    ##   ccode year                                                  
    ##   <dbl> <chr>                                                 
    ## 1   260 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005
    ## 2   678 1995;1996;1997;1998;1999;2000;2001;2002;2003;2004;2005

Post-1990 Germany is coded as 255 versus 260 in `cy`.

``` r
gmfd %>% filter(ccode==255)
```

    ## # A tibble: 11 x 4
    ##    ccode country  year mediascore
    ##    <dbl> <chr>   <dbl>      <dbl>
    ##  1   255 Germany  1995          1
    ##  2   255 Germany  1996          1
    ##  3   255 Germany  1997          1
    ##  4   255 Germany  1998          1
    ##  5   255 Germany  1999          1
    ##  6   255 Germany  2000          1
    ##  7   255 Germany  2001          1
    ##  8   255 Germany  2002          1
    ##  9   255 Germany  2003          1
    ## 10   255 Germany  2004          1
    ## 11   255 Germany  2005          1

Yemen is coded as 678 versus 679 in `cy`.

``` r
gmfd %>% filter(ccode==679)
```

    ## # A tibble: 11 x 4
    ##    ccode country  year mediascore
    ##    <dbl> <chr>   <dbl>      <dbl>
    ##  1   679 Yemen    1995          3
    ##  2   679 Yemen    1996          3
    ##  3   679 Yemen    1997          3
    ##  4   679 Yemen    1998          3
    ##  5   679 Yemen    1999          3
    ##  6   679 Yemen    2000          3
    ##  7   679 Yemen    2001          3
    ##  8   679 Yemen    2002          3
    ##  9   679 Yemen    2003          3
    ## 10   679 Yemen    2004          3
    ## 11   679 Yemen    2005          3

Easy peasy.

``` r
gmfd <- gmfd %>%
  mutate(ccode = case_when(
    ccode==255L ~ 260L,
    ccode==679L ~ 678L,
    TRUE ~ as.integer(ccode)
  )) %>%
  rename(gwcode = ccode) %>%
  mutate(in_gmfd = TRUE) %>%
  full_join(cy_set, by = c("gwcode", "year")) %>%
  filter(in_gmfd | in_cy)
```

Check there are no missing left.

``` r
check <- gmfd %>%
  group_by(in_gmfd, in_cy) %>%
  count()

if (any(is.na(gmfd$in_gmfd))) {
  stop("GMFD is missing some needed cases")
}
```

We can drop the excess GMFD cases.

``` r
gmfd <- gmfd %>%
  filter(in_cy) %>%
  select(-in_gmfd, -in_cy, -country)
```

The dataset authors recommend collapsing to a dichotomous “functinally
free” version if used as IV. Do that.

``` r
gmfd <- gmfd %>%
  mutate(gmfd_functionallyfree = as.integer(mediascore < 3)) %>%
  select(-mediascore)
```

``` r
write_csv(gmfd, "output/gmfd.csv")
```
