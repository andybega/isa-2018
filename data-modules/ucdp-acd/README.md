UCDP/PRIO Armed Conflict Data
================

A template for data modules.

Usage
-----

``` r
library("dplyr")
```

    ## Warning: package 'dplyr' was built under R version 3.5.1

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("readr")

source("ucdp-acd.R")

example <- ucdp_acd_api()
str(example)
```

    ## 'data.frame':    10925 obs. of  17 variables:
    ##  $ gwcode                   : int  2 20 40 41 42 70 90 91 92 93 ...
    ##  $ year                     : num  1946 1946 1946 1946 1946 ...
    ##  $ internal_confl           : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ internal_confl_major     : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ internal_confl_minor     : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ internal_confl_part      : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ internal_confl_part_major: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ internal_confl_part_minor: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ war                      : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ war_major                : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ war_minor                : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ any_conflict             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ any_conflict_major       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ any_conflict_minor       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ext_conf                 : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ext_conf_major           : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ext_conf_minor           : num  0 0 0 0 0 0 0 0 0 0 ...

``` r
range(example$year)
```

    ## [1] 1946 2016
