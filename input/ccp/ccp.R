
library("tidyverse")
library("states")
library("readxl")

PATH_CCP <- getSrcDirectory(function(x) x)
if (PATH_CCP=="") PATH_CCP <- path <- "input/ccp"


ccp_get <- function(what = "yearly") {
  if (what=="yearly") return(ccp_get_yearly())
}

ccp_get_raw <- function(path = PATH_CCP) {
  raw <- readr::read_tsv(file.path(path, "data/ccpcnc_v2.txt"),
                         col_types = cols(.default = col_character()))
}