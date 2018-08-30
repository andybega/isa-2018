
# don't attach packages unless in development
# assumes for development, wd is set to module path
if (basename(getwd())=="ucdp-acd") {
  library("dplyr")
  library("readr")
}

# Setup path to module directory, so we can access data stored locally
find_own_path <- function() {
  path <- getSrcDirectory(function(x) {x})
  # sources from RMD
  if (!length(path) > 0) path <- ""
  if (path == "") path <- "."
  path
}
UCDP_ACD_PATH <- find_own_path()
rm(find_own_path)

# API function; everything else should not be called directly by user
ucdp_acd_api <- function(what = "yearly", own_path = UCDP_ACD_PATH) {
  load(file.path(own_path, "output/acd.rda"))
  select(acd, gwcode, year, everything())
}
