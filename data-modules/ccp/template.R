
# don't attach packages unless in development
# assumes for development, wd is set to module path
if (basename(getwd())=="template") {
  library("dplyr")
}

# Setup path to module directory, so we can access data stored locally
find_own_path <- function() {
  path <- getSrcDirectory(function(x) {x})
  # sources from RMD
  if (!length(path) > 0) path <- ""
  if (path == "") path <- "."
  path
}
TEMPLATE_PATH <- determine_path()
rm(find_own_path)

# API function; everything else should not be called directly by user
template_api <- function(what = "raw") {
  NULL
}
