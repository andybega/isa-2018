
vdem <- function(path = "input/v-dem") {
  vdem <- read_csv(file.path(path, "data/vdem.csv"),
                   col_types = cols(
                     datestr = col_double(),
                     gwcode = col_integer(),
                     v2x_polyarchy = col_double(),
                     v2x_api = col_double(),
                     v2x_mpi = col_double(),
                     v2x_libdem = col_double(),
                     v2x_liberal = col_double(),
                     v2x_partipdem = col_double(),
                     v2x_partip = col_double(),
                     v2x_delibdem = col_double(),
                     v2xdl_delib = col_double(),
                     v2x_egaldem = col_double(),
                     v2x_egal = col_double()
                   ))
  vdem
}