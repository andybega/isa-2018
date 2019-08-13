

library("tidyverse")
library("nlme")
library("lme4")
library("broom")
library("futile.logger")

source("R/functions.R")

cy <- readRDS("output/cy.rds")

# Build up model / spec list ----------------------------------------------

# DV's
# this is for trimming the model object size below
dv <- c(
  "itt_alleg_vtcriminal", "itt_alleg_vtdissident", "itt_alleg_vtmarginalized"
)

# Setup list of variables of interest
voi <- c(
  c("ccp_torture", "ccp_prerel", "ccp_habcorp", "ccp_dueproc", "ccp_speedtri"),
  c("v2x_jucon", "v2xlg_legcon", "v2clacjust", "v2clsocgrp", "v2pepwrses", "v2pepwrsoc"),
  c("epr_excluded_groups_count", "epr_excluded_group_pop",
    "dd_democracy")
)

# Basic spec
base_spec <- c(
  "Intercept(s) only" = "(1|gwcode) + 1", 
  "Basic controls"    = "(1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess"
  )

# Which models to run?
model_type <- c(
  "Poisson w RE (GLMER)" = "glmer_pois", 
  "Poisson (GLM)" = "glm_pois")

# Media freedom
press_free <- c(
  "None" = "",
  "Funct. free" = "gmfd_functionallyfree"
)

# Political globalization - IGO membership
pol_glob <- c(
  "None" = "",
  "IGO num" = "igo_n",
  "IGO fraction" = "igo_p"
)

# Year trend
year_trend <- c(
  "None"    = "", 
  "Linear"  = "year_poly1", 
  "Squared" = "year_poly1 + year_poly2"
)

# HRO
hro <- c(
  "None" = "",
  "HRO Number" = "hro_n",
  "HRO HQs" = "hro_secloc"
)

econ_glob <- c(
  "None" = "",
  "Trade, % of GDP" = "norm_ln_NE.TRD.GNFS.ZS"
)

# For plotting, it helps to preserve the order of these things
spec_element_factor_levels <- c(
  voi, names(model_type), rev(names(base_spec)), names(press_free), names(pol_glob),
  names(year_trend), names(hro), names(econ_glob))
dput(spec_element_factor_levels, "output/spec_element_factor_levels.txt")

# make sure to add these in the for loop below, too
model_list <- crossing(
  voi        = voi,
  base_spec  = names(base_spec),
  press_free = names(press_free),
  pol_glob   = names(pol_glob),
  year_trend = names(year_trend),
  # these variables have missing values, don't use
  #hro        = names(hro),
  #econ_glob  = names(econ_glob),
  model      = names(model_type)
) %>%
  mutate(id = 1:n(),
         model_func_name = model_type[model],
         spec = NA_character_) %>%
  select(id, everything())

# safely add a term without putting a + for "" empty terms
add_term <- function(spec, x) {
  lh_only <- str_ends(str_trim(spec), "~")
  good_x  <- x!=""
  if (lh_only & good_x) spec <- paste0(spec, x)
  if (!lh_only & good_x) spec <- paste0(spec, " + ", x)
  spec
}

# Fill in the actual R specification to use
for (i in 1:nrow(model_list)) {
  row_i <- model_list[i, ]
  pieces <- c(
    row_i$voi, 
    base_spec[row_i$base_spec],
    press_free[row_i$press_free],
    pol_glob[row_i$pol_glob],
    year_trend[row_i$year_trend]#,
    #hro[row_i$hro],
    #econ_glob[row_i$econ_glob]
  )
  spec_str <- Reduce(add_term, pieces, "~ ")
  # Take out RE in spec if glm model
  if (str_detect(row_i$model_func_name, "glm_")) {
    spec_str <- str_replace(spec_str, " \\(1\\|gwcode\\) \\+", "")
  }
  model_list$spec[i] <- spec_str
}

write_rds(model_list, "output/models/model_list.rds")


# Estimate all models -----------------------------------------------------

get_terms <- function(x) {
  x <- x %>% 
    str_split(" \\+ ") %>% 
    `[[`(1) %>% 
    str_remove("~") %>% 
    str_trim() %>%
    str_replace("\\(1\\|gwcode\\)", "gwcode")
  x <- x[x!="1"]
  x
}

N_models <- nrow(model_list)
for (i in 1:N_models) {
  flog.info("Specification %s of %s (%s%%)", i, N_models, round(i/N_models*100, 1))
  
  spec_i <- model_list$spec[i]
  
  # trim training data to reduce model size
  vars <- c(get_terms(spec_i), dv)
  cy_i <- cy[, vars]
  # some of the robustness vars are missing cases, so subset as needed
  #cy_i <- cy[complete.cases(cy), ]
  
  fh  <- sprintf("output/models/spec_%s.rds", model_list$id[i])

  mdl <- fit_model(spec_i, cy_i, model_list$model_func_name[i])
  write_rds(mdl, fh)
}
flog.info("Estimations complete")



