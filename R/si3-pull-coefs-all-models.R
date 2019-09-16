#
#   Create a table of the coefficient estimates from all specifications
#
#   14 August 2019
#
#   Depends on s1-estimate-all-models.R
#

library("nlme")
library("lme4")
library("dplyr")
library("tidyr")
library("broom")  # for tidy
library("readr")
library("cowplot")
library("tibble")
library("futile.logger")

source("R/functions.R")


# Setup list of variables of interest
voi <- c(
  c("dd_democracy"),
  sort(c("ccp_torture", 
         "ccp_prerel", 
         "ccp_habcorp", 
         "ccp_dueproc", 
         "ccp_speedtri")),
  c("epr_excluded_groups_count" = "norm_sqrt_epr_excluded_group_pop", 
    "epr_excluded_group_pop" = "norm_ln1p_epr_excluded_groups_count"),
  sort(c("v2x_jucon", 
         "v2xlg_legcon", 
         "v2clacjust", 
         "v2clsocgrp", 
         "v2pepwrses", 
         "v2pepwrsoc"))
)
names(voi)[names(voi)==""] <- voi[names(voi)==""]

model_list <- read_rds("output/models/model_list.rds")

coefs_i <- lapply(model_list$id, function(id) {
  fh  <- sprintf("output/models/spec_%04d.rds", model_list$id[id])
  mdl <- read_rds(fh)
  
  voi_coef <- tidy(mdl) %>%
    as_tibble() %>%
    select(y, term, estimate, std.error, p.value) %>%
    filter(term %in% voi) %>%
    mutate(id = id)
  voi_coef
})
coefs <- bind_rows(coefs_i)
coefs <- coefs %>%
  mutate(
    direction = case_when(
      estimate < 0 & p.value < 0.05 ~ "neg",
      estimate > 0 & p.value < 0.05 ~ "pos",
      estimate < 0 & p.value > 0.05 ~ "leanneg",
      estimate > 0 & p.value > 0.05 ~ "leanpos"
    ),
    direction = factor(direction, levels = c("pos", "leanpos", "leanneg", "neg"))
  )
coefs <- coefs %>%
  # the term can sometimes be different from the VOI name, so use model_list
  # which has the VOI name, not indicator name
  select(-term) %>%
  left_join(model_list %>% select(-spec, -model_func_name, voi), by = "id")

write_rds(coefs, "output/all-models-coefs.rds")




