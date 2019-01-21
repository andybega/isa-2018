

library("tidyverse")
library("states")
library("hrbrthemes")
library("nlme")
library("lme4")
library("broom")
library("ggstance")

source("code/functions.R")


# Setup list of variables of interest
voi <- c(
  c("ccp_torture", "ccp_prerel", "ccp_habcorp", "ccp_dueproc", "ccp_speedtri"),
  c("v2x_elecoff", "v2xel_frefair", "v2asuffrage", "v2x_jucon", 
    "v2xlg_legcon", "v2clacjust", "v2clsocgrp", "v2pepwrses", "v2pepwrsoc"),
  c("epr_excluded_groups_count", "epr_excluded_group_pop")
)

base_spec    <- "~ (1|gwcode) + 1"
control_spec <- "~ (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + itt_RstrctAccess"

# Which models to run?
mt <- c("glmer_pois", "glm_pois")

cy <- readRDS("output/cy.rds")

data(gwstates)
cnames <- gwstates %>% group_by(gwcode) %>% summarize(country = unique(country_name)[1])

# Base models -------------------------------------------------------------


for (mm in mt) {
  fh <- sprintf("output/models/mdl_basic_%s.rds", mm)
  mdl <- fit_model(base_spec, cy, mm)
  write_rds(mdl, path = fh)
}

par(mfrow = c(1, 3))
for (i in 1:3) {
  qqnorm(residuals(mdl[[i]]), main = names(mdl)[i])
  qqline(residuals(mdl[[i]]))
}

plot_predictions(mdl) +
  scale_x_continuous(limits = c(0, 150))

for (mm in mt) {
  fh <- sprintf("output/models/mdl_w_controls_%s.rds", mm)
  mdl <- fit_model(control_spec, cy, mm)
  write_rds(mdl, path = fh)
}

par(mfrow = c(1, 3))
for (i in 1:3) {
  qqnorm(residuals(mdl[[i]]), main = names(mdl)[i])
  qqline(residuals(mdl[[i]]))
}

plot_predictions(mdl) +
  scale_x_continuous(limits = c(0, 150))


# Plot variables of interest ----------------------------------------------

for (xx in voi) {
  fh <- sprintf("output/figures/spaghetti/voi-%s.png", xx)
  p <- ggplot(cy, aes(x = date, group = gwcode)) +
    geom_line(aes_string(y = xx), alpha = .5) +
    theme_minimal()
  ggsave(p, file = fh, width = 8, height = 5)
}


# Estimate basic VOI models -----------------------------------------------


for (xx in voi) {
  for (mm in mt) {
    cat(xx, "-", mm, "\n")
    fh <- sprintf("output/models/mdl_%s_basic_%s.rds", xx, mm)
    mdl <- fit_model(paste0(base_spec, " + ", xx), cy, mm)
    write_rds(mdl, path = fh)
  }
}


# Estimate VOI models with controls ---------------------------------------

# 1; 4; 6; 7; 8!; 9; 10; 16

for (xx in voi) {
  for (mm in mt) {
    cat(xx, "-", mm, "\n")
    fh <- sprintf("output/models/mdl_%s_w_controls_%s.rds", xx, mm)
    mdl <- fit_model(paste0(control_spec, " + ", xx), cy, mm)
    write_rds(mdl, path = fh)
  }
}

