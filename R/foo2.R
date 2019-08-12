#
#   These are some leftovers from the old (pre-August 2019 estimate-all-models.R)
#   version
#

library("tidyverse")
library("states")
library("hrbrthemes")
library("nlme")
library("lme4")
library("broom")
library("ggstance")

source("code/functions.R")

cy <- readRDS("output/cy.rds")

data(gwstates)
cnames <- gwstates %>% 
  group_by(gwcode) %>% 
  summarize(country = unique(country_name)[1])


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

for (xx in voi) {
  fh <- sprintf("output/figures/spaghetti/voi-%s.png", xx)
  p <- ggplot(cy, aes(x = date, group = gwcode)) +
    geom_line(aes_string(y = xx), alpha = .5) +
    theme_minimal()
  ggsave(p, file = fh, width = 8, height = 5)
}