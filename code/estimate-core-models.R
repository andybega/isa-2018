library("tidyverse")
library("states")
library("hrbrthemes")
library("nlme")
library("lme4")
library("broom")
library("ggstance")
library("stargazer")
library("scoringRules")

source("code/functions.R")

# this is not used, for reference only
base_spec    <- "~ (1|gwcode) + 1"
control_spec <- "~ (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess"

cy <- readRDS("output/cy.rds")

data(gwstates)
cnames <- gwstates %>% group_by(gwcode) %>% summarize(country = unique(country_name)[1])


none <- list(
  # base = list(
  #   Criminal     = glmer(itt_alleg_vtcriminal     ~ (1|gwcode) + 1, 
  #                        data = cy, family = poisson(link = "log")),
  #   Dissident    = glmer(itt_alleg_vtdissident    ~ (1|gwcode) + 1, 
  #                        data = cy, family = poisson(link = "log")),
  #   Marginalized = glmer(itt_alleg_vtmarginalized ~ (1|gwcode) + 1, 
  #                        data = cy, family = poisson(link = "log"))
  # ),
  controls = list(
    Criminal     = glmer(itt_alleg_vtcriminal     ~ (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Dissident    = glmer(itt_alleg_vtdissident    ~ (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Marginalized = glmer(itt_alleg_vtmarginalized ~ (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log"))
  )
)

# ccp_torture
ccp_torture <- list(
  controls = list(
    Criminal     = glmer(itt_alleg_vtcriminal     ~ ccp_torture + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Dissident    = glmer(itt_alleg_vtdissident    ~ ccp_torture + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Marginalized = glmer(itt_alleg_vtmarginalized ~ ccp_torture + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log"))
  )
)

# ccp_prerel
ccp_prerel <- list(
  controls = list(
    Criminal     = glmer(itt_alleg_vtcriminal     ~ ccp_prerel + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Dissident    = glmer(itt_alleg_vtdissident    ~ ccp_prerel + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Marginalized = glmer(itt_alleg_vtmarginalized ~ ccp_prerel + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log"))
  )
)

# ccp_habcorp
ccp_habcorp <- list(
  controls = list(
    Criminal     = glmer(itt_alleg_vtcriminal     ~ ccp_habcorp + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Dissident    = glmer(itt_alleg_vtdissident    ~ ccp_habcorp + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Marginalized = glmer(itt_alleg_vtmarginalized ~ ccp_habcorp + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log"))
  )
)

# ccp_dueproc
ccp_dueproc <- list(
  controls = list(
    Criminal     = glmer(itt_alleg_vtcriminal     ~ ccp_dueproc + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Dissident    = glmer(itt_alleg_vtdissident    ~ ccp_dueproc + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Marginalized = glmer(itt_alleg_vtmarginalized ~ ccp_dueproc + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log"))
  )
)

# ccp_speedtri
ccp_speedtri <- list(
  controls = list(
    Criminal     = glmer(itt_alleg_vtcriminal     ~ ccp_speedtri + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Dissident    = glmer(itt_alleg_vtdissident    ~ ccp_speedtri + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Marginalized = glmer(itt_alleg_vtmarginalized ~ ccp_speedtri + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log"))
  )
)

# v2x_jucon
v2x_jucon  <- list(
  controls = list(
    Criminal     = glmer(itt_alleg_vtcriminal     ~ v2x_jucon  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Dissident    = glmer(itt_alleg_vtdissident    ~ v2x_jucon  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Marginalized = glmer(itt_alleg_vtmarginalized ~ v2x_jucon  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log"))
  )
)

# v2xlg_legcon
v2xlg_legcon  <- list(
  controls = list(
    Criminal     = glmer(itt_alleg_vtcriminal     ~ v2xlg_legcon  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Dissident    = glmer(itt_alleg_vtdissident    ~ v2xlg_legcon  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Marginalized = glmer(itt_alleg_vtmarginalized ~ v2xlg_legcon  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log"))
  )
)

# v2clacjust
v2clacjust  <- list(
  controls = list(
    Criminal     = glmer(itt_alleg_vtcriminal     ~ v2clacjust  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Dissident    = glmer(itt_alleg_vtdissident    ~ v2clacjust  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Marginalized = glmer(itt_alleg_vtmarginalized ~ v2clacjust  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log"))
  )
)

# v2clsocgrp
v2clsocgrp  <- list(
  controls = list(
    Criminal     = glmer(itt_alleg_vtcriminal     ~ v2clsocgrp  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Dissident    = glmer(itt_alleg_vtdissident    ~ v2clsocgrp  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Marginalized = glmer(itt_alleg_vtmarginalized ~ v2clsocgrp  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log"))
  )
)

# v2pepwrses
v2pepwrses  <- list(
  controls = list(
    Criminal     = glmer(itt_alleg_vtcriminal     ~ v2pepwrses  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Dissident    = glmer(itt_alleg_vtdissident    ~ v2pepwrses  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Marginalized = glmer(itt_alleg_vtmarginalized ~ v2pepwrses  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log"))
  )
)

# v2pepwrsoc
v2pepwrsoc  <- list(
  controls = list(
    Criminal     = glmer(itt_alleg_vtcriminal     ~ v2pepwrsoc  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Dissident    = glmer(itt_alleg_vtdissident    ~ v2pepwrsoc  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Marginalized = glmer(itt_alleg_vtmarginalized ~ v2pepwrsoc  + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log"))
  )
)

# epr_excluded_group_pop
epr_excluded_group_pop  <- list(
  controls = list(
    Criminal     = glmer(itt_alleg_vtcriminal     ~ norm_sqrt_epr_excluded_group_pop + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Dissident    = glmer(itt_alleg_vtdissident    ~ norm_sqrt_epr_excluded_group_pop + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Marginalized = glmer(itt_alleg_vtmarginalized ~ norm_sqrt_epr_excluded_group_pop + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log"))
  )
)

# epr_excluded_groups_count
epr_excluded_groups_count  <- list(
  controls = list(
    Criminal     = glmer(itt_alleg_vtcriminal     ~ norm_ln1p_epr_excluded_groups_count + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Dissident    = glmer(itt_alleg_vtdissident    ~ norm_ln1p_epr_excluded_groups_count + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Marginalized = glmer(itt_alleg_vtmarginalized ~ norm_ln1p_epr_excluded_groups_count + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log"))
  )
)

# dd_democracy
dd_democracy  <- list(
  controls = list(
    Criminal     = glmer(itt_alleg_vtcriminal     ~ dd_democracy + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Dissident    = glmer(itt_alleg_vtdissident    ~ dd_democracy + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log")),
    Marginalized = glmer(itt_alleg_vtmarginalized ~ dd_democracy + (1|gwcode) + 1 + norm_ln_NY.GDP.MKTP.KD + norm_ln_pop + internal_confl + itt_RstrctAccess,
                         data = cy, family = poisson(link = "log"))
  )
)

# Pull all models together
models <- list(none         = none, 
               ccp_torture  = ccp_torture,
               ccp_prerel   = ccp_prerel,
               ccp_habcorp  = ccp_habcorp,
               ccp_dueproc  = ccp_dueproc,
               ccp_speedtri = ccp_speedtri,
               v2x_jucon = v2x_jucon,
               v2xlg_legcon = v2xlg_legcon,
               v2clacjust = v2clacjust,
               v2clsocgrp = v2clsocgrp,
               v2pepwrses = v2pepwrses,
               v2pepwrsoc = v2pepwrsoc,
               epr_excluded_group_pop = epr_excluded_group_pop,
               epr_excluded_groups_count = epr_excluded_groups_count,
               dd_democracy = dd_democracy)

models <- models %>%
  enframe(name = "variable") %>%
  mutate(value = map(value, enframe, name = "specification")) %>%
  unnest(value) %>%
  mutate(value = map(value, enframe, name = "outcome", value = "model_obj")) %>%
  unnest(value)

# Add OOS predictions
models <- models %>%
  mutate(oos_preds = cv_predict(models$model_obj, data = cy, folds = 11))

write_rds(models, path = "output/core-models.rds")



# Check models with convergence warnings ----------------------------------


# to be done, right now no issues


# Coefficient plot --------------------------------------------------------

models <- read_rds("output/core-models.rds")

coefs <- models %>%
  mutate(estimates = map(model_obj, tidy)) %>%
  dplyr::select(-model_obj) %>%
  unnest(estimates) %>% 
  mutate(term = rename_terms(term))

h_width = .9
p <- coefs %>%
  ggplot(., aes(y = estimate, x = term, group = interaction(variable, specification))) +
  geom_hline(yintercept = 0, linetype = 1, color = "gray70", size = .4) +
  facet_wrap(~ outcome) +
  coord_flip() +
  geom_linerange(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                 position = position_dodge(width = h_width)) +
  #geom_linerange(aes(ymin = estimate - 1.28*std.error, ymax = estimate + 1.28*std.error),
  #               position = position_dodge(width = h_width), size = 1) +
  geom_point(position = position_dodge(width = h_width)) +
  theme_ipsum() +
  labs(x = "", y = "") +
  scale_color_discrete("Specification")
p
ggsave(p, file = "output/figures/model-coefs.png", height = 6, width = 10.5)


# Regression table --------------------------------------------------------


for (yy in unique(models$outcome)) {
  for (g in 1:2) {
    fh <- sprintf("output/tables/coefficients-%s-group%s.tex", tolower(yy), g)
    tbl_str <- filter(models, outcome==yy) %>%
      mutate(group = ifelse(row_number() < 8, 1, 2)) %>%
      filter(group==g) %>%
      pull(model_obj) %>%
      stargazer(float.env = "sidewaystable",
                no.space = TRUE,
                font.size = "tiny") %>%
      fix_table_varnames()
    
    write_lines(tbl_str, path = fh)
  }
}


# Out-of-sample predictions -----------------------------------------------


oos_fit <- models %>%
  select(-model_obj) %>%
  unnest(oos_preds) %>%
  mutate(ygt0 = as.integer(y > 0),
         yhatgt0 = as.integer(yhat > 0)) %>%
  group_by(variable, specification, outcome) %>%
  summarize(MAE  = mae(y, yhat), 
            RMSE = rmse(y, yhat),
            CRPS = mean(crps_pois(y, yhat)) ) %>%
            # the models never predict 0, so no point checking this
            #Recall = sum(yhatgt0[ygt0]) / sum(ygt0),
            #Precision = sum(yhatgt0[ygt0]) / sum(yhatgt0)) %>%
  arrange(outcome, CRPS, variable, specification)
write_csv(oos_fit, "output/model-fit-out-of-sample.csv")


oos_fit <- read_csv("output/model-fit-out-of-sample.csv")

xgboost_fit <- read_csv("output/xgboost-fit.csv") %>%
  mutate(outcome = str_to_title(outcome))

oos_fit_all <- oos_fit %>%
  mutate(model_name = case_when(
    variable=="none" ~ "Poisson, RE and controls only",
    TRUE ~ "Poisson, RE, controls, and variable")) %>%
  bind_rows(xgboost_fit)

oos_fit_all %>%
  gather(key, value, MAE:CRPS) %>%
  ggplot(., aes(x = value, y = outcome, color = model_name)) +
  facet_wrap(~ key, scales = "free_x") +
  geom_jitter(height = .2, alpha = .6) +
  theme_minimal() +
  scale_colour_brewer("Model:", type = "qual", palette = 6) +
  labs(x = "Fit statistic value", y = "Outcome variable") +
  theme(legend.position = "top")
ggsave("output/figures/oos-fit-all.png", height = 3, width = 8)
