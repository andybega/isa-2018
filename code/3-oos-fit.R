#
#   depends on output from:
#     - 1-estimate-core-models.R
#     - 2-xgboost.R
#

library("tidyverse")
library("scoringRules")

source("code/functions.R")

oos_fit     <- read_csv("output/model-fit-out-of-sample.csv")
xgboost_fit <- read_csv("output/xgboost-fit.csv") %>%
  mutate(outcome = str_to_title(outcome))

oos_fit_all <- oos_fit %>%
  mutate(model_name = case_when(
    variable=="none" ~ "Poisson, RE and controls only",
    TRUE ~ "Poisson, RE, controls, and variable")) %>%
  bind_rows(xgboost_fit)

# For each fit statistic and outcome, find the best-performing VOI model
best_voi_model <- oos_fit_all %>%
  filter(model_name!="xgboost", variable!="none") %>%
  gather(fitstat, value, MAE:CRPS) %>%
  group_by(fitstat, outcome) %>%
  filter(value==min(value)) %>%
  mutate(variable = rename_terms(variable))

oos_fit_all %>%
  gather(key, value, MAE:CRPS) %>%
  ggplot(., aes(x = value, y = outcome, color = model_name)) +
  facet_wrap(~ key, scales = "free_x") +
  geom_point(aes(alpha = str_detect(model_name, "(XGBoost)|(controls only)"))) +
  theme_minimal() +
  scale_colour_brewer("Model:", type = "qual", palette = 6) +
  scale_alpha_manual(guide = FALSE, values = c(.2, 1)) +
  labs(x = "Fit statistic value", y = "Outcome variable") +
  theme(legend.position = "top")
ggsave("output/figures/oos-fit-all.png", height = 3, width = 8)
