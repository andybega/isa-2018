#
#   depends on output from:
#     - 1-estimate-core-models.R
#     - 2-xgboost.R
#

library("tidyverse")
library("scoringRules")
library("ggrepel")
library("hrbrthemes")

source("R/functions.R")

oos_fit     <- read_csv("output/core-models-fit-out-of-sample.csv")
xgboost_fit <- read_csv("output/xgboost-fit.csv") %>%
  mutate(outcome = str_to_title(outcome))

oos_fit_all <- oos_fit %>%
  mutate(model_name = case_when(
    variable=="none" ~ "Poisson, RE and controls only",
    TRUE ~ "Poisson, RE, controls, and variable")) %>%
  bind_rows(xgboost_fit) %>%
  mutate(model_name = factor(model_name),
         model_name = fct_relevel(model_name, "Poisson, RE, controls, and variable"))

# For each fit statistic and outcome, find the best-performing VOI model
best_voi_model <- oos_fit_all %>%
  filter(model_name!="xgboost", variable!="none") %>%
  gather(fitstat, value, MAE:CRPS) %>%
  group_by(fitstat, outcome) %>%
  filter(value==min(value)) %>%
  mutate(label = rename_terms(variable)) %>%
  select(fitstat, outcome, variable, label)

df <- oos_fit_all %>%
  gather(fitstat, value, MAE:CRPS) %>%
  # join instead of using best_voi_model as separate data because geom_text_repel
  # only works properly when missing labels are ""
  left_join(best_voi_model) %>%
  replace_na(list(label = ""))

df %>%
  # point drawing is by order in data, so make voi models first so that the 
  # control models are drawn over them
  arrange(model_name) %>%
  ggplot(., aes(x = value, y = outcome)) +
  facet_wrap(~ fitstat, scales = "free_x") +
  geom_point(aes(color = model_name,
                 alpha = str_detect(model_name, "(XGBoost)|(controls only)")),
             size = 2) +
  scale_colour_manual("Model:", values = c("#377eb8", "#e41a1c", "#4daf4a")) +
  scale_alpha_manual(guide = FALSE, values = c(.5, 1)) +
  # highlight best non control model
  geom_text_repel(aes(label = label),
                  nudge_y = .3, size = 3.5, family = "Roboto Condensed",
                  color = "gray20") +
  theme_ipsum_rc() +
  labs(x = "Fit statistic value", y = "Outcome variable") +
  theme(legend.position = "top")
ggsave("output/figures/oos-fit-all.png", height = 4.5, width = 10)
