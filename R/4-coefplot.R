

library("tidyverse")
library("hrbrthemes")
library("nlme")
library("lme4")
library("broom")
library("ggstance")

source("R/functions.R")

models <- read_rds("output/core-models.rds")

coefs <- models %>%
  mutate(estimates = map(model_obj, tidy)) %>%
  dplyr::select(-model_obj) %>%
  unnest(estimates) %>% 
  mutate(term = rename_terms(term))

h_width = .95
alpha = .5
p <- coefs %>%
  ggplot(., aes(y = estimate, x = term, group = interaction(variable, specification))) +
  geom_hline(yintercept = 0, linetype = 1, color = "gray70", size = .4) +
  facet_wrap(~ outcome) +
  coord_flip() +
  geom_linerange(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                 position = position_dodge(width = h_width), alpha = alpha) +
  #geom_linerange(aes(ymin = estimate - 1.28*std.error, ymax = estimate + 1.28*std.error),
  #               position = position_dodge(width = h_width), size = 1) +
  geom_point(position = position_dodge(width = h_width), alpha = alpha) +
  theme_ipsum() +
  labs(x = "", y = "") +
  scale_color_discrete("Specification") +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))
p
ggsave(p, file = "output/figures/model-coefs.png", height = 7, width = 12)
