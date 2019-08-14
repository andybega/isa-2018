

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
  mutate(
    direction = case_when(
      estimate < 0 & p.value < 0.05 ~ "neg",
      estimate > 0 & p.value < 0.05 ~ "pos",
      p.value > 0.05 ~ "insig"
    ),
    direction = factor(direction, levels = c("neg", "insig", "pos"))) %>%
  ggplot(., aes(y = estimate, x = term, color = direction)) +
  geom_hline(yintercept = 0, linetype = 1, color = "gray70", size = .4) +
  facet_wrap(~ outcome) +
  coord_flip() +
  geom_linerange(aes(ymin = estimate - 1.96*std.error, 
                     ymax = estimate + 1.96*std.error,
                     group = interaction(variable, specification)),
                 position = position_dodge(width = h_width), alpha = alpha) +
  geom_point(aes(group = interaction(variable, specification)),
             position = position_dodge(width = h_width), alpha = alpha) +
  theme_ipsum() +
  labs(x = "", y = "") +
  scale_color_manual("Sign and statistical significance:",
                     # colors: wes_palette("Zissou1")[c(1,3,5)]
                     values = c("pos"   = "#3B9AB2", 
                                "insig" = "gray50",
                                "neg"   = "#F21A00"), na.value = "gray50",
                     labels = c("Neg & p < 0.05", "p > 0.05", "Pos & p < 0.05", 
                                "n/a")) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = "bottom")
p
ggsave(p, file = "output/figures/core-models-coefplot.png", height = 7, width = 9.5)
