
library("tidyverse")
library("states")

source("input/itt/itt.R")

cy <- readRDS("output/cy.rds")

itt <- cy %>%
  select(gwcode, year, starts_with("LoT")) %>%
  gather(Victim, LoT, starts_with("LoT")) %>%
  mutate(Victim = str_replace(Victim, "LoT", ""),
         LoT = itt_factor(LoT))

col_vals <- c(
  "No Allegations"     = "#ffffcc",
  "Allegation, No LoT" = "#c7e9b4",
  "Infrequent"         = "#7fcdbb",
  "Several"            = "#41b6c4",
  "Routinely" = "#1d91c0",
  "Widespread" = "#225ea8",
  "Systematic" = "#0c2c84",
  "Continued" = "gray80",
  "Increased/Worsening" = "gray80",
  "Improved" = "gray80"
)
itt %>%
  filter(!LoT %in% c(-999, -777)) %>%
  ggplot(., aes(x = year, y = factor(gwcode), fill = LoT)) +
  geom_tile() +
  facet_wrap(~ Victim, nrow = 1) +
  scale_fill_manual(values = col_vals)

itt %>%
  filter(!LoT %in% c(-999, -777)) %>%
  mutate(LoT = fct_other(LoT, drop = c("Continued", "Increased/Worsening", "Improved"))) %>%
  ggplot(., aes(x = LoT, fill = Victim)) +
  geom_bar(position = "dodge") +
  coord_flip()



cor(cy[, c("yy_Unknown", "yy_Dissident", "yy_Criminal", "RstrctAccess", 
           "NY.GDP.PCAP.KD_ln", "NY.GDP.PETR.RT.ZS",
           "LJI", "v2x_polyarchy")], use = "complete.obs")

mdl1 <-  tibble(yvar = yvars) %>%
  mutate(formula = paste(yvar, "~ LJI")) %>%
  mutate(formula = map(formula, as.formula)) %>%
  mutate(model = purrr::map(formula, glm, data = cy, family = binomial(link = "logit"))) %>%
  mutate(AIC = map_dbl(model, AIC), 
         BIC = map_dbl(model, BIC))
mdl1$model %>% map(summary) %>% setNames(yvars)

mdl2 <-  tibble(yvar = yvars) %>%
  mutate(formula = paste(yvar, "~ v2x_polyarchy + NY.GDP.PCAP.KD + NY.GDP.PETR.RT.ZS")) %>%
  mutate(formula = map(formula, as.formula)) %>%
  mutate(model = purrr::map(formula, glm, data = cy, family = binomial(link = "logit"))) %>%
  mutate(AIC = map_dbl(model, AIC), 
         BIC = map_dbl(model, BIC))
mdl2$model %>% map(summary) %>% setNames(yvars)

mdl3 <- tibble(yvar = yvars) %>%
  mutate(formula = paste(yvar, "~ NY.GDP.PCAP.KD")) %>%
  mutate(formula = map(formula, as.formula)) %>%
  mutate(model = purrr::map(formula, glm, data = cy, family = binomial(link = "logit"))) %>%
  mutate(AIC = map_dbl(model, AIC), 
         BIC = map_dbl(model, BIC))
mdl3$model %>% map(summary) %>% setNames(yvars)


ggplot(cy) +
  geom_point(aes(y = LoTCriminal %in% c("Systematic", "Widespread"), x = LJI))

ggplot(cy) +
  geom_line(aes(x = NY.GDP.PCAP.KD, y = v2x_polyarchy, group = gwcode))

ggplot(cy) +
  geom_line(aes(x = LJI, y = v2x_polyarchy, group = gwcode))

            