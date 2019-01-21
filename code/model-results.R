
library("tidyverse")
library("states")
library("hrbrthemes")
library("nlme")
library("lme4")
library("broom")
library("ggstance")
library("scoringRules")
library("PRROC")

source("code/functions.R")

cy <- readRDS("output/cy.rds")

data(gwstates)
cnames <- gwstates %>% group_by(gwcode) %>% summarize(country = unique(country_name)[1])

models <- tibble(file = dir("output/models", full.names = TRUE)) %>%
  mutate(model_name = basename(file) %>% str_replace(".rds", "")) %>%
  mutate(model_obj = map(file, readRDS)) %>%
  # Mark what kind of controls are in the model
  mutate(controls = ifelse(str_detect(model_name, "(base2)|(controls)"), "Controls", "Base"),
         model_form = case_when(
           str_detect(model_name, "glm_pois") ~ "glm_pois",
           str_detect(model_name, "glm_nb") ~ "glm_nb",
           str_detect(model_name, "glmer_pois") ~ "glmer_pois",
           str_detect(model_name, "glmer_nb") ~ "glmer_nb"
         ))

# Coefficient plot/table --------------------------------------------------


coefs <- models %>%
  mutate(estimates = map(model_obj, tidy.itt)) %>%
  dplyr::select(-file, -model_obj) %>%
  unnest(estimates) %>% 
  # # add a dummy row so factor level gets plotted
  # bind_rows(., tibble(y = "itt_alleg_vtcriminal", 
  #                     term = "Legal system: Civil\n(reference category)",
  #                     model_name = "mdl_base1")) %>%
  mutate(y = str_replace(y, "itt_alleg_vt", "") %>% str_to_title(),
         model_name = factor(model_name) %>%
           fct_recode("M1: Intercepts only" = "mdl_base1",
                      "M2: Base 2" = "mdl_base2",
                      "M3: M1 + democracy" = "mdl_dem1",
                      "M4: M2 + democracy" = "mdl_dem2",
                      "M1 + LJI" = "mdl_lji1",
                      "M3 + LJI" = "mdl_lji2",
                      "M1 + Legal" = "mdl_legal1")) %>%
  mutate(term = factor(term) %>%
           fct_recode("Global intercept" = "(Intercept)",
                      "Country intercepts, SD" = "sd_(Intercept).gwcode",
                      "ln GDP ($ billions)" = "log(NY.GDP.MKTP.KD)",
                      "ln GDP (normalized)" = "norm_ln_NY.GDP.MKTP.KD",
                      "ln Population (millions)" = "log(pop)",
                      "ln Population (normalized)" = "norm_ln_pop", 
                      "Natural resource rents, %GDP, log(x + 1)" = "log1p(NY.GDP.TOTL.RT.ZS)",
                      "Democracy, 0/1" = "dd_democracy",
                      "Judicial independence" = "LJI",
                      "Internal conflict" = "internal_confl",
                      "INGO restricted access" = "itt_RstrctAccess",
                      "Legal system: Common" = "mrs_legalsysCommon",
                      "Legal system: Islamic" = "mrs_legalsysIslamic",
                      "Legal system: Mixed" = "mrs_legalsysMixed",
                      # CCP
                      "CCP Torture" = "ccp_torture",
                      "CCP Due process" = "ccp_dueproc",
                      # EPR
                      "EPR Excluded groups (count)",
                      "EPR Excluded gruups (% of total pop)",
                      # V-Dem
                      "VDem Suffrage" = "V2asuffrage"),
         # for ggplot
         term = fct_rev(term),
         term = term %>%
           fct_relevel(c("Country intercepts, SD", "Global intercept", 
                         "ln Population (millions)", "ln GDP ($ billions)", 
                         "ln Population (normalized)", "ln GDP (normalized)",
                         "INGO restricted access")) %>%
           fct_relevel(c("Legal system: Mixed", "Legal system: Islamic",
                         "Legal system: Common", "Legal system: Civil\n(reference category)",
                         "Judicial independence"), after = Inf)
         )

h_width = .7
p <- coefs %>%
  filter(term!="Global intercept") %>%
  filter(model_form=="glmer_pois") %>%
  ggplot(., aes(y = estimate, x = term, color = controls, group = model_name)) +
  geom_hline(yintercept = 0, linetype = 1, color = "gray70", size = .4) +
  facet_wrap(~ y) +
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
ggsave(p, file = "output/figures/model-coefs.png", height = 5, width = 10)


p <- coefs %>%
  filter(term!="Global intercept") %>%
  ggplot(., aes(y = estimate, x = term, color = controls, group = model_name)) +
  geom_hline(yintercept = 0, linetype = 1, color = "gray70", size = .4) +
  facet_wrap(~ y) +
  coord_flip() +
  geom_linerange(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                 position = position_dodge(width = h_width)) +
  #geom_linerange(aes(ymin = estimate - 1.28*std.error, ymax = estimate + 1.28*std.error),
  #               position = position_dodge(width = h_width), size = 1) +
  geom_point(position = position_dodge(width = h_width), aes(shape = model_form)) +
  theme_ipsum() +
  labs(x = "", y = "") +
  scale_color_discrete("Specification")
p
ggsave(p, file = "output/figures/model-coefs-all-model-forms.png", height = 5, width = 10)





# In sample fit -----------------------------------------------------------




fit <- models %>%
  mutate(AIC = map(model_obj, function(x) {
    res <- AIC.itt(x)
    res <- tibble(outcome = names(res), AIC = res)
  })) %>%
  mutate(BIC = map(model_obj, function(x) {
    res <- BIC.itt(x)
    res <- tibble(outcome = names(res), BIC = res)
  })) %>%
  mutate(MAE = map(model_obj, function(x) {
    res <- mae.itt(x)
    res <- tibble(outcome = names(res), MAE = res)
  })) %>%
  mutate(RMSE = map(model_obj, function(x) {
    res <- rmse.itt(x)
    res <- tibble(outcome = names(res), RMSE = res)
  })) %>%
  select(-file, -model_obj) %>%
  unnest() %>%
  select(outcome, model_name, AIC, BIC, MAE, RMSE) %>%
  arrange(outcome, model_name)
write_csv(fit, "output/model-fit-in-sample.csv")


# Out of sample fit -------------------------------------------------------

oos_preds <- models %>%
  mutate(preds = map(model_obj, function(x) cv_predict.itt(x, data = cy, folds = 11))) %>%
  dplyr::select(-file, -model_obj) %>%
  unnest()
oos_fit <- oos_preds %>%
  filter(!is.na(yhat)) %>%
  rename(outcome = yname) %>%
  mutate(outcome = str_replace(outcome, "itt_alleg_vt", "")) %>%
  group_by(model_name, outcome) %>%
  summarize(MAE  = mae(y, yhat), 
            RMSE = rmse(y, yhat),
            CRPS = mean(crps_pois(y, yhat)),
            Recall = sum(yhatgt0[ygt0]) / sum(ygt0),
            Precision = sum(yhatgt0[ygt0]) / sum(yhatgt0)) %>%
  arrange(outcome, model_name)
write_csv(oos_fit, "output/model-fit-out-of-sample.csv")

oos_preds <- oos_preds %>%
  mutate(ygt0 = as.integer(y > 0),
         yhatgt0 = as.integer(yhat > 0))

with(oos_preds, foo<<-roc.curve(scores.class0 = yhatgt0[ygt0==1], scores.class1 = yhatgt0[ygt0==0], curve = TRUE))

# Compose fit plot/table --------------------------------------------------

res1 <- read_csv("output/count-model-fit.csv", 
                 col_types = cols(
                   outcome = col_character(),
                   model_name = col_character(),
                   AIC = col_double(),
                   BIC = col_double(),
                   MAE = col_double(),
                   RMSE = col_double()
                 )) 
res1$type <- "in sample"
res2 <- read_csv("output/count-model-oos-fit.csv", 
                 col_types = cols(
                   outcome = col_character(),
                   model_name = col_character(),
                   MAE = col_double(),
                   RMSE = col_double()
                 )) 
res2$type <- "out of sample"
res3 <- read_csv("output/xgboost-fit.csv",
                 col_types = cols(
                   outcome = col_character(),
                   model_name = col_character(),
                   MAE = col_double(),
                   RMSE = col_double()
                 ))
res3$type <- "out of sample"
res <- bind_rows(res1, res2, res3) 
p <- res %>%
  gather(metric, value, AIC:RMSE) %>%
  filter(type=="out of sample" | metric %in% c("AIC", "BIC")) %>%
  mutate(model_name = factor(model_name) %>% fct_rev() %>%
           fct_recode("Intercepts-only (M1)" = "mdl_base1",
                      "Controls only (M2)" = "mdl_base2",
                      "Democracy + intercepts (M1)" = "mdl_dem1",
                      "Democracy + controls (M2)" = "mdl_dem2",
                      "LJI + intercepts (M1)" = "mdl_lji1",
                      "LJI + democracy" = "mdl_lji2",
                      "legal_system" = "mdl_legal1")) %>%
  ggplot(.) +
  geom_point(aes(x = value, y = model_name, colour = outcome)) +
  geom_path(aes(x = value, y = model_name, group = outcome, colour = outcome),
            linetype = 3) +
  facet_wrap(~ metric, scales = "free") +
  theme_ipsum() +
  labs(x = "", y = "")
p
ggsave(p, file = "output/figures/model-fit-plot.png", height = 5, width = 8)

