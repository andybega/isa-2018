
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
  filter(!str_detect(basename(file), "xgboost")) %>%
  mutate(model_name = basename(file) %>% str_replace(".rds", "")) %>%
  mutate(model_obj = map(file, readRDS)) %>%
  # Mark what kind of controls are in the model
  mutate(controls = ifelse(str_detect(model_name, "(base2)|(controls)"), "Controls", "Base"),
         model_form = case_when(
           str_detect(model_name, "glm_pois")   ~ "Poisson (GLM)",
           str_detect(model_name, "glm_nb")     ~ "NegBin (GLM)",
           str_detect(model_name, "glmer_pois") ~ "Poisson w RE (GLMER)",
           str_detect(model_name, "glmer_nb")   ~ "NegBin (GLMER)"
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
  mutate(y = str_replace(y, "itt_alleg_vt", "") %>% str_to_title()) %>%
  mutate(term = rename_terms(term))

p <- coefs %>%
  filter(term!="Global intercept") %>%
  ggplot(., aes(y = estimate, x = term, color = controls, group = model_name)) +
  geom_hline(yintercept = 0, linetype = 1, color = "gray70", size = .4) +
  facet_wrap(~ y) +
  coord_flip() +
  geom_linerange(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                 position = position_dodge(width = h_width), alpha = .6) +
  geom_point(position = position_dodge(width = h_width), aes(shape = model_form),
             alpha = .6) +
  theme_ipsum() +
  labs(x = "", y = "") +
  scale_color_discrete("Specification:") +
  scale_shape_discrete("Model type:") +
  theme(legend.position = "top")
p
ggsave(p, file = "output/figures/model-coefs-all-model-forms.png", height = 8, width = 10)


#   Tables

foo = coefs %>%
  filter(model_form=="glmer_pois",
         y=="Criminal") %>%
  gather(key, value, estimate, std.error) %>%
  mutate(col = paste0(y, "_", model_name, "_", key)) %>%
  select(term, col, value) 

stargazer(mdl[[1]], mdl[[2]], type = "text")

stargazer(mdl[[1]], mdl[[1]], type = "text", 
          coef = list(tidy_model$estimate, tidy_model$estimate),
            se = list(tidy_model$std.error, tidy_model$std.error),
            add.lines = lapply(1:nrow(fit_stats), function(i) unlist(fit_stats[i, ])),
            omit.table.layout = "s"
)


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

