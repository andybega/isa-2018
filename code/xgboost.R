
# Rscript code/xgboost.R

suppressMessages({
  library("tidyverse")
  library("states")
  library("hrbrthemes")
  library("broom")
  library("ggstance")
  library("xgboost")
  library("caret")
  library("recipes")
  library("pdp")
  library("lme4")
  library("doMC") 
  library("scoringRules")
  library("futile.logger")
})

flog.info("xgboost.R script start")

set.seed(1234)

registerDoMC(cores = 4) 

source("code/functions.R")

cy <- readRDS("output/cy.rds")

data(gwstates)
cnames <- gwstates %>% group_by(gwcode) %>% summarize(country = unique(country_name)[1])


# Xgboost general count model ---------------------------------------------


num_data <- cy %>%
  select(gwcode, year, one_of(voi), starts_with("norm_"), starts_with("itt_alleg"), 
         "itt_RstrctAccess") %>%
  mutate(gwcode_fct = factor(gwcode))
# Don't want to center/scale binary/quasi-categorical vars, so ID those
binary_vars <- num_data %>%
  summarize_all(~ mean(. %in% c(0, 1, 100)) > .9) %>%
  tidyr::gather(var, quasibinary) %>%
  filter(quasibinary) %>%
  pull(var)
num_data <- recipe(num_data) %>%
  step_dummy(gwcode_fct) %>%
  update_role(everything(), new_role = "predictor") %>%
  update_role(., starts_with("itt_alleg"), new_role = "outcome") %>%
  update_role(., gwcode, year, new_role = "ID") %>%
  step_zv(all_predictors()) %>%
  prep(retain = TRUE)
train_id_vars <- num_data %>%
  juice(has_role("ID"))
train_y <- num_data %>%
  juice(has_role("outcome"))
train_x <- num_data %>%
  juice(has_role("predictor")) %>%
  as.matrix()

trControl <- caret::trainControl(method = "cv", number = 11, 
                                 verboseIter = FALSE, savePredictions = TRUE)

# http://xgboost.readthedocs.io/en/latest/how_to/param_tuning.html
# http://xgboost.readthedocs.io/en/latest/parameter.html
generate_random_grid <- function(n) {
  data.frame(nrounds = sample(c(100, 150, 200, 500, 1000), n, replace = TRUE),
             eta = round(runif(n, 0, .5), 2),
             # tree complexity
             max_depth = sample(2:10, n, replace = TRUE),
             min_child_weight = rpois(n, 1),
             gamma = round(rexp(n, rate = 5), 1),
             # randomization
             colsample_bytree = round(rbeta(n, 3, 1), 2),
             subsample = round(rbeta(n, 5, 2), 2)
             )
}
fixed_grid <- data.frame(
  nrounds = 100,
  eta = .3,
  max_depth = 5,
  min_child_weight = 1,
  gamma = 0,
  colsample_by_tree = .6,
  subsample = .75
)
hp_grid <- generate_random_grid(200)

mdlX <- list(
  criminal = caret::train(x = train_x, y = train_y[["itt_alleg_vtcriminal"]],
                   method = "xgbTree", objective = "count:poisson", 
                   eval_metric = "poisson-nloglik", trControl = trControl,
                   tuneGrid = hp_grid, metric = "MAE"),
  dissident = caret::train(x = train_x, y = train_y[["itt_alleg_vtdissident"]],
                    method = "xgbTree", objective = "count:poisson", 
                    eval_metric = "poisson-nloglik", trControl = trControl,
                    tuneGrid = hp_grid, metric = "MAE"),
  marginalized = caret::train(x = train_x, y = train_y[["itt_alleg_vtmarginalized"]],
                       method = "xgbTree", objective = "count:poisson", 
                       eval_metric = "poisson-nloglik", trControl = trControl,
                       tuneGrid = hp_grid, metric = "MAE")
)

write_rds(mdlX, path = "output/models/mdl_xgboost.rds")

# Hyperparameter tuning
# Extract all resample predictions
resample_preds <- mdlX %>% 
  enframe("outcome") %>%
  mutate(pred = map(value, "pred")) %>%
  select(-value) %>%
  unnest(pred) %>%
  mutate(pred = as.numeric(pred))
# ID the hyperparameter group and compute fit metrics
hp_performance <- resample_preds %>%
  mutate(hp_group = group_indices(., eta, max_depth, gamma, colsample_bytree, 
                                  min_child_weight, subsample, nrounds)) %>%
  group_by(outcome, hp_group) %>%
  mutate(MAE = mae(obs, pred),
         RMSE = rmse(obs, pred),
         CRPS = mean(crps_pois(obs, pred)) ) %>%
  dplyr::summarize_at(vars(eta:nrounds, MAE:CRPS), unique)
p <- hp_performance %>%
  gather(hp, value, eta:nrounds) %>%
  ggplot(., aes(x = value, y = MAE)) +
  facet_grid(outcome ~ hp, scales = "free_x") +
  geom_point() +
  geom_smooth() +
  theme_minimal() 
p
ggsave(p, file = "output/figures/xgboost-hp-tuning.png", height = 8, width = 12)



oos_preds <- mdlX %>%
  map_dfr(., .id = "yname", function(mm) {
    out <- mm$pred %>%
      filter(nrounds   == mm$bestTune$nrounds,
             max_depth == mm$bestTune$max_depth,
             eta       == mm$bestTune$eta,
             gamma     == mm$bestTune$gamma,
             colsample_bytree == mm$bestTune$colsample_bytree,
             min_child_weight == mm$bestTune$min_child_weight,
             subsample == mm$bestTune$subsample)
    out <- out %>%
      rename(y = obs, yhat = pred, row_index = rowIndex) %>%
      select(y, yhat, row_index) %>%
      mutate(yhat = as.numeric(yhat)) %>%
      as_tibble()
    out <- out %>% arrange(row_index) %>% select(-row_index)
    out
  })

p <- oos_preds %>%
  mutate(yname = str_to_title(yname)) %>%
  ggplot(., aes(x = yhat, y = y)) +
  facet_wrap(~ yname) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = 3) +
  theme_ipsum() +
  ggtitle("XGBoost model yhat vs y") 
p
ggsave(p, file = "output/figures/xgboost-y-vs-yhat.png", height = 5, width = 12)


fit_xgboost <- oos_preds %>%
  rename(outcome = yname) %>%
  group_by(outcome) %>%
  summarize(MAE = mae(y, yhat),
            RMSE = rmse(y, yhat),
            CRPS = mean(crps_pois(y, yhat)) ) %>%
  mutate(model_name = "XGBoost") %>%
  select(outcome, model_name, MAE, RMSE, CRPS)
write_csv(fit_xgboost, "output/xgboost-fit.csv")



predictor_importance <- mdlX %>%
  map_dfr(., .id = "outcome", function(mm) {
    imp <- varImp(mm)$importance
    imp <- tibble(predictor = rownames(imp), importance = imp[, 1])
    imp
  })

p <- predictor_importance %>%
  dplyr::filter(!str_detect(predictor, "gwcode_fct")) %>%
  mutate(predictor = rename_terms(predictor)) %>%
  group_by(predictor) %>%
  mutate(avg_imp = mean(importance)) %>%
  arrange(avg_imp) %>%
  ungroup() %>%
  mutate(predictor = factor(predictor) %>% fct_inorder()) %>%
  ggplot(., aes(x = importance, y = predictor)) +
  geom_linerangeh(aes(xmin = 0, xmax = importance, y = predictor, group = outcome), 
                  linetype = 1, color = "gray90", position = position_dodgev(height = .5)) +
  geom_point(aes(colour = outcome), position = position_dodgev(height = .5)) + 
  theme_ipsum() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = c(.8, .2)) +
  labs(x = "XGBoost variable importance, 0-100", y = "") +
  scale_colour_discrete("Outcome")
p
ggsave(p, file = "output/figures/xgboost-variable-importance-v1.png", height = 6, width = 10)

p <- predictor_importance %>%
  mutate(predictor = rename_terms(predictor)) %>%
  group_by(predictor) %>%
  mutate(avg_imp = mean(importance)) %>%
  arrange(avg_imp) %>%
  ungroup() %>%
  dplyr::slice((501-3*30+1):501) %>%
  mutate(predictor = factor(predictor) %>% fct_inorder()) %>%
  ggplot(., aes(x = importance, y = predictor)) +
  geom_linerangeh(aes(xmin = 0, xmax = importance, y = predictor, group = outcome), 
                  linetype = 1, color = "gray90", position = position_dodgev(height = .5)) +
  geom_point(aes(colour = outcome), position = position_dodgev(height = .5)) + 
  theme_ipsum() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = c(.8, .2)) +
  labs(x = "XGBoost variable importance, 0-100", y = "") +
  scale_colour_discrete("Outcome")
p
ggsave(p, file = "output/figures/xgboost-variable-importance-v1-with-gwcode.png", height = 6, width = 10)


p <- predictor_importance %>%
  dplyr::filter(!str_detect(predictor, "gwcode_fct")) %>%
  mutate(predictor = rename_terms(predictor),
         outcome = str_to_title(outcome)) %>%
  group_by(predictor) %>%
  mutate(avg_imp = mean(importance)) %>%
  arrange(avg_imp) %>%
  ungroup() %>%
  mutate(predictor = factor(predictor) %>% fct_inorder()) %>%
  ggplot(., aes(x = importance, y = predictor)) +
  geom_linerangeh(aes(xmin = 0, xmax = importance, y = predictor, group = outcome), 
                  linetype = 1, color = "gray90") +
  geom_point(aes(colour = outcome)) + 
  facet_wrap(~ outcome) +
  theme_ipsum() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "top") +
  labs(x = "XGBoost variable importance, 0-100", y = "") +
  scale_colour_discrete("Outcome", guide = FALSE)
p
ggsave(p, file = "output/figures/xgboost-variable-importance-v2.png", height = 6, width = 10)


# ice <- FALSE
# pd <- mdlX %>%
#   map_df(., .id = "outcome", partial, pred.var = "LJI", ice = ice)
# pd %>%
#   ggplot(., aes(x = LJI, y = yhat)) +
#   facet_wrap(~ outcome) + 
#   geom_line()
# 
# pd <- mdlX %>%
#   map_df(., .id = "outcome", partial, pred.var = "NY.GDP.PCAP.KD", ice = ice)
# pd %>%
#   ggplot(., aes(x = NY.GDP.PCAP.KD, y = yhat)) +
#   facet_wrap(~ outcome) + 
#   geom_line()




dontrun <- function() {
  

# Use xgboost on count model residuals ------------------------------------


mdl_base1 <- list(
  criminal     = glmer(
    itt_alleg_vtcriminal ~ (1|gwcode) + 1, 
    data = cy, family = poisson(link = "log")),
  dissident    = glmer(
    itt_alleg_vtdissident ~ (1|gwcode) + 1, 
    data = cy, family = poisson(link = "log")),
  marginalized = glmer(
    itt_alleg_vtmarginalized ~ (1|gwcode) + 1, 
    data = cy, family = poisson(link = "log"))
)
attr(mdl_base1, "class") <- "itt"

in_sample_errors <- lapply(names(mdl_base1), function(nn) {
  model = mdl_base1[[nn]]
  yy_name = paste0("itt_alleg_vt", nn)
  yhat = predict(model, newdata = cy, allow.new.levels = TRUE, type = "response")
  cy[[yy_name]] - yhat
}) %>%
  setNames(paste0("resid_", names(mdl_base1))) %>%
  bind_cols()

cy_w_resid <- cy %>%
  bind_cols(., in_sample_errors)

# Almost all error is within country; makes sense
for_decomp = cy_w_resid %>%
  filter(!is.na(resid_criminal) & !is.na(resid_dissident) & !is.na(resid_marginalized))
varDecomp(for_decomp$gwcode, for_decomp$resid_criminal)
varDecomp(for_decomp$gwcode, for_decomp$resid_dissident)
varDecomp(for_decomp$gwcode, for_decomp$resid_marginal)

num_data <- cy_w_resid %>%
  select(-starts_with("yy_"), -starts_with("itt_LoT"), -starts_with("itt_alleg"),
         # doesn't play with step_dummy
         -hensel_colonial) %>% 
  mutate(gwcodeUS = as.integer(gwcode==2)) %>%
  filter(!is.na(resid_criminal) & !is.na(resid_dissident) & !is.na(resid_marginalized))
# Don't want to center/scale binary/quasi-categorical vars, so ID those
binary_vars <- num_data %>%
  summarize_all(funs(length(unique(.)))) %>%
  tidyr::gather(var, unique_vals) %>%
  filter(unique_vals < 10) %>%
  pull(var)
num_data <- recipe(num_data) %>%
  add_role(starts_with("resid"), new_role = "outcome") %>%
  add_role(gwcode, year, date, new_role = "ID") %>%
  add_role(-starts_with("resid"), -gwcode, -year, -date, new_role = "predictor") %>%
  step_center(all_numeric(), -all_outcomes(), -has_role("ID"), -one_of(binary_vars)) %>%
  step_scale(all_numeric(), -all_outcomes(), -has_role("ID"), -one_of(binary_vars)) %>%
  step_dummy(regime, ht_colonial, mrs_legalsys) %>%
  step_zv(all_predictors()) %>%
  prep(retain = TRUE)
train_id_vars <- num_data %>%
  juice(has_role("ID"))
train_y <- num_data %>%
  juice(has_role("outcome"))
train_x <- num_data %>%
  juice(has_role("predictor")) %>%
  as.matrix()

trControl <- caret::trainControl(method = "cv", number = 11, 
                                 verboseIter = FALSE, savePredictions = "final")

# http://xgboost.readthedocs.io/en/latest/how_to/param_tuning.html
# http://xgboost.readthedocs.io/en/latest/parameter.html
hyperparameters <- data.frame(nrounds = 100, 
                              eta = .3, 
                              # tree complexity
                              max_depth = 5, 
                              min_child_weight = 1, 
                              gamma = 0, 
                              # randomization
                              colsample_bytree = .6, 
                              subsample = .75)

mdl_xgboost_resid <- list(
  criminal = train(x = train_x, y = train_y[["resid_criminal"]],
                   method = "xgbTree", objective = "reg:linear", 
                   eval_metric = "rmse", trControl = trControl,
                   tuneGrid = hyperparameters),
  dissident = train(x = train_x, y = train_y[["resid_dissident"]],
                    method = "xgbTree", objective = "reg:linear", 
                    eval_metric = "rmse", trControl = trControl,
                    tuneGrid = hyperparameters),
  marginalized = train(x = train_x, y = train_y[["resid_marginalized"]],
                       method = "xgbTree", objective = "reg:linear", 
                       eval_metric = "rmse", trControl = trControl,
                       tuneGrid = hyperparameters)
)

oos_preds <- mdl_xgboost_resid %>%
  map_dfr(., .id = "yname", function(mm) {
    out <- tibble(y = mm$pred$obs,
                  yhat = mm$pred$pred,
                  row_index = mm$pred$rowIndex)
    out <- out %>% arrange(row_index) %>% select(-row_index)
    out
  })

p <- ggplot(oos_preds, aes(x = yhat, y = y)) +
  facet_wrap(~ yname) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = 3) +
  theme_ipsum() +
  ggtitle("XGBoost model yhat vs y") 
#ggsave(p, file = "output/mdlX-y-vs-yhat.png", height = 5, width = 12)
p

# R^2 are pretty low
mdl_xgboost_resid[[1]]
mdl_xgboost_resid[[2]]
mdl_xgboost_resid[[3]]

predictor_importance <- mdl_xgboost_resid %>%
  map_dfr(., .id = "outcome", function(mm) {
    imp <- varImp(mm)$importance
    imp <- tibble(predictor = rownames(imp), importance = imp[, 1])
    imp
  })

p <- predictor_importance %>%
  group_by(predictor) %>%
  mutate(avg_imp = mean(importance)) %>%
  arrange(avg_imp) %>%
  ungroup() %>%
  mutate(predictor = factor(predictor) %>% fct_inorder()) %>%
  ggplot(., aes(x = importance, y = predictor)) +
  geom_linerangeh(aes(xmin = 0, xmax = importance, y = predictor, group = outcome), 
                  linetype = 1, color = "gray90", position = position_dodgev(height = .5)) +
  geom_point(aes(colour = outcome), position = position_dodgev(height = .5)) + 
  theme_ipsum() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = c(.8, .1)) +
  labs(x = "XGBoost variable importance, 0-100", y = "") +
  scale_colour_discrete("Outcome")
ggsave(p, file = "output/figures/xgboost-residuals-variable-importance.png", height = 8, width = 6)
p

}

flog.info("xgboost.R script end")