
library("tidyverse")
library("states")
library("hrbrthemes")
library("nlme")
library("lme4")
library("broom")
library("ggstance")

cy <- readRDS("output/cy.rds")

data(gwstates)
cnames <- gwstates %>% group_by(gwcode) %>% summarize(country = unique(country_name)[1])

models <- tibble(file = dir("output/models", full.names = TRUE)) %>%
  mutate(model_name = basename(file) %>% str_replace(".rds", "")) %>%
  mutate(model_obj = map(file, readRDS))

# Helper functions --------------------------------------------------------

get_preds <- function(x) {
  df <- lapply(x, function(mm) {
    tibble(yname = names(mm@frame)[1],
           y = getME(mm, "y"), 
           yhat = predict(mm, type = "response"))
  })
  df <- bind_rows(df)
  df
}

plot_predictions <- function(model) {
  df <- get_preds(model)
  p <- ggplot(df, aes(x = yhat, y = y)) +
    facet_wrap(~ yname) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = 3) +
    theme_ipsum()
  p
}

AIC.itt <- function(x) {
  sapply(x, AIC)
}

BIC.itt <- function(x) {
  sapply(x, BIC)
}

mae <- function(y, x) {
  mean(abs(y - x))
}

mae.itt <- function(x) {
  sapply(x, function(mm) {
    mae(getME(mm, "y"), predict(mm, type = "response"))
  })
}

rmse <- function(y, x) {
  sqrt(mean((y - x)^2))
}

rmse.itt <- function(x) {
  sapply(x, function(mm) {
    rmse(getME(mm, "y"), predict(mm, type = "response"))
  })
}

tidy.itt <- function(x) {
  res <- lapply(x, function(x) {
    data.frame(y = names(x@frame)[1], tidy(x), stringsAsFactors = FALSE)
  })
  bind_rows(res)
}



varDecomp <- function(group, var) {
  # Decompose variance
  # group - vector denoting group membership
  # var   - variable to decompose
  # Returns vector with total, within, and between variance
  
  # Calculate raw deviations
  df <- data.frame(group=group, var=var)
  df$group.mean <- ave(df$var, df$group)
  df$within <- with(df, var - group.mean)
  df$between <- with(df, group.mean - mean(var))
  
  # Calculate variance given df with var, within and between dev.
  v.total   <- mean((var - mean(var))^2)
  v.within  <- mean(df$within^2)
  v.between <- mean(df$between^2)
  res <- c(total=v.total, within=v.within, between=v.between)
  res
}

cv_predict.itt <- function(model, data, folds) {
  # Obtain OOS predictions via CV
  # model: a model or list of models
  # data: the original data (since different models store data in different values, don't try auto pry)
  if (!class(model)[1] %in%  c("list", "itt")) {
    model <- list(model = model)
  }
  data <- data %>%
    ungroup() %>%
    mutate(.row_index = 1:n()) %>%
    # !!! this is hard coded grouping var
    group_by(gwcode) %>%
    mutate(.fold = paste0("fold", base::sample(1:folds, size = n(), replace = TRUE)))
  
  oob_preds <- map_df(model, .id = "model_name", df = data, .f = function(mm, df) {
    oob_preds <- map_dfr(1:folds, mm = mm, df = df, .f = function(ff, mm, df) {
      drop_fold <- paste0("fold", ff)
      yname <- names(mm@frame)[1]
      out_of_bag <- df[df$.fold==drop_fold, ]
      train_data <- df[df$.fold!=drop_fold, ]
      new_model <- update(mm, data = train_data)
      oob_preds <- tibble(yname = yname,
                          y = out_of_bag[[yname]], 
                          yhat = predict(new_model, type = "response", newdata = out_of_bag,
                                         allow.new.levels = TRUE),
                          .row_index = out_of_bag[[".row_index"]],
                          .fold = drop_fold)
      oob_preds
    })
    oob_preds
  })
  oob_preds
}


# Coefficient plot/table --------------------------------------------------


coefs <- models %>%
  mutate(estimates = map(model_obj, function(x) tidy.itt(x))) %>%
  select(-file, -model_obj) %>%
  unnest(estimates) %>%
  # add a dummy row so factor level gets plotted
  bind_rows(., tibble(y = "itt_alleg_vtcriminal", 
                      term = "Legal system: Civil\n(reference category)",
                      model_name = "mdl_base1")) %>%
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
                      "ln GDP per capita" = "NY.GDP.PCAP.KD_ln",
                      "Natural resource rents, %GDP, log(x + 1)" = "log1p(NY.GDP.TOTL.RT.ZS)",
                      "Democracy, 0/1" = "dd_democracy",
                      "Judicial independence" = "LJI",
                      "Internal conflict" = "internal_confl",
                      "INGO restricted access" = "itt_RstrctAccess",
                      "Legal system: Common" = "mrs_legalsysCommon",
                      "Legal system: Islamic" = "mrs_legalsysIslamic",
                      "Legal system: Mixed" = "mrs_legalsysMixed") %>%
           fct_relevel(c("Country intercepts, SD", "Global intercept", 
                         "ln GDP per capita")) %>%
           fct_relevel(c("Legal system: Mixed", "Legal system: Islamic",
                         "Legal system: Common", "Legal system: Civil\n(reference category)",
                         "Judicial independence"), after = Inf))

h_width = .7
p <- coefs %>%
  filter(term!="Global intercept") %>%
  ggplot(., aes(y = estimate, x = term, color = model_name, group = model_name)) +
  geom_hline(yintercept = 0, linetype = 1, color = "gray70", size = .4) +
  facet_wrap(~ y) +
  coord_flip() +
  geom_linerange(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                 position = position_dodge(width = h_width)) +
  geom_linerange(aes(ymin = estimate - 1.28*std.error, ymax = estimate + 1.28*std.error),
                 position = position_dodge(width = h_width), size = 1) +
  geom_point(position = position_dodge(width = h_width)) +
  theme_ipsum() +
  labs(x = "", y = "") +
  scale_color_discrete("Model")
p
ggsave(p, file = "output/count-model-coefs.png", height = 5, width = 10)




# Fit, in and out-of-sample -----------------------------------------------

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
write_csv(fit, "output/count-model-fit.csv")

oos_preds <- models %>%
  mutate(preds = map(model_obj, function(x) cv_predict.itt(x, data = cy, folds = 11))) %>%
  select(-file, -model_obj) %>%
  unnest()
oos_fit <- oos_preds %>%
  filter(!is.na(yhat)) %>%
  rename(outcome = yname) %>%
  mutate(outcome = str_replace(outcome, "itt_alleg_vt", "")) %>%
  group_by(model_name, outcome) %>%
  summarize(MAE = mae(y, yhat), RMSE = rmse(y, yhat)) %>%
  arrange(outcome, model_name)
write_csv(oos_fit, "output/count-model-oos-fit.csv")



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
           fct_recode("Intercepts-only" = "mdl1",
                      "GDP + pop (M2)" = "mdl2",
                      "M2 + LJI (M3)" = "mdl3",
                      "M2 + legal_system (M4)" = "mdl4")) %>%
  ggplot(.) +
  geom_point(aes(x = value, y = model_name, colour = outcome)) +
  geom_path(aes(x = value, y = model_name, group = outcome, colour = outcome),
            linetype = 3) +
  facet_wrap(~ metric, scales = "free") +
  theme_ipsum() +
  labs(x = "", y = "")
ggsave(p, file = "output/model-fit-plot.png", height = 5, width = 8)

