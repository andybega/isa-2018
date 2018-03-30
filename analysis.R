
library("tidyverse")
library("states")
library("hrbrthemes")
library("nlme")
library("lme4")
library("broom")
library("ggstance")
library("xgboost")
library("caret")
library("recipes")
library("pdp")

source("input/itt/itt.R")

cy <- readRDS("output/cy.rds")

data(gwstates)
cnames <- gwstates %>% group_by(gwcode) %>% summarize(country = unique(country_name)[1])

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

#' Tidy up a correlation matrix
#' 
#' X is a matrix
tidy_cormat <- function(X) {
  cor(X) %>%
    as_tibble() %>% 
    mutate(var1 = names(.)) %>% 
    gather(var2, cor, -var1) 
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



# Plotting outcomes -------------------------------------------------------

# Allegations by victim type
p <- cy %>%
  select(gwcode, year, starts_with("itt_alleg"), -itt_alleg_vtall) %>%
  gather(Victim, Allegations, starts_with("itt_alleg")) %>%
  # Clean up victim factor labels
  mutate(Victim = str_replace(Victim, "itt_alleg_vt", "") %>% str_to_title(),
         Victim = factor(Victim) %>% fct_recode(Unknown = "Unst", POW = "Pow")) %>%
  group_by(Victim) %>%
  summarize(Allegations = sum(Allegations)) %>%
  # Sort by # allegations so we can re-order factor lables, so the plot is high to low
  arrange(Allegations) %>%
  mutate(Victim = fct_inorder(Victim)) %>%
  ungroup() %>%
  ggplot(.) +
  ggstance::geom_barh(aes(y = Victim, x = Allegations), stat = "identity",
                      width = .8) +
  geom_text(aes(y = Victim, x = Allegations, 
                label = formatC(Allegations, format="d", big.mark=",")),
            nudge_x = c(2*130, 3*130, rep(-100, 4)), hjust = 1, 
            colour = c(rep("gray10", 2), rep("gray95", 4))) +
  theme_ipsum() +
  labs(y = "", x = "# Allegations") +
  theme(axis.text=element_text(size=11, colour = "gray10", family = "Arial"))
ggsave(p, file = "output/allegations-by-victim.png", height = 5, width = 8)

# Which countries have the most allegations?
by_victim_and_country <- cy %>%
  select(gwcode, year, starts_with("itt_alleg"), -itt_alleg_vtall) %>%
  gather(Victim, Allegations, starts_with("itt_alleg")) %>%
  # Clean up victim factor labels
  mutate(Victim = str_replace(Victim, "itt_alleg_vt", "") %>% str_to_title(),
         Victim = factor(Victim) %>% fct_recode(Unknown = "Unst", POW = "Pow")) %>%
  group_by(gwcode, Victim) %>%
  summarize(Allegations = sum(Allegations)) %>%
  left_join(., cnames, by = "gwcode") %>%
  ungroup()
by_victim_and_country %>%
  arrange(desc(Allegations))

by_country <- by_victim_and_country %>%
  group_by(gwcode) %>%
  summarize(Allegations = sum(Allegations)) 
by_country %>%
  arrange(desc(Allegations))


itt <- cy %>%
  select(gwcode, year, starts_with("itt_LoT")) %>%
  gather(Victim, LoT, starts_with("itt_LoT")) %>%
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

data(gwstates)
cnames <- gwstates %>% group_by(gwcode) %>% summarize(country = unique(country_name)[1])
p <- cy %>%
  filter(gwcode %in% c(2, 200, 220, 260, 710, 750, 640, 775)) %>%
  select(gwcode, date, itt_alleg_vtcriminal:itt_alleg_vtunst) %>%
  gather(Victim, Allegations, -gwcode, -date) %>%
  # Clean up victim factor labels
  mutate(Victim = str_replace(Victim, "itt_alleg_vt", "") %>% str_to_title(),
         Victim = factor(Victim) %>% fct_recode(Unknown = "Unst", POW = "Pow")) %>%
  left_join(., cnames, by = "gwcode") %>%
  ggplot(., aes(x = date, y = Allegations, colour = Victim)) +
  facet_wrap(~ country, nrow = 2) +
  geom_line() +
  scale_colour_brewer(type = "qual", palette = 3) +
  theme_ipsum() +
  ggtitle("ITT allegations by victim type for select countries")
ggsave(p, file = "output/selected-allegation-counts.png", height = 5, width = 10, scale = 1.2)

p <- itt %>%
  filter(gwcode %in% c(2, 200, 220, 260, 710, 750, 640, 775)) %>%
  left_join(., cnames, by = "gwcode") %>%
  ggplot(., aes(x = year, y = Victim, fill = LoT)) +
  geom_tile() +
  facet_wrap(~ country, nrow = 2) +
  scale_fill_manual(values = col_vals) +
  theme_ipsum() +
  ggtitle("ITT level of torture by victim type for select countries")
ggsave(p, file = "output/selected-levels-of-torture.png", height = 5, width = 10, scale = 1.2)


# How much variance is within countries/victim-types?
df <- cy %>%
  select(gwcode, starts_with("itt_alleg_vt"), -itt_alleg_vtall) %>%
  gather(victim, allegations, starts_with("itt_alleg"))
varDecomp(paste0(df$gwcode, df$victim), df$allegations)
df2 <- filter(df, victim %in% c("itt_alleg_vtcriminal", "itt_alleg_vtdissident", "itt_alleg_vtmarginalized"))
varDecomp(paste0(df2$gwcode, df2$victim), df2$allegations)

cors_by_country <- cy %>%
  select(gwcode, itt_alleg_vtcriminal:itt_alleg_vtunst) %>%
  group_by(gwcode) %>%
  nest() %>%
  mutate(cormat = map(data, tidy_cormat)) %>%
  mutate(data = NULL) %>%
  unnest()

keep <- c("itt_alleg_vtcriminal", "itt_alleg_vtdissident", "itt_alleg_vtmarginalized", "itt_alleg_vtunst")
cors_by_country %>%
  filter(var1!=var2) %>%
  filter(var1 %in% keep & var2 %in% keep) %>%
  ggplot(., aes(x = cor, fill = cor)) +
  facet_grid(var1 ~ var2) +
  geom_histogram(binwidth = .05) +
  theme_ipsum()
  
# How many bivariate correlations are negative or 0?
big4 <- cors_by_country %>% filter(var1 %in% keep & var2 %in% keep) 
nrow(big4)
sum(big4$cor <= 0, na.rm = TRUE)
mean(big4$cor[big4$cor!=1] <= 0, na.rm = TRUE) 
mean(big4$cor[big4$cor!=1], na.rm = TRUE)

monster <- cy %>%
  select(gwcode, year, itt_alleg_vtcriminal:itt_alleg_vtunst) %>%
  gather(var1, x, -gwcode, -year) %>%
  mutate(var2 = var1, y = x) %>%
  spread(var2, y) %>%
  gather(var2, y, itt_alleg_vtcriminal:itt_alleg_vtunst) %>%
  arrange(gwcode, year, var2, var1) %>%
  group_by(gwcode, year, var2) %>%
  mutate(y = y[!is.na(y)]) %>%
  ungroup() %>%
  # drop in correlations
  left_join(., cors_by_country, by = c("gwcode", "var1", "var2"))

cors <- cors_by_country %>% 
  group_by(var1, var2) %>%
  summarize(cor = mean(cor, na.rm = T)) %>%
  ungroup() %>%
  filter(var1 %in% keep & var2 %in% keep) %>%
  mutate(var1 = str_replace(var1, "itt_alleg_vt", "") %>% str_to_title() %>% dplyr::recode("Unst" = "Unknown"),
         var2 = str_replace(var2, "itt_alleg_vt", "") %>% str_to_title() %>% dplyr::recode("Unst" = "Unknown")) 

p <- monster %>%
  filter(var1 %in% keep & var2 %in% keep) %>%
  mutate(var1 = str_replace(var1, "itt_alleg_vt", "") %>% str_to_title() %>% dplyr::recode("Unst" = "Unknown"),
         var2 = str_replace(var2, "itt_alleg_vt", "") %>% str_to_title() %>% dplyr::recode("Unst" = "Unknown")) %>%
  ggplot(.) +
  facet_grid(var1 ~ var2) +
  scale_x_continuous(trans = "log1p") +
  scale_y_continuous(trans = "log1p") +
  scale_colour_gradient2(limits = c(-1, 1), low = "#a50026", mid = "#ffffbf", high = "#313695") +
  geom_line(alpha = .5, aes(x = x, y = y, group = gwcode, colour = cor)) +
  theme_ipsum() +
  ggtitle("Allegations of torture against different types of victims are only loosely correlated within countries",
          sub = "Each line plots the number of allegations of torture for two victim types in a country from 1995 to 2005.\nThe slope of each line is the within-country correlation between the # of allegations of torture for those two victim types.") +
  labs(x = "ln(# of allegations + 1)", y = "ln(# of allegations + 1)") +
  geom_text(data = cors, aes(x = .5, y = 100, label = paste0("bar(r) == ", round(cor, 2))), 
            parse = TRUE, hjust = 0)
ggsave(p, file = "output/allegations-by-victim-scatterplots.png", height = 8, width = 10, scale = 1.2)


# Binary models using ITT ordinal scale ----------------------------------

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
  geom_point(aes(y = itt_LoTCriminal %in% c("Systematic", "Widespread"), x = LJI))

ggplot(cy) +
  geom_line(aes(x = NY.GDP.PCAP.KD, y = v2x_polyarchy, group = gwcode))

ggplot(cy) +
  geom_line(aes(x = LJI, y = v2x_polyarchy, group = gwcode))


# Bivariate analysis ------------------------------------------------------

p <- cy %>%
  select(gwcode, year, LJI, itt_alleg_vtcriminal:itt_alleg_vtmarginalized, itt_alleg_vtunst) %>%
  gather(yvar, value, starts_with("itt_alleg")) %>%
  mutate(yvar = str_replace(yvar, "itt_alleg_vt", "") %>% str_to_title(),
         yvar = replace(yvar, yvar=="Unst", "Unknown")) %>%
  ggplot(., aes(x = LJI, y = value)) +
  facet_wrap(~ yvar) +
  geom_point(aes(group = gwcode), alpha = .2) +
  #geom_line(aes(group = gwcode), alpha = .2) +
  scale_y_continuous("ln(# allegations + 1)", trans = "log1p") +
  geom_smooth(method = "lm") +
  theme_ipsum() +
  labs(y = "Latent judicial independence") 
ggsave(p, file = "output/scatterplot-itt-allegations-v-lji.png", height = 8, width = 10)

p <- cy %>%
  select(gwcode, year, mrs_legalsys, itt_alleg_vtcriminal:itt_alleg_vtmarginalized, itt_alleg_vtunst) %>%
  gather(yvar, value, starts_with("itt_alleg")) %>%
  mutate(yvar = str_replace(yvar, "itt_alleg_vt", "") %>% str_to_title(),
         yvar = replace(yvar, yvar=="Unst", "Unknown")) %>%
  ggplot(., aes(x = mrs_legalsys, y = value)) +
  facet_wrap(~ yvar) +
  geom_boxplot() +
  scale_y_continuous("ln(# allegations + 1)", trans = "log1p") +
  theme_ipsum() +
  labs(y = "Legaly system type") 
ggsave(p, file = "output/boxplots-itt-allegations-v-legalsys.png", height = 8, width = 10)


# Count models with ITT allegations ---------------------------------------


mdl1 <- list(
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
attr(mdl1, "class") <- "itt"

par(mfrow = c(1, 3))
for (i in 1:3) {
  qqnorm(residuals(mdl1[[i]]), main = names(mdl1)[i])
  qqline(residuals(mdl1[[i]]))
}

plot_predictions(mdl1) +
  scale_x_continuous(limits = c(0, 150))
  


mdl2 <-  list(
  criminal     = glmer(
    itt_alleg_vtcriminal ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + dd_democracy, 
    data = cy, family = poisson(link = "log")),
  dissident    = glmer(
    itt_alleg_vtdissident ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + dd_democracy, 
    data = cy, family = poisson(link = "log")),
  marginalized = glmer(
    itt_alleg_vtmarginalized ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + dd_democracy, 
    data = cy, family = poisson(link = "log"))
)
attr(mdl2, "class") <- "itt"


mdl3 <-  list(
  criminal     = glmer(
    itt_alleg_vtcriminal ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + dd_democracy + LJI, 
    data = cy, family = poisson(link = "log")),
  dissident    = glmer(
    itt_alleg_vtdissident ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + dd_democracy + LJI, 
    data = cy, family = poisson(link = "log")),
  marginalized = glmer(
    itt_alleg_vtmarginalized ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + dd_democracy + LJI, 
    data = cy, family = poisson(link = "log"))
)
attr(mdl3, "class") <- "itt"

# Do a version without random intercepts
mdl3b <-  list(
  criminal     = glm(
    itt_alleg_vtcriminal ~ 1 + NY.GDP.PCAP.KD_ln + dd_democracy + LJI, 
    data = cy, family = poisson(link = "log")),
  dissident    = glm(
    itt_alleg_vtdissident ~ 1 + NY.GDP.PCAP.KD_ln + dd_democracy + LJI, 
    data = cy, family = poisson(link = "log")),
  marginalized = glm(
    itt_alleg_vtmarginalized ~ 1 + NY.GDP.PCAP.KD_ln + dd_democracy + LJI, 
    data = cy, family = poisson(link = "log"))
)

# Do a NB version with random intercepts
mdl3b <-  list(
  criminal     = glmer.nb(
    itt_alleg_vtcriminal ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + dd_democracy + LJI, 
    data = cy),
  dissident    = glmer.nb(
    itt_alleg_vtdissident ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + dd_democracy + LJI, 
    data = cy),
  marginalized = glmer.nb(
    itt_alleg_vtmarginalized ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + dd_democracy + LJI, 
    data = cy)
)
attr(mdl3b, "class") <- "itt"


mdl4 <-  list(
  criminal     = glmer(
    itt_alleg_vtcriminal ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + dd_democracy + mrs_legalsys, 
    data = cy, family = poisson(link = "log")),
  dissident    = glmer(
    itt_alleg_vtdissident ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + dd_democracy + mrs_legalsys, 
    data = cy, family = poisson(link = "log")),
  marginalized = glmer(
    itt_alleg_vtmarginalized ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + dd_democracy + mrs_legalsys, 
    data = cy, family = poisson(link = "log"))
)
attr(mdl4, "class") <- "itt"


png("output/marginal-calibration-plots.png", height = 800, width = 1200)
par(mfrow = c(2, 3), cex = 1.2)
for (i in 1:3) {
  marcal(getME(mdl1[[i]], "y"), predict(mdl1[[i]], type = "response"),
         main = sprintf("Intercept-only (mdl1), '%s'", names(mdl1)[i]))
}
for (i in 1:3) {
  marcal(getME(mdl3[[i]], "y"), predict(mdl3[[i]], type = "response"),
         main = sprintf("Judicial indy (mdl3), '%s'", names(mdl3)[i]))
}
dev.off()

p <- plot_predictions(mdl1) +
  ggtitle("Intercept-only model yhat vs y") 
ggsave(p, file = "output/mdl1-y-vs-yhat.png", height = 5, width = 12)

p <- plot_predictions(mdl3) +
  ggtitle("Judicial independence model yhat vs y") 
ggsave(p, file = "output/mdl3-y-vs-yhat.png", height = 5, width = 12)

coefs <- tibble(model_name = paste0("mdl", 1:4)) %>%
  mutate(estimates = map(model_name, function(x) tidy.itt(get(x)))) %>%
  unnest(estimates) %>%
  # add a dummy row so factor level gets plotted
  bind_rows(., tibble(y = "itt_alleg_vtcriminal", 
                      term = "Legal system Civil\n(reference category)",
                      model_name = "mdl1")) %>%
  mutate(y = str_replace(y, "itt_alleg_vt", "") %>% str_to_title(),
         model_name = factor(model_name) %>%
           fct_recode("M1: Intercepts-only" = "mdl1",
                      "M2: GDP + pop" = "mdl2",
                      "M3: M2 + LJI" = "mdl3",
                      "M4: M2 + legal_system" = "mdl4")) %>%
  mutate(term = factor(term) %>%
           fct_recode("Global intercept" = "(Intercept)",
                      "Country intercepts, SD" = "sd_(Intercept).gwcode",
                      "ln GDP per capita" = "NY.GDP.PCAP.KD_ln",
                      "Judicial independence" = "LJI",
                      "Legal system Common" = "mrs_legalsysCommon",
                      "Legal system Islamic" = "mrs_legalsysIslamic",
                      "Legal system Mixed" = "mrs_legalsysMixed") %>%
           fct_relevel(c("Country intercepts, SD", "Global intercept", 
                         "ln GDP per capita")) %>%
           fct_relevel(c("Legal system Mixed", "Legal system Islamic",
                         "Legal system Common", "Legal system Civil\n(reference category)",
                         "Judicial independence"), after = Inf))

h_width = .7
p <- ggplot(coefs, aes(y = estimate, x = term, color = model_name, group = model_name)) +
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
ggsave(p, file = "output/count-model-coefs.png", height = 5, width = 10)

fit <- tibble(model_name = paste0("mdl", 1:4)) %>%
  mutate(AIC = map(model_name, function(x) {
    res <- AIC.itt(get(x))
    res <- tibble(outcome = names(res), AIC = res)
    })) %>%
  mutate(BIC = map(model_name, function(x) {
    res <- BIC.itt(get(x))
    res <- tibble(outcome = names(res), BIC = res)
  })) %>%
  mutate(MAE = map(model_name, function(x) {
    res <- mae.itt(get(x))
    res <- tibble(outcome = names(res), MAE = res)
  })) %>%
  mutate(RMSE = map(model_name, function(x) {
    res <- rmse.itt(get(x))
    res <- tibble(outcome = names(res), RMSE = res)
  })) %>%
  unnest() %>%
  select(outcome, model_name, AIC, BIC, MAE, RMSE) %>%
  arrange(outcome, model_name)

write_csv(fit, "output/count-model-fit.csv")


# Count model out of sample fit -------------------------------------------

oos_preds <- tibble(model_name = paste0("mdl", 1:4)) %>%
  mutate(preds = map(model_name, function(x) cv_predict.itt(get(x), data = cy, folds = 11))) %>%
  unnest()
oos_fit <- oos_preds %>%
  filter(!is.na(yhat)) %>%
  rename(outcome = yname) %>%
  mutate(outcome = str_replace(outcome, "itt_alleg_vt", "")) %>%
  group_by(model_name, outcome) %>%
  summarize(MAE = mae(y, yhat), RMSE = rmse(y, yhat)) %>%
  arrange(outcome, model_name)
write_csv(oos_fit, "output/count-model-oos-fit.csv")



# What are the average counts by country?
p <- cy %>%
  select(gwcode, year, itt_alleg_vtcriminal:itt_alleg_vtmarginalized) %>%
  gather(y, value, -gwcode, -year) %>%
  ggplot(., aes(x = year, y = value, group = gwcode)) +
  facet_wrap(~ y) +
  geom_line(alpha = .2) +
  theme_ipsum() +
  ggtitle("What we are trying to model")
ggsave(p, file = "output/outcome-time-series.png", height = 5, width = 12)

cy %>%
  select(gwcode, year, itt_alleg_vtcriminal:itt_alleg_vtmarginalized) %>%
  gather(y, value, -gwcode, -year) %>%
  group_by(gwcode, y) %>%
  summarize(avg = mean(value)) %>%
  ggplot(., aes(x = avg)) +
  facet_wrap(~ y) +
  geom_histogram()


# Started working on a model based on XGBoost variable importance, but it's not working so well
mdl5 <-  list(
  criminal     = glmer(
    itt_alleg_vtcriminal ~ (1|gwcode) + 1 + log(NY.GDP.MKTP.KD) + SP.POP.TOTL_ln + log1p(NY.GDP.TOTL.RT.ZS), 
    data = cy, family = poisson(link = "log")),
  dissident    = glmer(
    itt_alleg_vtdissident ~ (1|gwcode) + 1 + log(NY.GDP.MKTP.KD) + SP.POP.TOTL_ln + log1p(NY.GDP.TOTL.RT.ZS), 
    data = cy, family = poisson(link = "log")),
  marginalized = glmer(
    itt_alleg_vtmarginalized ~ (1|gwcode) + 1 + log(NY.GDP.MKTP.KD) + SP.POP.TOTL_ln + log1p(NY.GDP.TOTL.RT.ZS), 
    data = cy, family = poisson(link = "log"))
)


# Do democracies torture only dissidents less?
avg_allegations_by_regime <- cy %>%
  filter(!is.na(regime)) %>%
  select(gwcode, year, itt_alleg_vtcriminal:itt_alleg_vtmarginalized, regime) %>%
  gather(Victim, Allegations, starts_with("itt_alleg")) %>%
  # Clean up victim factor labels
  mutate(Victim = str_replace(Victim, "itt_alleg_vt", "") %>% str_to_title()) %>%
  group_by(Victim, regime) %>%
  summarize(mean_Allegations = mean(Allegations))

p <- ggplot(avg_allegations_by_regime) +
  geom_point(aes(x = mean_Allegations, y = regime, colour = Victim)) +
  geom_path(aes(x = mean_Allegations, y = regime, group = Victim, colour = Victim),
            linetype = 3) +
  theme_ipsum() +
  labs(y = "", x = "Average # of allegations per country-year")
ggsave(p, file = "output/avg-allegations-by-regime.png", height = 3, width = 5)

# It appears that this is the case, compare average by binary democracy/dictatorship
# to confirm
cy %>%
  filter(!is.na(regime)) %>%
  select(gwcode, year, itt_alleg_vtcriminal:itt_alleg_vtmarginalized, dd_democracy) %>%
  gather(Victim, Allegations, starts_with("itt_alleg")) %>%
  # Clean up victim factor labels
  mutate(Victim = str_replace(Victim, "itt_alleg_vt", "") %>% str_to_title()) %>%
  group_by(Victim, dd_democracy) %>%
  summarize(mean_Allegations = mean(Allegations))

# However, there is a lot of variation around the means, so this is not a very
# large pattern. 
cy %>%
  filter(!is.na(regime)) %>%
  select(gwcode, year, itt_alleg_vtcriminal:itt_alleg_vtmarginalized, regime) %>%
  gather(Victim, Allegations, starts_with("itt_alleg")) %>%
  # Clean up victim factor labels
  mutate(Victim = str_replace(Victim, "itt_alleg_vt", "") %>% str_to_title()) %>%
  ggplot(.) +
  geom_boxplot(aes(x = regime, y = Allegations, color = Victim))

# Is it possible that Cheibub et all's semi-presidential and presidential
# democracies include some regimes with authoritarian tendences, which explains
# why as many allegations of criminal, marginalized torture?
# Doesn't seem that way if we look at polyarchy, as there is no clear assocation.
cy %>%
  filter(!is.na(regime)) %>%
  select(gwcode, year, itt_alleg_vtcriminal:itt_alleg_vtmarginalized, v2x_polyarchy) %>%
  gather(Victim, Allegations, starts_with("itt_alleg")) %>%
  # Clean up victim factor labels
  mutate(Victim = str_replace(Victim, "itt_alleg_vt", "") %>% str_to_title()) %>%
  ggplot(., aes(x = v2x_polyarchy, y = Allegations)) +
  facet_wrap(~ Victim) +
  geom_point() +
  geom_smooth()


# Throw xgboost at it -----------------------------------------------------


num_data <- cy %>%
  select(-starts_with("yy_"), -starts_with("itt_LoT"),
         # doesn't play with step_dummy
         -hensel_colonial) %>% 
  mutate(gwcodeUS = as.integer(gwcode==2))
num_data <- recipe(num_data) %>%
  add_role(starts_with("itt_alleg"), new_role = "outcome") %>%
  add_role(gwcode, year, date, new_role = "ID") %>%
  add_role(-starts_with("itt_alleg"), -gwcode, -year, -date, new_role = "predictor") %>%
  step_center(all_numeric(), -all_outcomes(), -has_role("ID"), 
              -itt_RstrctAccess, -dd_democracy, -h_indjudiciary, -gwcodeUS) %>%
  step_scale(all_numeric(), -all_outcomes(), -has_role("ID"), 
             -itt_RstrctAccess, -dd_democracy, -h_indjudiciary, -gwcodeUS) %>%
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
                              max_depth = 6, 
                              min_child_weight = 1, 
                              gamma = 0, 
                              # randomization
                              colsample_bytree = .6, 
                              subsample = .75)

mdlX <- list(
  criminal = train(x = train_x, y = train_y[["itt_alleg_vtcriminal"]],
                   method = "xgbTree", objective = "count:poisson", 
                   eval_metric = "poisson-nloglik", trControl = trControl,
                   tuneGrid = hyperparameters),
  dissident = train(x = train_x, y = train_y[["itt_alleg_vtdissident"]],
                    method = "xgbTree", objective = "count:poisson", 
                    eval_metric = "poisson-nloglik", trControl = trControl,
                    tuneGrid = hyperparameters),
  marginalized = train(x = train_x, y = train_y[["itt_alleg_vtmarginalized"]],
                       method = "xgbTree", objective = "count:poisson", 
                       eval_metric = "poisson-nloglik", trControl = trControl,
                       tuneGrid = hyperparameters)
)

oos_preds <- mdlX %>%
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
ggsave(p, file = "output/mdlX-y-vs-yhat.png", height = 5, width = 12)


fit_xgboost <- oos_preds %>%
  rename(outcome = yname) %>%
  group_by(outcome) %>%
  summarize(MAE = mae(y, yhat),
            RMSE = rmse(y, yhat)) %>%
  mutate(model_name = "XGBoost") %>%
  select(outcome, model_name, MAE, RMSE)
write_csv(fit_xgboost, "output/xgboost-fit.csv")

predictor_importance <- mdlX %>%
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
ggsave(p, file = "output/xgboost-variable-importance.png", height = 8, width = 6)

ice <- FALSE
pd <- mdlX %>%
  map_df(., .id = "outcome", partial, pred.var = "LJI", ice = ice)
pd %>%
  ggplot(., aes(x = LJI, y = yhat)) +
  facet_wrap(~ outcome) + 
  geom_line()

pd <- mdlX %>%
  map_df(., .id = "outcome", partial, pred.var = "NY.GDP.MKTP.KD", ice = ice)
pd %>%
  ggplot(., aes(x = NY.GDP.MKTP.KD, y = yhat)) +
  facet_wrap(~ outcome) + 
  geom_line()


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
