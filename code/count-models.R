
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


# Base models -------------------------------------------------------------

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
saveRDS(mdl_base1, file = "output/models/mdl_base1.rds")

par(mfrow = c(1, 3))
for (i in 1:3) {
  qqnorm(residuals(mdl_base1[[i]]), main = names(mdl1)[i])
  qqline(residuals(mdl_base1[[i]]))
}

plot_predictions(mdl_base1) +
  scale_x_continuous(limits = c(0, 150))


mdl_base2 <-  list(
  criminal     = glmer(
    itt_alleg_vtcriminal ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + 
      log1p(NY.GDP.TOTL.RT.ZS) + itt_RstrctAccess + internal_confl, 
    data = cy, family = poisson(link = "log")),
  dissident    = glmer(
    itt_alleg_vtdissident ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + 
      log1p(NY.GDP.TOTL.RT.ZS) + itt_RstrctAccess + internal_confl, 
    data = cy, family = poisson(link = "log")),
  marginalized = glmer(
    itt_alleg_vtmarginalized ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + 
      log1p(NY.GDP.TOTL.RT.ZS) + itt_RstrctAccess + internal_confl, 
    data = cy, family = poisson(link = "log"))
)
attr(mdl_base2, "class") <- "itt"
saveRDS(mdl_base2, file = "output/models/mdl_base2.rds")

#cy$SP.POP.TOTL_ln_norm <- scale(cy$SP.POP.TOTL_ln)
#cy$gtd_events_log1p_norm <- scale(log1p(cy$gtd_events))

control <- glmerControl(optCtrl=list(maxfun=2e4))

mdl_base3 <-  list(
  criminal     = glmer(
    itt_alleg_vtcriminal ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + 
      log1p(NY.GDP.TOTL.RT.ZS) + itt_RstrctAccess + internal_confl +
      gtd_events_log1p_norm + log1p(epr_excluded_group_pop) + SP.POP.TOTL_ln_norm
      , 
    data = cy, family = poisson(link = "log"), control = control),
  dissident    = glmer(
    itt_alleg_vtdissident ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + 
      log1p(NY.GDP.TOTL.RT.ZS) + itt_RstrctAccess + internal_confl +
      gtd_events_log1p_norm + log1p(epr_excluded_group_pop) + SP.POP.TOTL_ln_norm
    , 
    data = cy, family = poisson(link = "log"), control = control),
  marginalized = glmer(
    itt_alleg_vtmarginalized ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + 
      log1p(NY.GDP.TOTL.RT.ZS) + itt_RstrctAccess + internal_confl +
      gtd_events_log1p_norm + log1p(epr_excluded_group_pop) + SP.POP.TOTL_ln_norm
    , 
    data = cy, family = poisson(link = "log"), control = control)
)
attr(mdl_base3, "class") <- "itt"
saveRDS(mdl_base3, file = "output/models/mdl_base3.rds")

bind_rows(
  AIC(mdl_base1),
  AIC(mdl_base2),
  AIC(mdl_base3),
  BIC(mdl_base1),
  BIC(mdl_base2),
  BIC(mdl_base3)
)

par(mfrow = c(1, 3))
for (i in 1:3) {
  qqnorm(residuals(mdl_base2[[i]]), main = names(mdl2)[i])
  qqline(residuals(mdl_base2[[i]]))
}

# Democracy ---------------------------------------------------------------


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

ggplot(cy, aes(y = itt_alleg_vtdissident, x = factor(dd_democracy))) +
  geom_violin()

# It appears that this is the case, compare average by binary democracy/dictatorship
# to confirm
cy %>%
  filter(!is.na(regime)) %>%
  select(gwcode, year, itt_alleg_vtcriminal:itt_alleg_vtmarginalized, dd_democracy) %>%
  gather(Victim, Allegations, starts_with("itt_alleg")) %>%
  # Clean up victim factor labels
  mutate(Victim = str_replace(Victim, "itt_alleg_vt", "") %>% str_to_title()) %>%
  group_by(Victim, dd_democracy) %>%
  summarize(mean_Allegations = mean(Allegations),
            median_Allegations = median(Allegations))

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

# Basic bivariate model
mdl_dem1 <-  list(
  criminal     = glmer(
    itt_alleg_vtcriminal ~ (1|gwcode) + 1 + dd_democracy, 
    data = cy, family = poisson(link = "log")),
  dissident    = glmer(
    itt_alleg_vtdissident ~ (1|gwcode) + 1 + dd_democracy, 
    data = cy, family = poisson(link = "log")),
  marginalized = glmer(
    itt_alleg_vtmarginalized ~ (1|gwcode) + 1 + dd_democracy, 
    data = cy, family = poisson(link = "log"))
)
attr(mdl_dem1, "class") <- "itt"
saveRDS(mdl_dem1, file = "output/models/mdl_dem1.rds")

# It's weird to estimate a group-level version of this model because dd_democracy
# is a binary variable and for most countries only has one value over the course
# of the history. 

# Model with controls
mdl_dem2 <-  list(
  criminal     = glmer(
    itt_alleg_vtcriminal ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + 
      log1p(NY.GDP.TOTL.RT.ZS) + itt_RstrctAccess + internal_confl + 
      dd_democracy, 
    data = cy, family = poisson(link = "log")),
  dissident    = glmer(
    itt_alleg_vtdissident ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + 
      log1p(NY.GDP.TOTL.RT.ZS) + itt_RstrctAccess + internal_confl + 
      dd_democracy, 
    data = cy, family = poisson(link = "log")),
  marginalized = glmer(
    itt_alleg_vtmarginalized ~ (1|gwcode) + 1 + NY.GDP.PCAP.KD_ln + 
      log1p(NY.GDP.TOTL.RT.ZS) + itt_RstrctAccess + internal_confl + 
      dd_democracy, 
    data = cy, family = poisson(link = "log"))
)
attr(mdl_dem2, "class") <- "itt"
saveRDS(mdl_dem2, file = "output/models/mdl_dem2.rds")


# Judicial independence ---------------------------------------------------

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


# Basic bivariate model
mdl_lji1 <-  list(
  criminal     = glmer(
    itt_alleg_vtcriminal ~ (1|gwcode) + 1 + LJI, 
    data = cy, family = poisson(link = "log")),
  dissident    = glmer(
    itt_alleg_vtdissident ~ (1|gwcode) + 1 + LJI, 
    data = cy, family = poisson(link = "log")),
  marginalized = glmer(
    itt_alleg_vtmarginalized ~ (1|gwcode) + 1 + LJI, 
    data = cy, family = poisson(link = "log"))
)
attr(mdl_lji1, "class") <- "itt"
saveRDS(mdl_lji1, file = "output/models/mdl_lji1.rds")

# Minimal control
mdl_lji2 <-  list(
  criminal     = glmer(
    itt_alleg_vtcriminal ~ (1|gwcode) + 1 + dd_democracy + LJI, 
    data = cy, family = poisson(link = "log")),
  dissident    = glmer(
    itt_alleg_vtdissident ~ (1|gwcode) + 1 + dd_democracy + LJI, 
    data = cy, family = poisson(link = "log")),
  marginalized = glmer(
    itt_alleg_vtmarginalized ~ (1|gwcode) + 1 + dd_democracy + LJI, 
    data = cy, family = poisson(link = "log"))
)
attr(mdl_lji2, "class") <- "itt"
saveRDS(mdl_lji2, file = "output/models/mdl_lji2.rds")



# Legal system type -------------------------------------------------------

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

# Basic bivariate model
mdl_legal1 <-  list(
  criminal     = glmer(
    itt_alleg_vtcriminal ~ (1|gwcode) + 1 + dd_democracy + mrs_legalsys, 
    data = cy, family = poisson(link = "log")),
  dissident    = glmer(
    itt_alleg_vtdissident ~ (1|gwcode) + 1 + dd_democracy + mrs_legalsys, 
    data = cy, family = poisson(link = "log")),
  marginalized = glmer(
    itt_alleg_vtmarginalized ~ (1|gwcode) + 1 + dd_democracy + mrs_legalsys, 
    data = cy, family = poisson(link = "log"))
)
attr(mdl_legal1, "class") <- "itt"
saveRDS(mdl_legal1, file = "output/models/mdl_legal1.rds")



# Constitutional aspects --------------------------------------------------




















# Random leftovers ------------------------------------------------------



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



