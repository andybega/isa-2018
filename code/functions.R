# Helper functions --------------------------------------------------------

get_preds <- function(x) {
  if (class(x[[1]])[1]=="glm") {
    df <- lapply(x, function(mm) {
      tibble(yname = colnames(mm$model)[1],
             y = mm$model[[1]], 
             yhat = predict(mm, type = "response"))
    })
  } else {
    df <- lapply(x, function(mm) {
      tibble(yname = names(mm@frame)[1],
             y = getME(mm, "y"), 
             yhat = predict(mm, type = "response"))
    })
  }
  
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
  if (class(x[[1]])[1]=="glm") {
    res <- lapply(x, function(x) {
      data.frame(y = colnames(x$model)[1], tidy(x), stringsAsFactors = FALSE)
    })
    out <- bind_rows(res)
  } else {
    res <- lapply(x, function(x) {
      data.frame(y = names(x@frame)[1], tidy(x), stringsAsFactors = FALSE)
    })
    out <- bind_rows(res)
  }
  out
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

# Estimates a model type given by model_form, e.g. "glm_pois" for Poisson 
# regression, for all three ITT outcome variables.
fit_model <- function(spec_str, data, model_form) {
  yvars <- c("itt_alleg_vtcriminal", "itt_alleg_vtdissident", "itt_alleg_vtmarginalized")
  fitted_models <- lapply(yvars, function(yy) {
    # Take out RE in spec if glm model
    if (str_detect(model_form, "glm_")) {
      spec_str <- str_replace(spec_str, " \\(1\\|gwcode\\) \\+", "")
    }
    formula_str <- paste0(yy, spec_str)
    
    if (model_form=="glm_pois") {
      args <- list(formula = eval(as.formula(formula_str)), data = data, family = quote(poisson(link = "log")))
      out <- do.call(glm, args)
    }
    if (model_form=="glmer_pois") {
      args <- list(formula = eval(as.formula(formula_str)), data = data, family = quote(poisson(link = "log")))
      out <- do.call(glmer, args)
    }
    if (model_form=="glm_nb") {
      model <- NULL
    }
    if (model_form=="glmer_nb") {
      model <- NULL
    }
    out
  }) 
  names(fitted_models) <- c("criminal", "dissident", "marginalized")
  structure(
    fitted_models,
    class = "itt"
  )
}

cv_predict <- function(model_list, data, folds) {
  # Obtain OOS predictions via CV
  # model: a model or list of models
  # data: the original data (since different models store data in different values, don't try auto pry)
  # Unlike cv_predict.itt, it returns a list of OOB predictions, one for each model in 
  # input model_list
  if (folds > length(unique(data$year))) {
    stop("Only 11 unique years in data, can't blocked-CV more than that")
  }
  data <- data %>%
    ungroup() %>%
    mutate(.row_index = 1:n()) %>%
    # !!! this is hard coded grouping var
    group_by(year) %>%
    mutate(.fold = paste0("fold", base::sample(1:folds, size = n(), replace = TRUE)))
  
  oob_preds <- map(model_list, df = data, .f = function(mm, df) {
    oob_preds <- map_dfr(1:folds, mm = mm, df = df, .f = function(ff, mm, df) {
      drop_fold <- paste0("fold", ff)
      
      out_of_bag <- df[df$.fold==drop_fold, ]
      train_data <- df[df$.fold!=drop_fold, ]
      new_model  <- update(mm, data = train_data)
      
      if (class(new_model)[1]=="glmerMod") {
        yname <- names(mm@frame)[1]
        yhat  <- predict(new_model, type = "response", newdata = out_of_bag,
                         allow.new.levels = TRUE)
      } else {
        yname <- colnames(x$model)[1]
        yhat  <- predict(new_model, type = "response", newdata = out_of_bag)
      }
      oob_preds <- tibble(yname = yname,
                          y     = out_of_bag[[yname]], 
                          yhat  = yhat,
                          .row_index = out_of_bag[[".row_index"]],
                          .fold      = drop_fold)
      oob_preds
    })
    oob_preds <- oob_preds %>% arrange(.row_index)
    oob_preds
  })
  oob_preds
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
      yname <- ifelse(class(mm)=="glmerMod", names(mm@frame)[1], colnames(x$model)[1])
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

crps.itt <- function(x) {
  sapply(x, function(mm) {
    crps_pois(getME(mm, "y"), predict(mm, type = "response"))
  })
}


rename_terms <- function(term) {
  df <- tibble(term = factor(as.character(term)))
  suppressWarnings({
    df <- df %>%
      mutate(term = fct_recode(term, 
                               "Global intercept" = "(Intercept)",
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
                             "Judicial independence"), after = Inf))
  })
  
  df$term
}
