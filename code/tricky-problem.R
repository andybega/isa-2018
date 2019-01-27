# how do i get this to work with update??

fit_model <- function(spec_str, data, model_form) {
  yvars <- c("itt_alleg_vtcriminal", "itt_alleg_vtdissident", "itt_alleg_vtmarginalized")
  if (model_form=="glm_pois") {
    model <- function(...) {
      args <- match.call(definition = glm, call = match.call()[[2]])
      args <- list(args)
      args
      #do.call(glm, )
      #glm(formula, data, ..., family = poisson(link = "log"))
    }
  }
  if (model_form=="glmer_pois") {
    model <- function(formula, data, ...) {
      glmer(formula, data, ..., family = poisson(link = "log"))
    }
  }
  if (model_form=="glm_nb") {
    model <- NULL
  }
  if (model_form=="glmer_nb") {
    model <- NULL
  }
  
  fitted_models <- sapply(yvars, function(yy) {
    # Take out RE in spec if glm model
    if (str_detect(model_form, "glm_")) {
      spec_str <- str_replace(spec_str, " \\(1\\|gwcode\\) \\+", "")
    }
    formula_str <- paste0(yy, spec_str)
    
  }) 
}