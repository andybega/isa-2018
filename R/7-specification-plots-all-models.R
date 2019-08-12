
library("nlme")
library("lme4")
library("dplyr")
library("tidyr")
library("broom")  # for tidy
library("readr")
library("cowplot")
library("tibble")

source("R/functions.R")


# Variables of interest
voi <- c(
  c("ccp_torture", "ccp_prerel", "ccp_habcorp", "ccp_dueproc", "ccp_speedtri"),
  c("v2x_jucon", "v2xlg_legcon", "v2clacjust", "v2clsocgrp", "v2pepwrses", "v2pepwrsoc"),
  c("epr_excluded_groups_count", "epr_excluded_group_pop",
    "dd_democracy")
)

# DV's
dv <- c(
  "itt_alleg_vtcriminal", "itt_alleg_vtdissident", "itt_alleg_vtmarginalized"
)

hl = c(model = "Poisson w RE (GLMER)", base_spec = "Basic controls", 
       pol_glob = "None", year_trend = "None", press_free = "None")

model_list <- read_rds("output/models/model_list.rds")

#' x: a data frame with columns:
#'   - id
#'   - estimate, std.error, p.value
#'   - additional columns defining the specification choices
#'   
#'   
specplot <- function(x, group_labeller = NULL, highlight = NULL) {
  df <- x
  
  spec_table <- df %>%
    select(-std.error, -p.value)
  
  # Highlight as needed
  spec_table$highlight = FALSE
  if (!is.null(highlight)) {
    hl <- as_tibble(as.list(highlight))
    by_vars <- names(hl)
    hl$.mark <- TRUE
    spec_table <- left_join(spec_table, hl, by = by_vars) %>%
      replace_na(list(.mark = FALSE)) %>%
      mutate(highlight = .mark,
             .mark = NULL)
  }
  
  spec_table <- spec_table %>%
    gather(group, element, -id, -estimate, -highlight)
  
  if (!is.null(group_labeller)) {
    spec_table <- spec_table %>%
      mutate(group = group_labeller(group))
  }
  
  p1 <- ggplot(df, aes(x  = reorder(id, estimate), 
                       y = estimate,
                       color = p.value < .05)) + 
    geom_pointrange(aes(ymin = estimate - 1.96 * std.error, 
                        ymax = estimate + 1.96 * std.error)) +
    scale_color_manual(guide = FALSE, 
                       values = c("TRUE" = "blue", "FALSE" = "red")) +
    geom_hline(yintercept = 0) +
    labs(x = "", y = "Coefficient estimate")
  
  p2 <- ggplot(spec_table,
               aes(x = reorder(id, estimate), 
                   y = element,
                   color = highlight)) + 
    geom_point() +
    facet_grid(rows = "group", 
               scales = "free_y",
               space = "free_y", 
               switch = "y") + 
    theme(strip.placement = "outside",
          strip.text.y = element_text(angle = 180, vjust = 1, hjust = 0),
          strip.background = element_blank()) +
    labs(x = "", y = "") +
    scale_color_manual(guide = FALSE, 
                       values = c("FALSE" = "black", "TRUE" = "blue"))
  
  plot_grid(p1, p2, ncol = 1, align = "v", axis = "l")
  
}

group_labeller <- function(x) {
  lvls <- c(
    "model" = "Model",
    "base_spec" = "Base terms",
    "press_free" = "Media freedom",
    "pol_glob"  = "Pol. globalization",
    "year_trend" = "Year trend"
  )
  xnew <- lvls[x]
  idx <- is.na(xnew)
  xnew[idx] <- x[idx]
  names(xnew)[idx] <- x[idx]
  
  factor(xnew, levels = c(unname(lvls), unique(x[idx])))
}



for (i in seq_along(voi)) {
  vv <- voi[i]
  
  flog.info("VOI: %s", vv)
  
  # Subset relevant model list
  ml_i <- model_list[model_list$voi==vv, ]
  
  coefs_i <- lapply(ml_i$id, function(id) {
    fh  <- sprintf("output/models/spec_%s.rds", model_list$id[id])
    mdl <- read_rds(fh)
    
    voi_coef <- tidy(mdl) %>%
      select(y, term, estimate, std.error, p.value) %>%
      filter(term == vv) %>%
      mutate(id = id)
    voi_coef
  })
  coefs_i <- bind_rows(coefs_i)
  coefs_i <- coefs_i %>%
    left_join(ml_i %>% select(-spec, -model_func_name, -voi), by = "id")
  
  for (j in seq_along(dv)) {
    yy <- dv[j]
    
    coefs_ij <- coefs_i %>%
      filter(y==yy) %>%
      select(-y, -term)
    p <- specplot(coefs_ij, group_labeller = group_labeller, highlight = hl) +
      draw_figure_label(sprintf("VOI: %s\nDV: %s", vv, yy))
    fh <- sprintf("output/figures-robustness/specplot-%s-%s.png", vv, yy)
    ggsave(fh, plot = p, height = 8, width = 10)
  }
}




