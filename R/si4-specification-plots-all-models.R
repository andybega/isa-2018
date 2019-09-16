#
#   Make specification plots for all sensitivity analysis models
#
#   15 August 2019
#
#   depends on s1-estimate-all-models.R output
#

library("nlme")
library("lme4")
library("dplyr")
library("tidyr")
library("broom")  # for tidy
library("readr")
library("cowplot")
library("tibble")
library("futile.logger")
library("ggplot2")

source("R/functions.R")


# Setup list of variables of interest
voi <- c(
  c("dd_democracy"),
  sort(c("ccp_torture", 
         "ccp_prerel", 
         "ccp_habcorp", 
         "ccp_dueproc", 
         "ccp_speedtri")),
  c("epr_excluded_groups_count" = "norm_sqrt_epr_excluded_group_pop", 
    "epr_excluded_group_pop" = "norm_ln1p_epr_excluded_groups_count"),
  sort(c("v2x_jucon", 
         "v2xlg_legcon", 
         "v2clacjust", 
         "v2clsocgrp", 
         "v2pepwrses", 
         "v2pepwrsoc"))
)
names(voi)[names(voi)==""] <- voi[names(voi)==""]

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
specplot <- function(x, group_labeller = NULL, element_labeller = NULL,
                     highlight = NULL) {
  df <- x
  df$id <- reorder(factor(df$id), df$estimate)
  # add 'direction' to encode how lines should be colored
  df <- df %>%
    mutate(
      direction = case_when(
        estimate < 0 & p.value < 0.05 ~ "neg",
        estimate > 0 & p.value < 0.05 ~ "pos",
        p.value > 0.05 ~ "insig"
      ),
      direction = factor(direction, levels = c("neg", "insig", "pos"))
    )
  
  spec_table <- df %>%
    select(-estimate, -std.error, -p.value, -direction)
  
  # Highlight as needed
  spec_table$highlight <- FALSE
  df$highlight         <- FALSE
  if (!is.null(highlight)) {
    hldf <- as_tibble(as.list(highlight))
    by_vars <- names(hldf)
    hldf$.mark <- TRUE
    
    spec_table <- left_join(spec_table, hldf, by = by_vars) %>%
      replace_na(list(.mark = FALSE)) %>%
      mutate(highlight = .mark,
             .mark = NULL)
    df <- left_join(df, hldf, by = by_vars) %>%
      replace_na(list(.mark = FALSE)) %>%
      mutate(highlight = .mark,
             .mark = NULL)
  }
  
  spec_table <- spec_table %>%
    gather(group, element, -id, -highlight) 
  
  if (!is.null(group_labeller)) {
    spec_table <- spec_table %>%
      mutate(group = group_labeller(group))
  }
  if (!is.null(element_labeller)) {
    spec_table <- spec_table %>%
      mutate(element = element_labeller(element))
  }
  
  hl_df <- df %>%
    filter(highlight)
  
  p1 <- ggplot(df, aes(x  = id, 
                       y = estimate,
                       color = direction)) + 
    annotation_raster(alpha("black", .2),
                      xmin = as.integer(hl_df$id) - .5,
                      xmax = as.integer(hl_df$id) + .5,
                      ymin = -Inf, ymax = Inf) +
    geom_pointrange(aes(ymin = estimate - 1.96 * std.error, 
                        ymax = estimate + 1.96 * std.error)) +
    scale_color_manual(guide = FALSE,
                       # colors: wes_palette("Zissou1")[c(1,3,5)]
                       values = c("pos"   = "#3B9AB2", 
                                  "insig" = "gray50",
                                  "neg"   = "#F21A00"), na.value = "gray50",
                       labels = c("Neg & p < 0.05", "p > 0.05", "Pos & p < 0.05", 
                                  "n/a")) +
    geom_hline(yintercept = 0, linetype = 3) +
    labs(x = "", y = "Coefficient estimate") +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  hl_spec <- spec_table %>%
    filter(highlight) 
  
  p2 <- ggplot(spec_table) + 
    geom_rect(data = hl_spec,
              xmin = as.integer(hl_spec$id) - .5,
              xmax = as.integer(hl_spec$id) + .5,
              aes(ymin = -Inf, ymax = Inf),
              fill = "black", alpha = .2) +
    geom_point(aes(x = id,
                   y = element,
                   size = highlight, color = highlight)) +
    facet_grid(rows = "group", 
               scales = "free_y",
               space = "free_y", 
               switch = "y") +
    labs(x = "Specification", y = "") +
    scale_size_manual(guide = FALSE,
                      values = c("FALSE" = 2, "TRUE" = 3)) +
    scale_color_manual(guide = FALSE, 
                       values = c("FALSE" = "gray20", "TRUE" = "gray20")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          strip.placement = "outside",
          strip.text.y = element_text(size = 12,
                                      angle = 180, vjust = 1, hjust = 0,
                                      face = "bold"),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = .5),
          axis.text.y = element_text(size = 12)) 
  
  plot_grid(p1, p2, ncol = 1, align = "v", axis = "l")
  
}

group_labeller <- function(x) {
  lvls <- c(
    "model" = "Model",
    "base_spec"  = "Base terms",
    "press_free" = "Media freedom",
    "pol_glob"   = "Pol. globalization",
    "year_trend" = "Year trend",
    "hro"        = "HRO",
    "econ_glob"  = "Econ. globalization"
  )
  xnew <- lvls[x]
  idx <- is.na(xnew)
  xnew[idx] <- x[idx]
  names(xnew)[idx] <- x[idx]
  
  factor(xnew, levels = c(unname(lvls), unique(x[idx])))
}

element_labeller <- function(x, lvls) {
  lvls <- dget("output/spec_element_factor_levels.txt")
  other_lvls <- setdiff(unique(x), lvls)
  factor(x, levels = rev(c("None", setdiff(lvls, "None"), other_lvls)))
}



for (i in seq_along(voi)) {
  vv_name      <- names(voi)[i]
  vv_indicator <- voi[i]
  
  flog.info("VOI: %s", vv_name)
  
  # Subset relevant model list
  ml_i <- model_list[model_list$voi==vv_name, ]
  
  coefs_i <- lapply(ml_i$id, function(id) {
    fh  <- sprintf("output/models/spec_%04d.rds", model_list$id[id])
    mdl <- read_rds(fh)
    
    voi_coef <- tidy(mdl) %>%
      select(y, term, estimate, std.error, p.value) %>%
      filter(term == vv_indicator) %>%
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
    
    # stats 
    smry <- coefs_ij %>%
      mutate(pos = estimate > 0,
             sig = p.value < 0.05) %>%
      summarize(`Pos sig` = sum(pos & sig),
                `Neg sig` = sum(!pos & sig),
                `Not sig` = sum(!sig))
    smry <- paste0(sprintf("%s: %s", names(smry), smry), collapse = "\n")
    
    p <- specplot(x = coefs_ij, group_labeller = group_labeller, 
                  element_labeller = element_labeller, highlight = hl) +
      draw_plot_label(sprintf("VOI: %s\nDV: %s", vv_name, yy), 
                      x = 0.01, y = .99, 
                      hjust = 0, vjust = 1, fontface = "bold") +
      draw_plot_label(smry,
                      x = 0.01, y = 0.8,
                      hjust = 0, vjust = 1, fontface = "plain", size = 12)
    fh <- sprintf("output/figures-robustness/specplot-%s-%s.png", vv_name, yy)
    ggsave(fh, plot = p, height = 8, width = 10)
  }
}




