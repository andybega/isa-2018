
library("tidyverse")
library("states")
library("hrbrthemes")

source("data-modules/itt/itt.R")

cy <- readRDS("output/cy.rds")

data(gwstates)
cnames <- gwstates %>% group_by(gwcode) %>% summarize(country = unique(country_name)[1])

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
p
ggsave(p, file = "output/figures/allegations-by-victim.png", height = 5, width = 8)

# What are the average counts by country?
p <- cy %>%
  select(gwcode, year, itt_alleg_vtcriminal:itt_alleg_vtmarginalized) %>%
  gather(y, value, -gwcode, -year) %>%
  ggplot(., aes(x = year, y = value, group = gwcode)) +
  facet_wrap(~ y) +
  geom_line(alpha = .2) +
  theme_ipsum() +
  ggtitle("What we are trying to model")
p
ggsave(p, file = "output/figures/outcome-time-series.png", height = 5, width = 12)

cy %>%
  select(gwcode, year, itt_alleg_vtcriminal:itt_alleg_vtmarginalized) %>%
  gather(y, value, -gwcode, -year) %>%
  group_by(gwcode, y) %>%
  summarize(avg = mean(value)) %>%
  ggplot(., aes(x = avg)) +
  facet_wrap(~ y) +
  geom_histogram()

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
p
ggsave(p, file = "output/figures/selected-allegation-counts.png", height = 5, width = 10, scale = 1.2)

p <- itt %>%
  filter(gwcode %in% c(2, 200, 220, 260, 710, 750, 640, 775)) %>%
  left_join(., cnames, by = "gwcode") %>%
  ggplot(., aes(x = year, y = Victim, fill = LoT)) +
  geom_tile() +
  facet_wrap(~ country, nrow = 2) +
  scale_fill_manual(values = col_vals) +
  theme_ipsum() +
  ggtitle("ITT level of torture by victim type for select countries")
p
ggsave(p, file = "output/figures/selected-levels-of-torture.png", height = 5, width = 10, scale = 1.2)


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

pair_cor <- cors_by_country %>% 
  group_by(var1, var2) %>%
  summarize(cor = mean(cor, na.rm = T)) %>%
  ungroup() %>%
  filter(var1 %in% keep & var2 %in% keep) %>%
  mutate(var1 = str_replace(var1, "itt_alleg_vt", "") %>% str_to_title() %>% dplyr::recode("Unst" = "Unknown"),
         var2 = str_replace(var2, "itt_alleg_vt", "") %>% str_to_title() %>% dplyr::recode("Unst" = "Unknown")) 

p <- cors_by_country %>%
  filter(var1!=var2) %>%
  filter(var1 %in% keep & var2 %in% keep) %>%
  mutate(var1 = str_replace(var1, "itt_alleg_vt", "") %>% str_to_title() %>% dplyr::recode("Unst" = "Unknown"),
         var2 = str_replace(var2, "itt_alleg_vt", "") %>% str_to_title() %>% dplyr::recode("Unst" = "Unknown")) %>%
  ggplot(., aes(x = cor)) +
  facet_grid(var1 ~ var2) +
  geom_histogram(binwidth = .05, alpha = .5) +
  scale_x_continuous(limits = c(-1, 1)) +
  theme_ipsum() +
  geom_vline(xintercept = 0, linetype = 3) +
  geom_vline(data = pair_cor, aes(xintercept = cor)) +
  geom_text(data = pair_cor, aes(x = cor - .1, y = 10, label = paste0("bar(r) == ", round(cor, 2))), 
            parse = TRUE, hjust = 1) +
  #ggtitle("Allegations of torture against different types of victims are only loosely correlated within countries",
  #        sub = "Each plot is a histogram of the pairwise correlations within each country for # of allegations of torture of x and y victim types") +
  labs(x = "Correlation between # of allegations of torture in each country for victim types x and y", y = "Count")
p 
ggsave(p, file = "output/figures/allegations-by-victim-pairwise-correlations.png", height = 6, width = 8)

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

p <- monster %>%
  filter(var1 %in% keep & var2 %in% keep) %>%
  mutate(var1 = str_replace(var1, "itt_alleg_vt", "") %>% str_to_title() %>% dplyr::recode("Unst" = "Unknown"),
         var2 = str_replace(var2, "itt_alleg_vt", "") %>% str_to_title() %>% dplyr::recode("Unst" = "Unknown")) %>%
  ggplot(.) +
  facet_grid(var1 ~ var2) +
  geom_point(alpha = .1, size = .5, aes(x = x, y = y), color = "black") +
  scale_x_continuous(trans = "log1p") +
  scale_y_continuous(trans = "log1p") +
  scale_colour_gradient2(limits = c(-1, 1), low = "#a50026", mid = "#ffffbf", high = "#313695") +
  geom_smooth(aes(x = x, y = y, group = gwcode, colour = cor), 
              method = "lm", se = FALSE, formula = y ~ x, size = .5, alpha = .05) +
  #geom_point(alpha = .5, aes(x = x, y = y, group = gwcode, colour = cor))
  #geom_line(alpha = .5, aes(x = x, y = y, group = gwcode, colour = cor)) +
  theme_ipsum() +
  ggtitle("Allegations of torture against different types of victims are only loosely correlated within countries",
          sub = "The slope of each line is the within-country correlation between the # of allegations of torture for those two victim types.") +
  labs(x = "ln(# of allegations + 1)", y = "ln(# of allegations + 1)") +
  geom_text(data = pair_cor, aes(x = .5, y = 100, label = paste0("bar(r) == ", round(cor, 2))), 
            parse = TRUE, hjust = 0)
p
ggsave(p, file = "output/figures/allegations-by-victim-scatterplots.png", height = 8, width = 10, scale = 1.2)


#
#   Are average allegation levels related to wealth?
#   _______________________________


cy %>%
  group_by(gwcode) %>%
  mutate(avg_alleg_criminal = mean(itt_alleg_vtcriminal),
         avg_alleg_dissident = mean(itt_alleg_vtdissident),
         avg_alleg_marginalized = mean(itt_alleg_vtmarginalized)) %>%
  select(gwcode, year, starts_with("avg_alleg_"), starts_with("norm")) %>%
  gather(var, value, -gwcode, -year, -starts_with("norm")) %>%
  ggplot(., aes(x = norm_ln_NY.GDP.MKTP.KD, y = log1p(value))) +
  facet_wrap(~ var) +
  geom_point() +
  geom_smooth(method = "lm")

df <- cy %>%
  group_by(gwcode) %>%
  mutate(avg_alleg_criminal = mean(itt_alleg_vtcriminal),
         avg_alleg_dissident = mean(itt_alleg_vtdissident),
         avg_alleg_marginalized = mean(itt_alleg_vtmarginalized)) %>%
  select(gwcode, year, starts_with("avg_alleg_"), starts_with("norm")) 
  
df %>%
  gather(var, value, -gwcode, -year, -starts_with("norm")) %>%
  ggplot(., aes(x = norm_ln_pop, y = log1p(value))) +
  facet_wrap(~ var) +
  geom_point() +
  geom_smooth(method = "lm")

summary(lm(avg_alleg_criminal ~ norm_ln_NY.GDP.MKTP.KD + norm_ln_pop, data = df))
