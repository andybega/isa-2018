
library("tidyverse")

cy <- readRDS("output/cy.rds")

glm(LoTCriminal %in% c("Systematic", "Widespread") ~ 
      NY.GDP.PCAP.KD + LJI,
    data = cy) %>%
  summary()

ggplot(cy) +
  geom_point(aes(y = LoTCriminal %in% c("Systematic", "Widespread"), x = LJI))
