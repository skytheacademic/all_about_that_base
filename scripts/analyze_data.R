# Sky Kunkel #
# All About That Base #
# Analyze Data #
# 4/5/2023 #


#### Load libraries and data ####
library(tidyverse)

setwd("../")
d = readRDS("./data/kunkel_final.RDS") %>%
  dplyr::filter(!(no.troops == "unknown")) %>%
  transform(no.troops = as.numeric(no.troops))

#### summary statistics ####

a = d %>%
  group_by(hq) %>%
  summarize(min = min(no.troops), max = max(no.troops))



