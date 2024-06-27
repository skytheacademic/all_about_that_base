# Sky Kunkel #
# All About That Base #
# Analyze Data #
# 4/5/2023 #


### load packages
library(did); library(sf); library(tidyverse); library(lubridate); library(ggtext)

# turn off scientific notation
options(scipen = 999)

### set working directory ###
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../") # back out to main folder

### set seed
set.seed(8675309) # hey jenny

### Main Models ###


##################################### DEATHS - 2KM #####################################
rm(list = ls())
dd_2_0 = readRDS("./data/dd_2_0.RDS") %>% mutate(first_treated = first_treated_hq0)
dd_2_1 = readRDS("./data/dd_2_1.RDS") %>% mutate(first_treated = first_treated_hq1)
dd_2_2 = readRDS("./data/dd_2_2.RDS") %>% mutate(first_treated = first_treated_hq2) 
dd_2_3 = readRDS("./data/dd_2_3.RDS") %>% mutate(first_treated = first_treated_hq3)

# make dataframe to save values
d_2 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA, hq = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_2 = rbind(d_2, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_2 = rbind(d_2, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_2 = rbind(d_2, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_2 = rbind(d_2, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_2 = rbind(d_2,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_2 = rbind(d_2,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_2 = rbind(d_2,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_2 = rbind(d_2,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_2 = rbind(d_2, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_2 = rbind(d_2, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_2 = rbind(d_2, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_2 = rbind(d_2, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_2 = rbind(d_2,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_2 = rbind(d_2,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_2 = rbind(d_2,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_2 = rbind(d_2,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

saveRDS(d_2, "./results/2km_results_hq.RDS")


##################################### DEATHS - 5KM #####################################
rm(list = ls())
dd_5_0 = readRDS("./data/dd_5_0.RDS") %>% mutate(first_treated = first_treated_hq0)
dd_5_1 = readRDS("./data/dd_5_1.RDS") %>% mutate(first_treated = first_treated_hq1)
dd_5_2 = readRDS("./data/dd_5_2.RDS") %>% mutate(first_treated = first_treated_hq2)
dd_5_3 = readRDS("./data/dd_5_3.RDS") %>% mutate(first_treated = first_treated_hq3)

# make dataframe to save values
d_5 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA, hq = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_5 = rbind(d_5, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_5 = rbind(d_5, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_5 = rbind(d_5, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_5 = rbind(d_5, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_5 = rbind(d_5,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_5 = rbind(d_5,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_5 = rbind(d_5,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_5 = rbind(d_5,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_5 = rbind(d_5, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_5 = rbind(d_5, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_5 = rbind(d_5, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_5 = rbind(d_5, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_5 = rbind(d_5,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_5 = rbind(d_5,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_5 = rbind(d_5,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_5 = rbind(d_5,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

saveRDS(d_5, "./results/5km_results_hq.RDS")

##################################### DEATHS - 10KM #####################################
rm(list = ls())
dd_10_0 = readRDS("./data/dd_10_0.RDS") %>% mutate(first_treated = first_treated_hq0)
dd_10_1 = readRDS("./data/dd_10_1.RDS")%>% mutate(first_treated = first_treated_hq1)
dd_10_2 = readRDS("./data/dd_10_2.RDS") %>% mutate(first_treated = first_treated_hq2)
dd_10_3 = readRDS("./data/dd_10_3.RDS") %>% mutate(first_treated = first_treated_hq3)

# make dataframe to save values
d_10 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA, hq = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_10 = rbind(d_10, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_10 = rbind(d_10, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_10 = rbind(d_10, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_10 = rbind(d_10, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_10 = rbind(d_10,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_10 = rbind(d_10,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_10 = rbind(d_10,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_10 = rbind(d_10,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_10 = rbind(d_10, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_10 = rbind(d_10, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_10 = rbind(d_10, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_10 = rbind(d_10, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_10 = rbind(d_10,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_10 = rbind(d_10,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_10 = rbind(d_10,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_10 = rbind(d_10,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

saveRDS(d_10, "./results/10km_results_hq.RDS")

##################################### DEATHS - 15KM #####################################
rm(list=ls())
dd_15_0 = readRDS("./data/dd_15_0.RDS") %>% mutate(first_treated = first_treated_hq0)
dd_15_1 = readRDS("./data/dd_15_1.RDS")%>% mutate(first_treated = first_treated_hq1)
dd_15_2 = readRDS("./data/dd_15_2.RDS") %>% mutate(first_treated = first_treated_hq2)
dd_15_3 = readRDS("./data/dd_15_3.RDS") %>% mutate(first_treated = first_treated_hq3)

# make dataframe to save values
d_15 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA, hq = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_15 = rbind(d_15, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_15 = rbind(d_15, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_15 = rbind(d_15, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_15 = rbind(d_15, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_15 = rbind(d_15,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_15 = rbind(d_15,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_15 = rbind(d_15,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_15 = rbind(d_15,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_15 = rbind(d_15, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_15 = rbind(d_15, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_15 = rbind(d_15, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_15 = rbind(d_15, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_15 = rbind(d_15,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_15 = rbind(d_15,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_15 = rbind(d_15,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_15 = rbind(d_15,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

saveRDS(d_15, "./results/15km_results_hq.RDS")

##################################### DEATHS - 20KM #####################################
rm(list = ls())
dd_20_0 = readRDS("./data/dd_20_0.RDS") %>% mutate(first_treated = first_treated_hq0)
dd_20_1 = readRDS("./data/dd_20_1.RDS")%>% mutate(first_treated = first_treated_hq1)
dd_20_2 = readRDS("./data/dd_20_2.RDS") %>% mutate(first_treated = first_treated_hq2)
dd_20_3 = readRDS("./data/dd_20_3.RDS") %>% mutate(first_treated = first_treated_hq3)

# make dataframe to save values
d_20 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA, hq = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_20 = rbind(d_20, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_20 = rbind(d_20, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_20 = rbind(d_20, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_20 = rbind(d_20, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_20 = rbind(d_20,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_20 = rbind(d_20,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_20 = rbind(d_20,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_20 = rbind(d_20,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_20 = rbind(d_20, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_20 = rbind(d_20, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_20 = rbind(d_20, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_20 = rbind(d_20, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_20 = rbind(d_20,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_20 = rbind(d_20,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_20 = rbind(d_20,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_20 = rbind(d_20,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

saveRDS(d_20, "./results/20km_results_hq.RDS")

##################################### DEATHS - 25KM #####################################
dd_25_0 = readRDS("./data/dd_25_0.RDS") %>% mutate(first_treated = first_treated_hq0)
dd_25_1 = readRDS("./data/dd_25_1.RDS")%>% mutate(first_treated = first_treated_hq1)
dd_25_2 = readRDS("./data/dd_25_2.RDS") %>% mutate(first_treated = first_treated_hq2)
dd_25_3 = readRDS("./data/dd_25_3.RDS") %>% mutate(first_treated = first_treated_hq3)

# make dataframe to save values
d_25 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA, hq = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_25 = rbind(d_25, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_25 = rbind(d_25, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_25 = rbind(d_25, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_25 = rbind(d_25, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_25 = rbind(d_25,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_25 = rbind(d_25,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_25 = rbind(d_25,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_25 = rbind(d_25,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_25 = rbind(d_25, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_25 = rbind(d_25, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_25 = rbind(d_25, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_25 = rbind(d_25, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_25 = rbind(d_25,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_25 = rbind(d_25,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_25 = rbind(d_25,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_25 = rbind(d_25,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

saveRDS(d_25, "./results/25km_results_hq.RDS")

##################################### DEATHS - 30KM #####################################
rm(list = ls())
dd_30_0 = readRDS("./data/dd_30_0.RDS") %>% mutate(first_treated = first_treated_hq0)
dd_30_1 = readRDS("./data/dd_30_1.RDS")%>% mutate(first_treated = first_treated_hq1)
dd_30_2 = readRDS("./data/dd_30_2.RDS") %>% mutate(first_treated = first_treated_hq2)
dd_30_3 = readRDS("./data/dd_30_3.RDS") %>% mutate(first_treated = first_treated_hq3)

# make dataframe to save values
d_30 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA, hq = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_30 = rbind(d_30, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_30 = rbind(d_30, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_30 = rbind(d_30, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_30 = rbind(d_30, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_30 = rbind(d_30,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_30 = rbind(d_30,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_30 = rbind(d_30,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_30 = rbind(d_30,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_30 = rbind(d_30, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_30 = rbind(d_30, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_30 = rbind(d_30, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_30 = rbind(d_30, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_30 = rbind(d_30,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_30 = rbind(d_30,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_30 = rbind(d_30,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_30 = rbind(d_30,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

saveRDS(d_30, "./results/30km_results_hq.RDS")


##################################### DEATHS - 35KM #####################################
rm(list = ls())
dd_35_0 = readRDS("./data/dd_35_0.RDS") %>% mutate(first_treated = first_treated_hq0)
dd_35_1 = readRDS("./data/dd_35_1.RDS")%>% mutate(first_treated = first_treated_hq1)
dd_35_2 = readRDS("./data/dd_35_2.RDS") %>% mutate(first_treated = first_treated_hq2)
dd_35_3 = readRDS("./data/dd_35_3.RDS") %>% mutate(first_treated = first_treated_hq3)

# make dataframe to save values
d_35 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA, hq = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_35 = rbind(d_35, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_35 = rbind(d_35, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_35 = rbind(d_35, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_35 = rbind(d_35, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_35 = rbind(d_35,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_35 = rbind(d_35,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_35 = rbind(d_35,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_35 = rbind(d_35,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_35 = rbind(d_35, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_35 = rbind(d_35, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_35 = rbind(d_35, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_35 = rbind(d_35, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_35 = rbind(d_35,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_35 = rbind(d_35,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_35 = rbind(d_35,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_35 = rbind(d_35,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

saveRDS(d_35, "./results/35km_results_hq.RDS")


##################################### DEATHS - 40KM #####################################
rm(list = ls())
dd_40_0 = readRDS("./data/dd_40_0.RDS") %>% mutate(first_treated = first_treated_hq0)
dd_40_1 = readRDS("./data/dd_40_1.RDS")%>% mutate(first_treated = first_treated_hq1)
dd_40_2 = readRDS("./data/dd_40_2.RDS") %>% mutate(first_treated = first_treated_hq2)
dd_40_3 = readRDS("./data/dd_40_3.RDS") %>% mutate(first_treated = first_treated_hq3)

# make dataframe to save values
d_40 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA, hq = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_40 = rbind(d_40, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_40 = rbind(d_40, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_40 = rbind(d_40, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_40 = rbind(d_40, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_40 = rbind(d_40,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_40 = rbind(d_40,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_40 = rbind(d_40,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_40 = rbind(d_40,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_40 = rbind(d_40, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_40 = rbind(d_40, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_40 = rbind(d_40, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_40 = rbind(d_40, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_40 = rbind(d_40,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_40 = rbind(d_40,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_40 = rbind(d_40,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_40 = rbind(d_40,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

saveRDS(d_40, "./results/40km_results_hq.RDS")

##################################### DEATHS - 30KM #####################################
rm(list = ls())
dd_45_0 = readRDS("./data/dd_45_0.RDS") %>% mutate(first_treated = first_treated_hq0)
dd_45_1 = readRDS("./data/dd_45_1.RDS")%>% mutate(first_treated = first_treated_hq1)
dd_45_2 = readRDS("./data/dd_45_2.RDS") %>% mutate(first_treated = first_treated_hq2)
dd_45_3 = readRDS("./data/dd_45_3.RDS") %>% mutate(first_treated = first_treated_hq3)

# make dataframe to save values
d_45 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA, hq = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_45 = rbind(d_45, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_45 = rbind(d_45, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_45 = rbind(d_45, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_45 = rbind(d_45, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_45 = rbind(d_45,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_45 = rbind(d_45,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_45 = rbind(d_45,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_45 = rbind(d_45,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_45 = rbind(d_45, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_45 = rbind(d_45, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_45 = rbind(d_45, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_45 = rbind(d_45, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_45 = rbind(d_45,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_45 = rbind(d_45,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_45 = rbind(d_45,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_45 = rbind(d_45,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

saveRDS(d_45, "./results/45km_results_hq.RDS")

##################################### DEATHS - 50KM #####################################
rm(list = ls())
dd_50_0 = readRDS("./data/dd_50_0.RDS") %>% mutate(first_treated = first_treated_hq0)
dd_50_1 = readRDS("./data/dd_50_1.RDS")%>% mutate(first_treated = first_treated_hq1)
dd_50_2 = readRDS("./data/dd_50_2.RDS") %>% mutate(first_treated = first_treated_hq2)
dd_50_3 = readRDS("./data/dd_50_3.RDS") %>% mutate(first_treated = first_treated_hq3)

# make dataframe to save values
d_50 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA, hq = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_50 = rbind(d_50, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_50 = rbind(d_50, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_50 = rbind(d_50, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_50 = rbind(d_50, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_50 = rbind(d_50,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_50 = rbind(d_50,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_50 = rbind(d_50,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_50 = rbind(d_50,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_50 = rbind(d_50, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 0))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_50 = rbind(d_50, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 1))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_50 = rbind(d_50, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 2))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_50 = rbind(d_50, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 0, actor = "Gov", hq = 3))
rm(out1, es1)
gc()

## Rebel violence ##
set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50_0, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_50 = rbind(d_50,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 0))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50_1, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_50 = rbind(d_50,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 1))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_50 = rbind(d_50,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 2))
rm(out2, es2)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50_3, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_50 = rbind(d_50,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 0, actor = "Reb", hq = 3))
rm(out2, es2)
gc()

saveRDS(d_50, "./results/50km_results_hq.RDS")