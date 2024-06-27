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

# d = readRDS("./data/kunkel_final.RDS") %>%
#   dplyr::filter(!(no.troops == "unknown")) %>%
#   transform(no.troops = as.numeric(no.troops))
# 
# #### summary statistics ####
# 
# a = d %>%
#   group_by(hq) %>%
#   summarize(min = min(no.troops), max = max(no.troops))
# 
# rm(list = ls())


### Main Models ###

##################################### DEATHS - 2KM #####################################
dd_2 = readRDS("./data/kunkel_final_2km.RDS") %>%
  group_by(base_id) %>% 
  mutate(ucdp_gov_vac_5 = lag(ucdp_gov_vac_5, 1), ucdp_gov_vac_all = lag(ucdp_gov_vac_all, 1),
         ucdp_reb_vac_5 = lag(ucdp_reb_vac_5, 1), ucdp_reb_vac_all = lag(ucdp_reb_vac_all, 1)) %>%
  ungroup()

# make dataframe to save values
d_2 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_2 = rbind(d_2, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov"))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_2 = rbind(d_2,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb"))
rm(out2, es2)
gc()

###### Continuous #######
## State violence ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es3 <- aggte(out3, type = "group", na.rm = T)
summary(es3)
d_2 = rbind(d_2, data.frame(att = es3$overall.att, se = es3$overall.se, binary = 0, actor = "Gov"))
rm(out3, es3)
gc()

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es4 <- aggte(out4, type = "group", na.rm = T)
summary(es4)
d_2 = rbind(d_2, data.frame(att = es4$overall.att, se = es4$overall.se, binary = 0, actor = "Reb"))
rm(out4, es4)
gc()
saveRDS(d_2, "./results/2km_results.RDS")


##################################### DEATHS - 5KM #####################################
rm(list = ls())
dd_5 = readRDS("./data/kunkel_final_5km.RDS") %>%
  group_by(base_id) %>% 
  mutate(ucdp_gov_vac_5 = lag(ucdp_gov_vac_5, 1), ucdp_gov_vac_all = lag(ucdp_gov_vac_all, 1),
         ucdp_reb_vac_5 = lag(ucdp_reb_vac_5, 1), ucdp_reb_vac_all = lag(ucdp_reb_vac_all, 1)) %>%
  ungroup()
# make dataframe to save values
d_5 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_5 = rbind(d_5, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov"))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_5 = rbind(d_5,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb"))
rm(out2, es2)
gc()

###### Continuous #######
## State violence ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es3 <- aggte(out3, type = "group", na.rm = T)
summary(es3)
d_5 = rbind(d_5, data.frame(att = es3$overall.att, se = es3$overall.se, binary = 0, actor = "Gov"))
rm(out3, es3)
gc()

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_5, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es4 <- aggte(out4, type = "group", na.rm = T)
summary(es4)
d_5 = rbind(d_5, data.frame(att = es4$overall.att, se = es4$overall.se, binary = 0, actor = "Reb"))
rm(out4, es4)
gc()

saveRDS(d_5, "./results/5km_results.RDS")

##################################### DEATHS - 10KM #####################################
rm(list = ls())
dd_10 = readRDS("./data/kunkel_final_10km.RDS") %>%
  group_by(base_id) %>% 
  mutate(ucdp_gov_vac_5 = lag(ucdp_gov_vac_5, 1), ucdp_gov_vac_all = lag(ucdp_gov_vac_all, 1),
         ucdp_reb_vac_5 = lag(ucdp_reb_vac_5, 1), ucdp_reb_vac_all = lag(ucdp_reb_vac_all, 1)) %>%
  ungroup()
# make dataframe to save values
d_10 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_10 = rbind(d_10, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov"))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_10 = rbind(d_10,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb"))
rm(out2, es2)
gc()

###### Continuous #######
## State violence ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es3 <- aggte(out3, type = "group", na.rm = T)
summary(es3)
d_10 = rbind(d_10, data.frame(att = es3$overall.att, se = es3$overall.se, binary = 0, actor = "Gov"))
rm(out3, es3)
gc()

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_10, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es4 <- aggte(out4, type = "group", na.rm = T)
summary(es4)
d_10 = rbind(d_10, data.frame(att = es4$overall.att, se = es4$overall.se, binary = 0, actor = "Reb"))
rm(out4, es4)
gc()

saveRDS(d_10, "./results/10km_results.RDS")

##################################### DEATHS - 15KM #####################################
rm(list = ls())
dd_15 = readRDS("./data/kunkel_final_15km.RDS") %>%
  group_by(base_id) %>% 
  mutate(ucdp_gov_vac_5 = lag(ucdp_gov_vac_5, 1), ucdp_gov_vac_all = lag(ucdp_gov_vac_all, 1),
         ucdp_reb_vac_5 = lag(ucdp_reb_vac_5, 1), ucdp_reb_vac_all = lag(ucdp_reb_vac_all, 1)) %>%
  ungroup()
# make dataframe to save values 
d_15 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_15 = rbind(d_15, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov"))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_15 = rbind(d_15,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb"))
rm(out2, es2)
gc()

###### Continuous #######
## State violence ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es3 <- aggte(out3, type = "group", na.rm = T)
summary(es3)
d_15 = rbind(d_15, data.frame(att = es3$overall.att, se = es3$overall.se, binary = 0, actor = "Gov"))
rm(out3, es3)
gc()

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_15, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es4 <- aggte(out4, type = "group", na.rm = T)
summary(es4)
d_15 = rbind(d_15, data.frame(att = es4$overall.att, se = es4$overall.se, binary = 0, actor = "Reb"))
rm(out4, es4)
gc()

saveRDS(d_15, "./results/15km_results.RDS")

##################################### DEATHS - 20KM #####################################
rm(list = ls())
dd_20 = readRDS("./data/kunkel_final_20km.RDS") %>%
  group_by(base_id) %>% 
  mutate(ucdp_gov_vac_5 = lag(ucdp_gov_vac_5, 1), ucdp_gov_vac_all = lag(ucdp_gov_vac_all, 1),
         ucdp_reb_vac_5 = lag(ucdp_reb_vac_5, 1), ucdp_reb_vac_all = lag(ucdp_reb_vac_all, 1)) %>%
  ungroup()
# make dataframe to save values
d_20 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_20 = rbind(d_20, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov"))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_20 = rbind(d_20,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb"))
rm(out2, es2)
gc()

###### Continuous #######
## State violence ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es3 <- aggte(out3, type = "group", na.rm = T)
summary(es3)
d_20 = rbind(d_20, data.frame(att = es3$overall.att, se = es3$overall.se, binary = 0, actor = "Gov"))
rm(out3, es3)
gc()

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_20, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es4 <- aggte(out4, type = "group", na.rm = T)
summary(es4)
d_20 = rbind(d_20, data.frame(att = es4$overall.att, se = es4$overall.se, binary = 0, actor = "Reb"))
rm(out4, es4)
gc()

saveRDS(d_20, "./results/20km_results.RDS")


##################################### DEATHS - 25KM #####################################
rm(list = ls())
dd_25 = readRDS("./data/kunkel_final_25km.RDS") %>%
  group_by(base_id) %>% 
  mutate(ucdp_gov_vac_5 = lag(ucdp_gov_vac_5, 1), ucdp_gov_vac_all = lag(ucdp_gov_vac_all, 1),
         ucdp_reb_vac_5 = lag(ucdp_reb_vac_5, 1), ucdp_reb_vac_all = lag(ucdp_reb_vac_all, 1)) %>%
  ungroup()
# make dataframe to save values
d_25 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_25 = rbind(d_25, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov"))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_25 = rbind(d_25,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb"))
rm(out2, es2)
gc()

###### Continuous #######
## State violence ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es3 <- aggte(out3, type = "group", na.rm = T)
summary(es3)
d_25 = rbind(d_25, data.frame(att = es3$overall.att, se = es3$overall.se, binary = 0, actor = "Gov"))
rm(out3, es3)
gc()

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_25, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es4 <- aggte(out4, type = "group", na.rm = T)
summary(es4)
d_25 = rbind(d_25, data.frame(att = es4$overall.att, se = es4$overall.se, binary = 0, actor = "Reb"))
rm(out4, es4)
gc()

saveRDS(d_25, "./results/25km_results.RDS")

##################################### DEATHS - 30KM #####################################
rm(list = ls())
dd_30 = readRDS("./data/kunkel_final_30km.RDS") %>%
  group_by(base_id) %>% 
  mutate(ucdp_gov_vac_5 = lag(ucdp_gov_vac_5, 1), ucdp_gov_vac_all = lag(ucdp_gov_vac_all, 1),
         ucdp_reb_vac_5 = lag(ucdp_reb_vac_5, 1), ucdp_reb_vac_all = lag(ucdp_reb_vac_all, 1)) %>%
  ungroup()
# make dataframe to save values
d_30 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_30 = rbind(d_30, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov"))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_30 = rbind(d_30,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb"))
rm(out2, es2)
gc()

###### Continuous #######
## State violence ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es3 <- aggte(out3, type = "group", na.rm = T)
summary(es3)
d_30 = rbind(d_30, data.frame(att = es3$overall.att, se = es3$overall.se, binary = 0, actor = "Gov"))
rm(out3, es3)
gc()

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_30, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es4 <- aggte(out4, type = "group", na.rm = T)
summary(es4)
d_30 = rbind(d_30, data.frame(att = es4$overall.att, se = es4$overall.se, binary = 0, actor = "Reb"))
rm(out4, es4)
gc()

saveRDS(d_30, "./results/30km_results.RDS")


##################################### DEATHS - 35KM #####################################
rm(list = ls())
dd_35 = readRDS("./data/kunkel_final_35km.RDS") %>%
  group_by(base_id) %>% 
  mutate(ucdp_gov_vac_5 = lag(ucdp_gov_vac_5, 1), ucdp_gov_vac_all = lag(ucdp_gov_vac_all, 1),
         ucdp_reb_vac_5 = lag(ucdp_reb_vac_5, 1), ucdp_reb_vac_all = lag(ucdp_reb_vac_all, 1)) %>%
  ungroup()
# make dataframe to save values
d_35 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_35 = rbind(d_35, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov"))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_35 = rbind(d_35,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb"))
rm(out2, es2)
gc()

###### Continuous #######
## State violence ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es3 <- aggte(out3, type = "group", na.rm = T)
summary(es3)
d_35 = rbind(d_35, data.frame(att = es3$overall.att, se = es3$overall.se, binary = 0, actor = "Gov"))
rm(out3, es3)
gc()

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_35, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es4 <- aggte(out4, type = "group", na.rm = T)
summary(es4)
d_35 = rbind(d_35, data.frame(att = es4$overall.att, se = es4$overall.se, binary = 0, actor = "Reb"))
rm(out4, es4)
gc()

saveRDS(d_35, "./results/35km_results.RDS")

##################################### DEATHS - 40KM #####################################
rm(list = ls())
dd_40 = readRDS("./data/kunkel_final_40km.RDS") %>%
  group_by(base_id) %>% 
  mutate(ucdp_gov_vac_5 = lag(ucdp_gov_vac_5, 1), ucdp_gov_vac_all = lag(ucdp_gov_vac_all, 1),
         ucdp_reb_vac_5 = lag(ucdp_reb_vac_5, 1), ucdp_reb_vac_all = lag(ucdp_reb_vac_all, 1)) %>%
  ungroup()
# make dataframe to save values
d_40 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_40 = rbind(d_40, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov"))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_40 = rbind(d_40,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb"))
rm(out2, es2)
gc()

###### Continuous #######
## State violence ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es3 <- aggte(out3, type = "group", na.rm = T)
summary(es3)
d_40 = rbind(d_40, data.frame(att = es3$overall.att, se = es3$overall.se, binary = 0, actor = "Gov"))
rm(out3, es3)
gc()

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_40, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es4 <- aggte(out4, type = "group", na.rm = T)
summary(es4)
d_40 = rbind(d_40, data.frame(att = es4$overall.att, se = es4$overall.se, binary = 0, actor = "Reb"))
rm(out4, es4)
gc()

saveRDS(d_40, "./results/40km_results.RDS")


##################################### DEATHS - 45KM #####################################
rm(list = ls())
dd_45 = readRDS("./data/kunkel_final_45km.RDS") %>%
  group_by(base_id) %>% 
  mutate(ucdp_gov_vac_5 = lag(ucdp_gov_vac_5, 1), ucdp_gov_vac_all = lag(ucdp_gov_vac_all, 1),
         ucdp_reb_vac_5 = lag(ucdp_reb_vac_5, 1), ucdp_reb_vac_all = lag(ucdp_reb_vac_all, 1)) %>%
  ungroup()
# make dataframe to save values
d_45 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_45 = rbind(d_45, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov"))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_45 = rbind(d_45,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb"))
rm(out2, es2)
gc()

###### Continuous #######
## State violence ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es3 <- aggte(out3, type = "group", na.rm = T)
summary(es3)
d_45 = rbind(d_45, data.frame(att = es3$overall.att, se = es3$overall.se, binary = 0, actor = "Gov"))
rm(out3, es3)
gc()

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_45, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es4 <- aggte(out4, type = "group", na.rm = T)
summary(es4)
d_45 = rbind(d_45, data.frame(att = es4$overall.att, se = es4$overall.se, binary = 0, actor = "Reb"))
rm(out4, es4)
gc()

saveRDS(d_45, "./results/45km_results.RDS")

##################################### DEATHS - 50KM #####################################
rm(list = ls())
dd_50 = readRDS("./data/kunkel_final_50km.RDS") %>%
  group_by(base_id) %>% 
  mutate(ucdp_gov_vac_5 = lag(ucdp_gov_vac_5, 1), ucdp_gov_vac_all = lag(ucdp_gov_vac_all, 1),
         ucdp_reb_vac_5 = lag(ucdp_reb_vac_5, 1), ucdp_reb_vac_all = lag(ucdp_reb_vac_all, 1)) %>%
  ungroup()
# make dataframe to save values
d_50 = data.frame() %>%
  mutate(att = NA, se = NA, binary = NA, actor = NA)

###### Binary #######
## State violence ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
d_50 = rbind(d_50, data.frame(att = es1$overall.att, se = es1$overall.se, binary = 1, actor = "Gov"))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
d_50 = rbind(d_50,data.frame(att = es2$overall.att, se = es2$overall.se, binary = 1, actor = "Reb"))
rm(out2, es2)
gc()

###### Continuous #######
## State violence ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es3 <- aggte(out3, type = "group", na.rm = T)
summary(es3)
d_50 = rbind(d_50, data.frame(att = es3$overall.att, se = es3$overall.se, binary = 0, actor = "Gov"))
rm(out3, es3)
gc()

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_50, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es4 <- aggte(out4, type = "group", na.rm = T)
summary(es4)
d_50 = rbind(d_50, data.frame(att = es4$overall.att, se = es4$overall.se, binary = 0, actor = "Reb"))
rm(out4, es4)
gc()

saveRDS(d_50, "./results/50km_results.RDS")














# # Define the models and corresponding labels
# models <- c("ucdp_gov_vac_5", "ucdp_reb_vac_5", "ucdp_gov_vac_all", "ucdp_reb_vac_all")
# labels <- c("Gov", "Reb", "Gov", "Reb")
# 
# # Loop through the models
# for (i in seq_along(models)) {
#   set.seed(8675309) # hey jenny
#   
#   # Run the att_gt and aggte functions
#   out <- att_gt(yname = models[i], tname = "time", idname = "base_id", 
#                 gname = "first_treated", data = dd_2, pl = TRUE, cores = 1, 
#                 allow_unbalanced_panel = TRUE, control_group = "notyettreated")
#   
#   es <- aggte(out, type = "group", na.rm = TRUE)
#   
#   # Print the summary
#   print(summary(es))
#   
#   # Append the results to the data frame
#   d_2 <- rbind(d_2, data.frame(att = es$overall.att, se = es$overall.se, binary = ifelse(i %% 2 == 0, 0, 1), actor = labels[i]))
#   
#   # Remove unnecessary objects
#   rm(out, es)
# }