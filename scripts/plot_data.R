# Sky Kunkel #
# All About That Base #
# Plot Data #
# 4/5/2023 #

#### Load libraries and data ####
library(tidyverse)


# turn off scientific notation
options(scipen = 999)

### set working directory ###
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../") # back out to main folder

############# Plot point estimates of models ############# 
d_2 = readRDS("./results/2km_results.RDS") %>% 
  mutate(distance = "2km")
d_5 = readRDS("./results/5km_results.RDS") %>% 
  mutate(distance = "5km")
d_10 = readRDS("./results/10km_results.RDS") %>% 
  mutate(distance = "10km")
d_15 = readRDS("./results/15km_results.RDS") %>% 
  mutate(distance = "15km")
d_20 = readRDS("./results/20km_results.RDS") %>% 
  mutate(distance = "20km")
d_25 = readRDS("./results/25km_results.RDS") %>% 
  mutate(distance = "25km")
d_30 = readRDS("./results/30km_results.RDS") %>% 
  mutate(distance = "30km")
d_35 = readRDS("./results/35km_results.RDS") %>% 
  mutate(distance = "35km")
d_40 = readRDS("./results/40km_results.RDS") %>% 
  mutate(distance = "40km")
d_45 = readRDS("./results/45km_results.RDS") %>% 
  mutate(distance = "45km")
d_50 = readRDS("./results/50km_results.RDS") %>% 
  mutate(distance = "50km")
d = rbind(d_2, d_5, d_10, d_15, d_20, d_25, d_30, d_35, d_40, d_45, d_50)
rm(d_2, d_5, d_10, d_15, d_20, d_25, d_30, d_35, d_40, d_45, d_50)



############# Plots aggregated ############# 

pdf("./results/gov_vac_binary.pdf", width = 8, height = 4)
ggplot(subset(d, distance %in% c("2km", "5km", "10km", "15km", "20km", "25km", "30km", "35km", "40km", "45km", "50km") & 
                binary == 1 & actor == "Gov"),
       aes(x = factor(distance, levels = c("2km", "5km", "10km", "15km", "20km", "25km", "30km", "35km", "40km", "45km", "50km")), 
           y = att, color = factor(distance))) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = att - se, ymax = att + se),
                width = 0.2, position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c("2km" = "#4D858E", "5km" = "#4D858E", "10km" = "#4D858E", "15km" = "#4D858E", 
                                "20km" = "#4D858E", "25km" = "#4D858E", "30km" = "#4D858E", "35km" = "#4D858E",
                                "40km" = "#4D858E", "45km" = "#4D858E", "50km" = "#4D858E")) +
  theme_minimal() + labs(title = "", y = "ATT", x = "") +
  theme(strip.text.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y.right = element_blank(),
        legend.position = "none", axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  # scale_y_continuous(limits = c(-0.040, 0.001), breaks = seq(-0.040, 0.001, 0.01)) + 
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5)
dev.off()

pdf("./results/reb_vac_binary.pdf", width = 8, height = 4)
ggplot(subset(d, distance %in% c("2km", "5km", "10km", "15km", "20km", "25km", "30km", "35km", "40km", "45km", "50km") & 
                binary == 1 & actor == "Reb"),
       aes(x = factor(distance, levels = c("2km", "5km", "10km", "15km", "20km", "25km", "30km", "35km", "40km", "45km", "50km")), 
           y = att, color = factor(distance))) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = att - se, ymax = att + se),
                width = 0.2, position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c("2km" = "#E38030", "5km" = "#E38030", "10km" = "#E38030", "15km" = "#E38030", 
                                "20km" = "#E38030", "25km" = "#E38030", "30km" = "#E38030", "35km" = "#E38030",
                                "40km" = "#E38030", "45km" = "#E38030", "50km" = "#E38030")) +
  theme_minimal() + labs(title = "", y = "ATT", x = "") +
  theme(strip.text.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y.right = element_blank(),
        legend.position = "none", axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  # scale_y_continuous(limits = c(-0.050, 0.001), breaks = seq(-0.050, 0.001, 0.01)) + 
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5)
dev.off()

############# GOV & REBEL VIOLENCE - COUNT ############# 

pdf("./results/gov_vac_all.pdf", width = 8, height = 4)
ggplot(subset(d, distance %in% c("2km", "5km", "10km", "15km", "20km", "25km", "30km", "35km", "40km", "45km", "50km") & 
                binary == 0 & actor == "Gov"),
       aes(x = factor(distance, levels = c("2km", "5km", "10km", "15km", "20km", "25km", "30km", "35km", "40km", "45km", "50km")), 
           y = att, color = factor(distance))) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = att - se, ymax = att + se),
                width = 0.2, position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c("2km" = "#4D858E", "5km" = "#4D858E", "10km" = "#4D858E", "15km" = "#4D858E", 
                                "20km" = "#4D858E", "25km" = "#4D858E", "30km" = "#4D858E", "35km" = "#4D858E",
                                "40km" = "#4D858E", "45km" = "#4D858E", "50km" = "#4D858E")) +
  theme_minimal() + labs(title = "", y = "ATT", x = "") +
  theme(strip.text.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y.right = element_blank(),
        legend.position = "none", axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  # scale_y_continuous(limits = c(-250, 10), breaks = seq(-250, 10, 50)) + 
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5)
dev.off()

pdf("./results/reb_vac_all.pdf", width = 8, height = 4)
ggplot(subset(d, distance %in% c("2km", "5km", "10km", "15km", "20km", "25km", "30km", "35km", "40km", "45km", "50km") & 
                binary == 0 & actor == "Reb"),
       aes(x = factor(distance, levels = c("2km", "5km", "10km", "15km", "20km", "25km", "30km", "35km", "40km", "45km", "50km")), 
           y = att, color = factor(distance))) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = att - se, ymax = att + se),
                width = 0.2, position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c("2km" = "#E38030", "5km" = "#E38030", "10km" = "#E38030", "15km" = "#E38030", 
                                "20km" = "#E38030", "25km" = "#E38030", "30km" = "#E38030", "35km" = "#E38030",
                                "40km" = "#E38030", "45km" = "#E38030", "50km" = "#E38030")) +
  theme_minimal() + labs(title = "", y = "ATT", x = "") +
  theme(strip.text.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y.right = element_blank(),
        legend.position = "none", axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  # scale_y_continuous(limits = c(-1.25, 0.5), breaks = seq(-1.25, 0.5, 0.5)) + 
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5)
dev.off()

######## Plots by HQ ##########
rm(list = ls())
d_2 = readRDS("./results/2km_results_hq.RDS") %>% 
  mutate(distance = "2km")
d_5 = readRDS("./results/5km_results_hq.RDS") %>% 
  mutate(distance = "5km")
d_10 = readRDS("./results/10km_results_hq.RDS") %>% 
  mutate(distance = "10km")
d_15 = readRDS("./results/15km_results_hq.RDS") %>% 
  mutate(distance = "15km")
d_20 = readRDS("./results/20km_results_hq.RDS") %>% 
  mutate(distance = "20km")
d_25 = readRDS("./results/25km_results_hq.RDS") %>% 
  mutate(distance = "25km")
d_30 = readRDS("./results/30km_results_hq.RDS") %>% 
  mutate(distance = "30km")
d_35 = readRDS("./results/35km_results_hq.RDS") %>% 
  mutate(distance = "35km")
d_40 = readRDS("./results/40km_results_hq.RDS") %>% 
  mutate(distance = "40km")
d_45 = readRDS("./results/45km_results_hq.RDS") %>% 
  mutate(distance = "45km")
d_50 = readRDS("./results/50km_results_hq.RDS") %>% 
  mutate(distance = "50km")
d = rbind(d_2, d_5, d_10, d_15, d_20, d_25, d_30, d_35, d_40, d_45, d_50)
rm(d_2, d_5, d_10, d_15, d_20, d_25, d_30, d_35, d_40, d_45, d_50)


pdf("./results/gov_vac_binary_hq.pdf", width = 16, height = 6)
ggplot(subset(d, distance %in% c("2km", "5km", "10km", "15km", "20km", "25km", "30km", "35km", "40km", "45km", "50km") & 
                binary == 1 & actor == "Gov"),
       aes(x = factor(distance, 
                      levels = c("2km", "5km", "10km", "15km", "20km", "25km", "30km", "35km", "40km", "45km", "50km")), 
           y = att, color = factor(hq))) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = att - se, ymax = att + se),
                width = 0.2, position = position_dodge(width = 0.8)) +
  geom_line(position = position_dodge(width = 0.8), size = 1) +
  theme_minimal() + labs(title = "", y = "ATT", x = "") +
  theme(strip.text.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y.right = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_color_manual(name = "Headquarters", values = c("0" = "red", "1" = "blue", "2" = "orange", "3" = "purple"),
                     labels = c("0" = "Not a headquarters", "1" = "TCC headquarters", "2" = "Mission sector headquarters", "3" = "Mission headquarters")) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5) +
  facet_wrap(~hq, scales = "free_y", labeller = as_labeller(c("0" = "Not a headquarters", 
                                                              "1" = "TCC headquarters", 
                                                              "2" = "Mission sector headquarters", 
                                                              "3" = "Mission headquarters")),
             strip.position = "bottom")  # Adjust strip.position to add space between panels

dev.off()

pdf("./results/reb_vac_binary_hq.pdf", width = 16, height = 6)
ggplot(subset(d, distance %in% c("2km", "5km", "10km", "15km", "20km", "25km", "30km", "35km", "40km", "45km", "50km") & 
                binary == 1 & actor == "Reb"),
       aes(x = factor(distance, 
                      levels = c("2km", "5km", "10km", "15km", "20km", "25km", "30km", "35km", "40km", "45km", "50km")), 
           y = att, color = factor(hq))) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = att - se, ymax = att + se),
                width = 0.2, position = position_dodge(width = 0.8)) +
  geom_line(position = position_dodge(width = 0.8), size = 1) +
  theme_minimal() + labs(title = "", y = "ATT", x = "") +
  theme(strip.text.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y.right = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_color_manual(name = "Headquarters", values = c("0" = "red", "1" = "blue", "2" = "orange", "3" = "purple"),
                     labels = c("0" = "Not a headquarters", "1" = "TCC headquarters", "2" = "Mission sector headquarters", "3" = "Mission headquarters")) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5) +
  facet_wrap(~hq, scales = "free_y", labeller = as_labeller(c("0" = "Not a headquarters", 
                                                              "1" = "TCC headquarters", 
                                                              "2" = "Mission sector headquarters", 
                                                              "3" = "Mission headquarters")),
             strip.position = "bottom")  # Adjust strip.position to add space between panels
dev.off()

pdf("./results/gov_vac_all_hq.pdf", width = 16, height = 6)
ggplot(subset(d, distance %in% c("2km", "5km", "10km", "15km", "20km", "25km", "30km", "35km", "40km", "45km", "50km") & 
                binary == 0 & actor == "Gov"),
       aes(x = factor(distance, 
                      levels = c("2km", "5km", "10km", "15km", "20km", "25km", "30km", "35km", "40km", "45km", "50km")), 
           y = att, color = factor(hq))) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = att - se, ymax = att + se),
                width = 0.2, position = position_dodge(width = 0.8)) +
  geom_line(position = position_dodge(width = 0.8), size = 1) +
  theme_minimal() + labs(title = "", y = "ATT", x = "") +
  theme(strip.text.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y.right = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_color_manual(name = "Headquarters", values = c("0" = "red", "1" = "blue", "2" = "orange", "3" = "purple"),
                     labels = c("0" = "Not a headquarters", "1" = "TCC headquarters", "2" = "Mission sector headquarters", "3" = "Mission headquarters")) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5) +
  facet_wrap(~hq, scales = "free_y", labeller = as_labeller(c("0" = "Not a headquarters", 
                                                              "1" = "TCC headquarters", 
                                                              "2" = "Mission sector headquarters", 
                                                              "3" = "Mission headquarters")),
             strip.position = "bottom")  # Adjust strip.position to add space between panels

dev.off()

pdf("./results/reb_vac_all_hq.pdf", width = 16, height = 6)
ggplot(subset(d, distance %in% c("2km", "5km", "10km", "15km", "20km", "25km", "30km", "35km", "40km", "45km", "50km") & 
                binary == 0 & actor == "Reb"),
       aes(x = factor(distance, 
                      levels = c("2km", "5km", "10km", "15km", "20km", "25km", "30km", "35km", "40km", "45km", "50km")), 
           y = att, color = factor(hq))) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = att - se, ymax = att + se),
                width = 0.2, position = position_dodge(width = 0.8)) +
  geom_line(position = position_dodge(width = 0.8), size = 1) +
  theme_minimal() + labs(title = "", y = "ATT", x = "") +
  theme(strip.text.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y.right = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_color_manual(name = "Headquarters", values = c("0" = "red", "1" = "blue", "2" = "orange", "3" = "purple"),
                     labels = c("0" = "Not a headquarters", "1" = "TCC headquarters", "2" = "Mission sector headquarters", "3" = "Mission headquarters")) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5) +
  facet_wrap(~hq, scales = "free_y", labeller = as_labeller(c("0" = "Not a headquarters", 
                                                              "1" = "TCC headquarters", 
                                                              "2" = "Mission sector headquarters", 
                                                              "3" = "Mission headquarters")),
             strip.position = "bottom")  # Adjust strip.position to add space between panels
dev.off()





############### Parallel Trends Plots ############### 
library(did); library(sf); library(lubridate); library(ggtext)
dd_2 = readRDS("./data/kunkel_final_2km.RDS") %>%
  group_by(base_id) %>% 
  mutate(ucdp_gov_vac_5 = lag(ucdp_gov_vac_5, 1), ucdp_gov_vac_all = lag(ucdp_gov_vac_all, 1),
         ucdp_reb_vac_5 = lag(ucdp_reb_vac_5, 1), ucdp_reb_vac_all = lag(ucdp_reb_vac_all, 1)) %>%
  ungroup()

set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "ucdp_gov_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es1 <- aggte(out1, type = "dynamic", na.rm = T) # extract for parallel trends plot
es1_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es1$egt, ")"),
  event.time= es1$egt,
  estimate  = es1$att.egt,
  std.error = es1$se.egt,
  conf.low  = es1$att.egt - es1$crit.val.egt * es1$se.egt,
  conf.high = es1$att.egt + es1$crit.val.egt  * es1$se.egt,
  point.conf.low  = es1$att.egt - stats::qnorm(1 - es1$DIDparams$alp/2) * es1$se.egt,
  point.conf.high = es1$att.egt + stats::qnorm(1 - es1$DIDparams$alp/2) * es1$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/pt_1.pdf")
ggplot(data = es1_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es1_plot$event.time), max(es1_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es1, es1_plot, out1)


set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "ucdp_reb_vac_5", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es2 <- aggte(out2, type = "dynamic", na.rm = T)
es2_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es2$egt, ")"),
  event.time= es2$egt,
  estimate  = es2$att.egt,
  std.error = es2$se.egt,
  conf.low  = es2$att.egt - es2$crit.val.egt * es2$se.egt,
  conf.high = es2$att.egt + es2$crit.val.egt  * es2$se.egt,
  point.conf.low  = es2$att.egt - stats::qnorm(1 - es2$DIDparams$alp/2) * es2$se.egt,
  point.conf.high = es2$att.egt + stats::qnorm(1 - es2$DIDparams$alp/2) * es2$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/pt_2.pdf")
ggplot(data = es2_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es2_plot$event.time), max(es2_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es2, es2_plot, out2)

## Neighbor cell, enter ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "ucdp_gov_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es3 <- aggte(out3, type = "dynamic", na.rm = T)
es3_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es3$egt, ")"),
  event.time= es3$egt,
  estimate  = es3$att.egt,
  std.error = es3$se.egt,
  conf.low  = es3$att.egt - es3$crit.val.egt * es3$se.egt,
  conf.high = es3$att.egt + es3$crit.val.egt  * es3$se.egt,
  point.conf.low  = es3$att.egt - stats::qnorm(1 - es3$DIDparams$alp/2) * es3$se.egt,
  point.conf.high = es3$att.egt + stats::qnorm(1 - es3$DIDparams$alp/2) * es3$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/pt_3.pdf")
ggplot(data = es3_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es3_plot$event.time), max(es3_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es3, es3_plot, out3)

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "ucdp_reb_vac_all", tname = "time", idname = "base_id", 
               gname = "first_treated",data = dd_2, pl = T, cores = 1, allow_unbalanced_panel = T,
               control_group = "notyettreated")
es4 <- aggte(out4, type = "dynamic", na.rm = T)
es4_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es4$egt, ")"),
  event.time= es4$egt,
  estimate  = es4$att.egt,
  std.error = es4$se.egt,
  conf.low  = es4$att.egt - es4$crit.val.egt * es4$se.egt,
  conf.high = es4$att.egt + es4$crit.val.egt  * es4$se.egt,
  point.conf.low  = es4$att.egt - stats::qnorm(1 - es4$DIDparams$alp/2) * es4$se.egt,
  point.conf.high = es4$att.egt + stats::qnorm(1 - es4$DIDparams$alp/2) * es4$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/pt_4.pdf")
ggplot(data = es4_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es4_plot$event.time), max(es4_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es4, es4_plot, out4)












############### Plot bases troop counts by missions ########################


d = readRDS("./data/kunkel_final.RDS")
d = readRDS("./data/kunkel_final.RDS") %>%
  dplyr::filter(!(no.troops == "unknown")) %>%
  transform(no.troops = as.numeric(no.troops))

#### summary statistics ####

# plot ideas
# need to break it up by mission and then plot

# let's try making joyplot still need to log deaths?
library(ggridges) # geom_density_ridges_gradient
library(viridis)  # scale_fill_viridis
library(hrbrthemes) # theme_ipsum

d$hq[d$hq == 0] = "No HQ"
d$hq[d$hq == 1] = "TCC HQ"
d$hq[d$hq == 2] = "Sector HQ"
d$hq[d$hq == 3] = "Mission HQ"

missions <- c("UNFICYP", "UNIFIL", "UNIKOM", "UNAVEM II", "UNPROFOR", "UNPREDEP", 
              "UNCRO", "MINURSO", "UNOMUR", "UNOMIL", "UNOSOM II", "UNAMIR", 
              "UNMOGIP", "UNOMIG", "MINUGUA", "UNAVEM III", "UNMOT", "UNMIH", 
              "UNTAES", "UNMIBH", "UNTSO", "UNSMIH", "MONUA", "MIPONUH", 
              "MINURCA", "UNOMSIL", "MONUC", "UNAMSIL", "UNMIK", "UNMOP", 
              "MINUCI", "UNMISET", "UNMIL", "ONUB", "UNOCI", "MINUSTAH", 
              "UNMIS", "UNMEE", "UNIOSIL", "UNMIT", "BINUB", "UNAMID", 
              "MINURCAT", "MONUSCO", "UNMISS", "UNISFA", "UNSMIS", "MINUSMA", 
              "MINUSCA", "MINUJUSTH", "ONUMOZ")

# use a loop to create a dataframe for each mission
for (m in missions) {
  assign(m, subset(d, mission == m))
}

# Create a vector of mission names
missions <- c("UNIFIL", "UNAMID", "UNMIL", "MONUSCO", "UNOCI",            # missions w/ max troops
              "BINUB", "MINUCI", "UNSMIH", "MINURCA", "UNCRO", "MIPONUH") # missions w/ min troops

# Loop over the missions and create a ggplot for each
for (m in missions) {
  # Create a ggplot for the current mission
  gg <- ggplot(get(m), aes(x = `no.troops`, y = `hq`, fill = stat(x))) +
    geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01, alpha = 0.5) +
    scale_fill_gradient(low = "#5b92e5", high = "#000000", space = "Lab",
                        guide = "colourbar", aesthetics = "fill") +
    scale_fill_viridis(name = "no.troops", option = "e") +
    labs(title = paste("Peacekeeping Troops by Base Type -", m)) +
    theme_ipsum() + ylab("") +
    theme(plot.title = element_text(family="Times"), legend.position="none", panel.spacing = unit(0.1, "lines"), 
          strip.text.x = element_text(size = 8), 
          axis.title.x = element_text(size =12, hjust = 0.4)) +
    xlab("Troops")
  
  # Save the ggplot to a pdf file
  cairo_pdf(paste0("./results/", tolower(m), "_bases.pdf"), width = 7, height = 5)
  print(gg)
  dev.off()
}


# monuc_gg = ggplot(MONUC, aes(x = `no.troops`, y = `hq`, fill = stat(x))) +
#   geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01, alpha = 0.5) +
#   scale_fill_gradient(low = "#5b92e5", high = "#000000", space = "Lab",
#                       guide = "colourbar", aesthetics = "fill") +
#   scale_fill_viridis(name = "no.troops", option = "e") +
#   labs(title = 'Peacekeeping Troops by Base Type') +
#   theme_ipsum() + ylab("") +
#   theme(plot.title = element_text(family="Times"), legend.position="none", panel.spacing = unit(0.1, "lines"), 
#         strip.text.x = element_text(size = 8), 
#         axis.title.x = element_text(size =12, hjust = 0.4)) +
#   xlab("Troops")
# 
# cairo_pdf("./results/monuc_bases", width = 7, height = 5)
# monuc_gg
# dev.off()