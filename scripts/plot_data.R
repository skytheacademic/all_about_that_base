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
d_20 = readRDS("./results/20km_results.RDS") %>% 
  mutate(distance = "20km")
d_30 = readRDS("./results/30km_results.RDS") %>% 
  mutate(distance = "30km")

d = rbind(d_2, d_5, d_10, d_20, d_30)
rm(d_2, d_5, d_10, d_20, d_30)





############# GOV & REBEL VIOLENCE - BINARY ############# 

pdf("./results/gov_vac_binary.pdf", width = 8, height = 4)
ggplot(subset(d, distance %in% c("2km", "5km", "10km", "20km", "30km") & binary == 1 & actor == "Gov"),
       aes(x = factor(distance, levels = c("2km", "5km", "10km", "20km", "30km")), y = att, color = factor(distance))) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = att - se, ymax = att + se),
                width = 0.2, position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c("2km" = "#4D858E", "5km" = "#4D858E", "10km" = "#4D858E", "20km" = "#4D858E", "30km" = "#4D858E")) +
  theme_minimal() + labs(title = "", y = "ATT", x = "") +
  theme(strip.text.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y.right = element_blank(),
        legend.position = "none", axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_y_continuous(limits = c(-0.040, 0.001), breaks = seq(-0.040, 0.001, 0.01)) + 
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5)
dev.off()

pdf("./results/reb_vac_binary.pdf", width = 8, height = 4)
ggplot(subset(d, distance %in% c("2km", "5km", "10km", "20km", "30km") & binary == 1 & actor == "Reb"),
       aes(x = factor(distance, levels = c("2km", "5km", "10km", "20km", "30km")), y = att, color = factor(distance))) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = att - se, ymax = att + se),
                width = 0.2, position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c("2km" = "#E38030", "5km" = "#E38030", "10km" = "#E38030", "20km" = "#E38030", "30km" = "#E38030")) +
  theme_minimal() + labs(title = "", y = "ATT", x = "") +
  theme(strip.text.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y.right = element_blank(),
        legend.position = "none", axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_y_continuous(limits = c(-0.040, 0.001), breaks = seq(-0.040, 0.001, 0.01)) + 
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5)
dev.off()

############# GOV & REBEL VIOLENCE - COUNT ############# 

pdf("./results/gov_vac_all.pdf", width = 8, height = 4)
ggplot(subset(d, distance %in% c("2km", "5km", "10km", "20km", "30km") & binary == 0 & actor == "Gov"),
       aes(x = factor(distance, levels = c("2km", "5km", "10km", "20km", "30km")), y = att, color = factor(distance))) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = att - se, ymax = att + se),
                width = 0.2, position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c("2km" = "#4D858E", "5km" = "#4D858E", "10km" = "#4D858E", "20km" = "#4D858E", "30km" = "#4D858E")) +
  theme_minimal() + labs(title = "", y = "ATT", x = "") +
  theme(strip.text.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y.right = element_blank(),
        legend.position = "none", axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_y_continuous(limits = c(-250, 10), breaks = seq(-250, 10, 50)) + 
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5)
dev.off()

pdf("./results/reb_vac_all.pdf", width = 8, height = 4)
ggplot(subset(d, distance %in% c("2km", "5km", "10km", "20km", "30km") & binary == 0 & actor == "Reb"),
       aes(x = factor(distance, levels = c("2km", "5km", "10km", "20km", "30km")), y = att, color = factor(distance))) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = att - se, ymax = att + se),
                width = 0.2, position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c("2km" = "#E38030", "5km" = "#E38030", "10km" = "#E38030", "20km" = "#E38030", "30km" = "#E38030")) +
  theme_minimal() + labs(title = "", y = "ATT", x = "") +
  theme(strip.text.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y.right = element_blank(),
        legend.position = "none", axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_y_continuous(limits = c(-1.25, 0.5), breaks = seq(-1.25, 0.5, 0.5)) + 
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5)
dev.off()










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