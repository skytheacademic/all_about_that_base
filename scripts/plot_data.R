# Sky Kunkel #
# All About That Base #
# Plot Data #
# 4/5/2023 #

#### Load libraries and data ####
library(tidyverse)

setwd("../")
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