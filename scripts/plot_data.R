# Sky Kunkel #
# All About That Base #
# Plot Data #
# 4/5/2023 #

#### Load libraries and data ####
library(tidyverse)

setwd("../")
d = readRDS("./data/kunkel_final.RDS")

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
# svg("./results/violence_joyplot.svg")
ggplot(d, aes(x = `no.troops`, y = `hq`, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01, alpha = 0.5) +
  scale_fill_gradient(low = "#5b92e5", high = "#000000", space = "Lab",
                      guide = "colourbar", aesthetics = "fill") +
  scale_fill_viridis(name = "no.troops", option = "e") +
  xlim(NA,4250) + labs(title = 'Peacekeeping Troops by Base Type') +
  theme_ipsum() + ylab("") +
  # annotate(geom="text", x=20.5, y=4.5, label= "bar(x)", color="black", parse=T) +
  # annotate(geom="text", x=23.5, y=4.5, label= "= 5.89", color="black") +
  # annotate(geom="text", x=23.5, y=3.5, label="   9.07", color="black") +
  # annotate(geom="text", x=23.5, y=2.5, label="   2.42", color="black") +
  # annotate(geom="text", x=23.5, y=1.5, label="   1.74", color="black") +
  theme(plot.title = element_text(family="Times"), legend.position="none", panel.spacing = unit(0.1, "lines"), 
        strip.text.x = element_text(size = 8), 
        axis.title.x = element_text(size =12, hjust = 0.4)) +
  xlab("Troops")

cairo_pdf("./results/violence_joyplot.pdf", width = 7, height = 5)
joy
dev.off()