# Sky Kunkel #
# All About That Base #
# Clean Data #
# 4/4/2023 #

#### Load libraries and data ####

library(tidyverse)

setwd("../")
d = readRDS("./data/Geo-PKO-v-2-1.RDS") %>%
  select(-c(source, geosplit, old_xy, geocomment, comment.on.unit, zone.de.confidence, no.tcc, nameoftcc_1, notroopspertcc_1, 
            nameoftcc_2, notroopspertcc_2, nameoftcc_3, notroopspertcc_3, nameoftcc_4, notroopspertcc_4,
            nameoftcc_5, notroopspertcc_5, nameoftcc_6, notroopspertcc_6, nameoftcc_7, notroopspertcc_7,
            nameoftcc_8, notroopspertcc_8,nameoftcc_9, notroopspertcc_9, nameoftcc_10, notroopspertcc_10,
            nameoftcc_11, notroopspertcc_11,nameoftcc_12, notroopspertcc_12, nameoftcc_13, notroopspertcc_13,
            nameoftcc_14, notroopspertcc_14, nameoftcc_15, notroopspertcc_15,nameoftcc_16, notroopspertcc_16,
            nameoftcc_17, notroopspertcc_17, tcc1, tcc2, tcc3, tcc4, tcc5, tcc6, tcc7, tcc8, tcc9, tcc10, tcc11,
            tcc12, tcc13, tcc14, tcc15, tcc16, tcc17, jmco, comments, unmo.coding.quality,cow_code, adm1.name, 
            adm1.id, prioid))







#### Export Data ####
saveRDS(d, "./data/kunkel_final.RDS")