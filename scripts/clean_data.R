# Sky Kunkel #
# All About That Base #
# Clean Data #
# 4/4/2023 #

#### Load libraries and data ####
options(max.print=1000000)
library(tidyverse); library(janitor); library(geosphere); library(sf)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../") # back out to main folder

### set seed
set.seed(8675309) # hey jenny

### read in data, filter out data not relevant to project
geopko = readRDS("./data/Geo-PKO-v-2-1.RDS") %>%
  select(-c(source, geosplit, old_xy, geocomment, comment.on.unit, zone.de.confidence, no.tcc, nameoftcc_1, 
            notroopspertcc_1, 
            nameoftcc_2, notroopspertcc_2, nameoftcc_3, notroopspertcc_3, nameoftcc_4, notroopspertcc_4,
            nameoftcc_5, notroopspertcc_5, nameoftcc_6, notroopspertcc_6, nameoftcc_7, notroopspertcc_7,
            nameoftcc_8, notroopspertcc_8,nameoftcc_9, notroopspertcc_9, nameoftcc_10, notroopspertcc_10,
            nameoftcc_11, notroopspertcc_11,nameoftcc_12, notroopspertcc_12, nameoftcc_13, notroopspertcc_13,
            nameoftcc_14, notroopspertcc_14, nameoftcc_15, notroopspertcc_15,nameoftcc_16, notroopspertcc_16,
            nameoftcc_17, notroopspertcc_17, tcc1, tcc2, tcc3, tcc4, tcc5, tcc6, tcc7, tcc8, tcc9, tcc10, tcc11,
            tcc12, tcc13, tcc14, tcc15, tcc16, tcc17, jmco, comments, unmo.coding.quality,cow_code))

duplicates <- duplicated(geopko)

# Subset 'test' to keep only the duplicate observations
duplicate_rows <- geopko[duplicates, ]

# Now 'duplicate_rows' contains the duplicated observations

### assign IDs to each base
geopko <- geopko %>% # Group the dataframe by latitude and longitude, then assign unique identifiers
  group_by(latitude, longitude) %>%
  mutate(base_id = 1000 + cur_group_id()) %>%
  relocate(base_id, .after = longitude)

# verify new variable created correctly
dd = geopko %>% 
  distinct(latitude, longitude, base_id)
# this code identifies unqiue combinations of these three variables; every observation should be unique
range(table(dd$base_id)) # should be from 1 to 1

rm(dd)

# upon visual check, base 2132 has no lat or lon, so remove
geopko <- geopko %>% filter(base_id != 2132)

### create a full grid of base_id-month-years
all_base_ids <- sort(unique(c(geopko$base_id)))
df <- expand_grid(base_id = all_base_ids, 
                  year = seq(1994, 2020, 1), 
                  month = seq(1, 12, 1))

### join geopko into full date
test = left_join(df, geopko, by = c("base_id", "month", "year"))




### calculate radii of each base
 # more computationally efficient to make a dataframe of just base_id and lat/lon, calculate circles, then merge
### set the CRS
proj_crs <- st_crs(prio_shp)

### convert acled to an sf object with a shared CRS
acled <- st_as_sf(acled, coords = c("longitude", "latitude"), crs = proj_crs)

####################################################################
# test stackechange answer #

dd <- st_as_sf(df, coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84") 
  # Reproject to EPSG:3035

# Buffer circles by 1000m
dat_circles <- st_buffer(dd, dist = 1000)

bb <- st_bbox(dat_circles)

pdf("./results/test_plot.pdf")
plot(dat_circles[, "base_id"], 
     xlim = c(mean(c(bb["xmin"], bb["xmax"])) - 100, 
              mean(c(bb["xmin"], bb["xmax"])) + 100), 
     ylim = c(mean(c(bb["ymin"], bb["ymax"])) - 100, 
              mean(c(bb["ymin"], bb["ymax"])) + 100))
dev.off()
plot(ticino_int_circles[, "NAME_3"], add = TRUE)


# end test
####################################################################


#### Export Data ####
saveRDS(d, "./data/kunkel_final.RDS")