# Sky Kunkel #
# All About That Base #
# Clean Data #
# 4/4/2023 #

#### Load libraries and data ####
options(max.print=1000000)
library(tidyverse); library(janitor); library(geosphere); library(sf)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../") # back out to main folder
d = readRDS("./data/Geo-PKO-v-2-1.RDS") %>%
  select(-c(source, geosplit, old_xy, geocomment, comment.on.unit, zone.de.confidence, no.tcc, nameoftcc_1, 
            notroopspertcc_1, 
            nameoftcc_2, notroopspertcc_2, nameoftcc_3, notroopspertcc_3, nameoftcc_4, notroopspertcc_4,
            nameoftcc_5, notroopspertcc_5, nameoftcc_6, notroopspertcc_6, nameoftcc_7, notroopspertcc_7,
            nameoftcc_8, notroopspertcc_8,nameoftcc_9, notroopspertcc_9, nameoftcc_10, notroopspertcc_10,
            nameoftcc_11, notroopspertcc_11,nameoftcc_12, notroopspertcc_12, nameoftcc_13, notroopspertcc_13,
            nameoftcc_14, notroopspertcc_14, nameoftcc_15, notroopspertcc_15,nameoftcc_16, notroopspertcc_16,
            nameoftcc_17, notroopspertcc_17, tcc1, tcc2, tcc3, tcc4, tcc5, tcc6, tcc7, tcc8, tcc9, tcc10, tcc11,
            tcc12, tcc13, tcc14, tcc15, tcc16, tcc17, jmco, comments, unmo.coding.quality,cow_code))

# Group the dataframe by latitude and longitude, then assign unique identifiers
df <- d %>%
  group_by(latitude, longitude) %>%
  mutate(group_id = 1000 + cur_group_id()) %>%
  relocate(group_id, .after = longitude)

# verify new variable created correctly
dd = df %>% 
  distinct(latitude, longitude, group_id)
# this code identifies unqiue combinations of these three variables; every observation should be unique
range(table(dd$group_id)) # should be from 1 to 1

rm(d)

# upon visual check, base 2132 has no lat or lon, so remove
df <- df %>% filter(group_id != 2132)


# calculate radii

# Function to create an sf object with circles for each unique group_id
create_circle_sf <- function(df, radius = 2000) {
  circles <- st_sfc()
  
  # Assuming the CRS for your data is EPSG:4326
  crs <- st_crs(4326)
  
  for (group in unique(df$group_id)) {
    group_data <- filter(df, group_id == group)
    center_lat <- mean(group_data$latitude)
    center_lon <- mean(group_data$longitude)
    
    # Create a circle as a Simple Features (sf) object with the same CRS
    circle <- st_buffer(st_sfc(st_point(c(center_lon, center_lat), crs = crs)), dist = radius)
    
    circles <- st_sf(geometry = c(circles, circle))
  }
  
  return(circles)
}

# Apply the function to your dataframe
circle_sf <- create_circle_sf(df)

# Display the result
print(circle_sf)

# Display the result
print(circle_sf)

#### Export Data ####
saveRDS(d, "./data/kunkel_final.RDS")