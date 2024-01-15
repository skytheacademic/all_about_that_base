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


# source, geosplit, old_xy, geocomment, comment.on.unit, zone.de.confidence
### read in data, filter out data not relevant to project
geopko = readRDS("./data/geo_pko/Geo-PKO-v-2-1.RDS") %>%
  filter(geosplit != 1) %>% 
  # geosplit is an issue. for example:
  # the MONUSCO mission in 2015 (base_id = 1216) has duplicate observations, where 1 observation is listed in the
  # DRC, but the other is listed in Burundi, yet each have the same coordinates. since there are only 138 of these,
  # and they're unlikely to effect the results, I'm removing them since it's impossible to know where to draw the 
  # circle
  select(year, month, latitude, longitude, hq, prioid) %>%
  # select(-c(source, geosplit, old_xy, geocomment, comment.on.unit, zone.de.confidence, no.tcc, nameoftcc_1, 
  #           notroopspertcc_1, 
  #           nameoftcc_2, notroopspertcc_2, nameoftcc_3, notroopspertcc_3, nameoftcc_4, notroopspertcc_4,
  #           nameoftcc_5, notroopspertcc_5, nameoftcc_6, notroopspertcc_6, nameoftcc_7, notroopspertcc_7,
  #           nameoftcc_8, notroopspertcc_8,nameoftcc_9, notroopspertcc_9, nameoftcc_10, notroopspertcc_10,
  #           nameoftcc_11, notroopspertcc_11,nameoftcc_12, notroopspertcc_12, nameoftcc_13, notroopspertcc_13,
  #           nameoftcc_14, notroopspertcc_14, nameoftcc_15, notroopspertcc_15,nameoftcc_16, notroopspertcc_16,
  #           nameoftcc_17, notroopspertcc_17, tcc1, tcc2, tcc3, tcc4, tcc5, tcc6, tcc7, tcc8, tcc9, tcc10, tcc11,
  #           tcc12, tcc13, tcc14, tcc15, tcc16, tcc17, jmco, comments, unmo.coding.quality,cow_code)) %>%
  distinct() %>%
  mutate(t_ind = 1) # create treatment variable
  # with these variables removed, there are duplicates in the data. it's unclear why (Cil, Fjelde, Hultman, & 
  # Nilsson don't explain duplicates in any of the supporting docs; it may just be due to different sources for 
  # the maps). since I'm running a DiD, thus with a binary treatment, we can leave this problem for another day

# duplicates <- duplicated(geopko)
# 
# # Subset 'test' to keep only the duplicate observations
# duplicate_rows <- geopko[duplicates, ]
# 
# test = geopko %>%
#   filter(mission == "UNFICYP" & year == 2016 & month == 7)

### assign IDs to each base
geopko <- geopko %>% # Group the dataframe by latitude and longitude, then assign unique identifiers
  group_by(latitude, longitude) %>%
  mutate(base_id = 1000 + cur_group_id()) %>%
  relocate(base_id, .after = longitude)

# verify new variable created correctly
dd = geopko %>% 
  distinct(latitude, longitude, base_id)
# this identifies unqiue combinations of these three variables; every observation should be unique
range(table(dd$base_id)) # should be from 1 to 1

rm(dd)

# some bases have no lat or lon, so remove
geopko <- geopko %>% 
  filter(!is.na(latitude) & !is.na(longitude))


### create a full grid of base_id-month-years
all_base_ids <- sort(unique(c(geopko$base_id)))
df <- expand_grid(base_id = all_base_ids, 
                  year = seq(1994, 2020, 1), 
                  month = seq(1, 12, 1))

### join geopko into full data
df = left_join(df, geopko, by = c("base_id", "month", "year")) %>%
  group_by(base_id, year, month, latitude, longitude, prioid, t_ind) %>%
  slice_max(order_by = hq) %>%
  ungroup()

df = df %>%
  mutate(t_ind = ifelse(is.na(t_ind), 0, t_ind))

dd = df %>% 
  distinct(latitude, longitude, base_id) %>%
  filter(!is.na(latitude) & !is.na(longitude))

df = df %>%
  select(-c(latitude, longitude))

df = left_join(df, dd, by = "base_id")

rm(geopko, all_base_ids, dd)
gc()

##### CREATE VARIABLES FOR PACKAGE `DID` #####

### create a unified time variable. this needs to be a positive integer for `did`
df = df %>% 
  mutate(time = (year-1994)*(12) + month)

### split by base_id and make some variables
dd = df %>% as.data.frame() %>% select(base_id, time, t_ind)
dd = split(dd, f = dd$base_id)
dd = lapply(dd, FUN = function(x){
  y = x[which(x$t_ind == 1),]
  # create a "first treated" variable. needs to be 0 for untreated
  x$first_treated = ifelse(nrow(y) == 0, 0, min(y$time))
  # create a "post treated" variable. needs to be 0 until treatment then 1
  x$post_treatment = ifelse(x$first_treated != 0 & x$time >= x$first_treated, 
                             1, 0)
  # create a "treated" variable. needs to be 0 if control and 1 if treated
  x$treated = ifelse(sum(x$t_ind, na.rm = T) > 0, 1, 0)
  x
})
dd = do.call(rbind, dd)
dd = dd[,c("base_id", "time", "first_treated", "treated", "post_treatment")]
# merge back to main df
d = left_join(df, dd, by = c("base_id", "time"))

### clean up
rm(dd, df)

### calculate radii (2, 5, 10, 20, 30 kms) of each base to use for merging
  # each will be it's own dataset (because of the way sf stores data)

##### Merge UCDP data #####
# read in data
dd = read.csv("./data/ucdp_ged/ucdp-actor-221.csv", encoding = "UTF-8") %>% 
  # if there is an error here, it's usually because of the encoding
  rename(a_id = ActorId, org = Org) %>%
  select(c(a_id, org)) # grab data so we can classify actors during OSV
df = read.csv("./data/ucdp_ged/GEDEvent_v22_1.csv") %>%
  # make the date variable a date type
  mutate(date = ymd_hms(date_end)) %>%
  mutate(month = month(date)) %>%
  filter(type_of_violence == 3) %>%
  select(c(date, month, year, side_a_new_id, deaths_civilians, longitude, latitude)) %>%
  rename(ucdp_deaths = deaths_civilians) %>%
  select(-c("date"))

df = left_join(df, dd, by = c("side_a_new_id" = "a_id")) %>%
  select(-c(side_a_new_id)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84") 

rm(dd)

dd = d %>%
  select(c(base_id, year, month, latitude, longitude))

# create single vector of unique bases to speed up computation timeafter calculation, merge into bigger data
base = dd %>% 
  distinct(latitude, longitude, base_id) 
rm(dd)

base = st_as_sf(base, coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84") 

## calculate radii of each base
# 2km
dd_2 = st_buffer(base, dist = 2000)  # distance is in meters
dd_2 <- st_join(dd_2, df) %>%
  as.data.frame() %>%
  select(-geometry)

dd_2 = dd_2 %>%
  group_by(base_id, month, year, org) %>%
  summarize(ucdp_deaths = sum(ucdp_deaths)) %>%
  drop_na(org) %>% 
  ungroup()

dd_2$ucdp_gov_vac_5 = 0
dd_2$ucdp_gov_vac_5[dd_2$org == 4 & dd_2$ucdp_deaths >= 5] = 1
dd_2$ucdp_gov_vac_all = 0
dd_2$ucdp_gov_vac_all[dd_2$org == 4] = dd_2$ucdp_deaths[dd_2$org == 4]
dd_2$ucdp_reb_vac_5 = 0
dd_2$ucdp_reb_vac_5[dd_2$org == 1 & dd_2$ucdp_deaths >= 5] = 1
dd_2$ucdp_reb_vac_all = 0 
dd_2$ucdp_reb_vac_all[dd_2$org==1] = dd_2$ucdp_deaths[dd_2$org==1]

dd_2 = dd_2 %>%
  group_by(base_id, year, month) %>%
  summarize(across(ucdp_gov_vac_5:ucdp_reb_vac_all, sum))

dd_2 = left_join(d, dd_2, by = c("base_id", "year", "month"))

dd_2 = dd_2 %>% 
  mutate(across(ucdp_gov_vac_5:ucdp_reb_vac_all, 
                ~replace_na(.x, 0)))

# 5km
dd_5 = st_buffer(base, dist = 5000)  # distance is in meters
dd_5 <- st_join(dd_5, df) %>%
  as.data.frame() %>%
  select(-geometry)

dd_5 = dd_5 %>%
  group_by(base_id, month, year, org) %>%
  summarize(ucdp_deaths = sum(ucdp_deaths)) %>%
  drop_na(org) %>% 
  ungroup()

dd_5$ucdp_gov_vac_5 = 0
dd_5$ucdp_gov_vac_5[dd_5$org == 4 & dd_5$ucdp_deaths >= 5] = 1
dd_5$ucdp_gov_vac_all = 0
dd_5$ucdp_gov_vac_all[dd_5$org == 4] = dd_5$ucdp_deaths[dd_5$org == 4]
dd_5$ucdp_reb_vac_5 = 0
dd_5$ucdp_reb_vac_5[dd_5$org == 1 & dd_5$ucdp_deaths >= 5] = 1
dd_5$ucdp_reb_vac_all = 0 
dd_5$ucdp_reb_vac_all[dd_5$org==1] = dd_5$ucdp_deaths[dd_5$org==1]

dd_5 = dd_5 %>%
  group_by(base_id, year, month) %>%
  summarize(across(ucdp_gov_vac_5:ucdp_reb_vac_all, sum))

dd_5 = left_join(d, dd_5, by = c("base_id", "year", "month"))

dd_5 = dd_5 %>% 
  mutate(across(ucdp_gov_vac_5:ucdp_reb_vac_all, 
                ~replace_na(.x, 0)))


# 10km
dd_10 = st_buffer(base, dist = 10000)  # distance is in meters
dd_10 <- st_join(dd_10, df) %>%
  as.data.frame() %>%
  select(-geometry)

dd_10 = dd_10 %>%
  group_by(base_id, month, year, org) %>%
  summarize(ucdp_deaths = sum(ucdp_deaths)) %>%
  drop_na(org) %>% 
  ungroup()

dd_10$ucdp_gov_vac_5 = 0
dd_10$ucdp_gov_vac_5[dd_10$org == 4 & dd_10$ucdp_deaths >= 5] = 1
dd_10$ucdp_gov_vac_all = 0
dd_10$ucdp_gov_vac_all[dd_10$org == 4] = dd_10$ucdp_deaths[dd_10$org == 4]
dd_10$ucdp_reb_vac_5 = 0
dd_10$ucdp_reb_vac_5[dd_10$org == 1 & dd_10$ucdp_deaths >= 5] = 1
dd_10$ucdp_reb_vac_all = 0 
dd_10$ucdp_reb_vac_all[dd_10$org==1] = dd_10$ucdp_deaths[dd_10$org==1]

dd_10 = dd_10 %>%
  group_by(base_id, year, month) %>%
  summarize(across(ucdp_gov_vac_5:ucdp_reb_vac_all, sum))

dd_10 = left_join(d, dd_10, by = c("base_id", "year", "month"))

dd_10 = dd_10 %>% 
  mutate(across(ucdp_gov_vac_5:ucdp_reb_vac_all, 
                ~replace_na(.x, 0)))

# 20km
dd_20 = st_buffer(base, dist = 20000)  # distance is in meters
dd_20 <- st_join(dd_20, df) %>%
  as.data.frame() %>%
  select(-geometry)

dd_20 = dd_20 %>%
  group_by(base_id, month, year, org) %>%
  summarize(ucdp_deaths = sum(ucdp_deaths)) %>%
  drop_na(org) %>% 
  ungroup()

dd_20$ucdp_gov_vac_5 = 0
dd_20$ucdp_gov_vac_5[dd_20$org == 4 & dd_20$ucdp_deaths >= 5] = 1
dd_20$ucdp_gov_vac_all = 0
dd_20$ucdp_gov_vac_all[dd_20$org == 4] = dd_20$ucdp_deaths[dd_20$org == 4]
dd_20$ucdp_reb_vac_5 = 0
dd_20$ucdp_reb_vac_5[dd_20$org == 1 & dd_20$ucdp_deaths >= 5] = 1
dd_20$ucdp_reb_vac_all = 0 
dd_20$ucdp_reb_vac_all[dd_20$org==1] = dd_20$ucdp_deaths[dd_20$org==1]

dd_20 = dd_20 %>%
  group_by(base_id, year, month) %>%
  summarize(across(ucdp_gov_vac_5:ucdp_reb_vac_all, sum))

dd_20 = left_join(d, dd_20, by = c("base_id", "year", "month"))

dd_20 = dd_20 %>% 
  mutate(across(ucdp_gov_vac_5:ucdp_reb_vac_all, 
                ~replace_na(.x, 0)))

# 30km
dd_30 = st_buffer(base, dist = 30000)  # distance is in meters
dd_30 <- st_join(dd_30, df) %>%
  as.data.frame() %>%
  select(-geometry)

dd_30 = dd_30 %>%
  group_by(base_id, month, year, org) %>%
  summarize(ucdp_deaths = sum(ucdp_deaths)) %>%
  drop_na(org) %>% 
  ungroup()

dd_30$ucdp_gov_vac_5 = 0
dd_30$ucdp_gov_vac_5[dd_30$org == 4 & dd_30$ucdp_deaths >= 5] = 1
dd_30$ucdp_gov_vac_all = 0
dd_30$ucdp_gov_vac_all[dd_30$org == 4] = dd_30$ucdp_deaths[dd_30$org == 4]
dd_30$ucdp_reb_vac_5 = 0
dd_30$ucdp_reb_vac_5[dd_30$org == 1 & dd_30$ucdp_deaths >= 5] = 1
dd_30$ucdp_reb_vac_all = 0 
dd_30$ucdp_reb_vac_all[dd_30$org==1] = dd_30$ucdp_deaths[dd_30$org==1]

dd_30 = dd_30 %>%
  group_by(base_id, year, month) %>%
  summarize(across(ucdp_gov_vac_5:ucdp_reb_vac_all, sum))

dd_30 = left_join(d, dd_30, by = c("base_id", "year", "month"))

dd_30 = dd_30 %>% 
  mutate(across(ucdp_gov_vac_5:ucdp_reb_vac_all, 
                ~replace_na(.x, 0)))

#### Export Data ####
saveRDS(dd_2, "./data/kunkel_final_2km.RDS")
saveRDS(dd_5, "./data/kunkel_final_5km.RDS")
saveRDS(dd_10, "./data/kunkel_final_10km.RDS")
saveRDS(dd_20, "./data/kunkel_final_20km.RDS")
saveRDS(dd_30, "./data/kunkel_final_30km.RDS")

