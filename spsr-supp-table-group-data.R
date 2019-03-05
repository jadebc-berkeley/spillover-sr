####################################
# spsr-supp-table-group-data.R

# spillover systematic review
# Supplement 7, Table 1

# Studies that estimated spillovers 
# through reduced transmission using 
# group-level data

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
####################################
rm(list=ls())

source(paste0(here::here(), "/0-config.R"))

# load data
data = read.csv(paste0(data_dir,"spillover-sr-extracted-data.csv"))
rob =  read.csv(paste0(data_dir,"spillover-sr-rob-overall.csv"))

# filter to group level parameters
data = data %>% filter(parameter == "Treatment coverage mean" | 
                         parameter == "Treatment coverage effect")

# filter to necessary columns
data = data %>% select(id, authors, pub.year, country, int1, outcome.primary, spillover.scale)

# drop duplicates
data = data[!duplicated(data),]

# merge in rob
rob = rob %>% select(id, quality.rating)
data.m = left_join(data, rob, by="id")

# sort by author + year
data.m = data.m %>% mutate(authyr = paste0(authors, ", ", pub.year)) %>%
  arrange(authyr) %>%
  select(authyr, country, int1, outcome.primary, spillover.scale, quality.rating)

data.m

write.csv(data.m, file = paste0(tab_dir, "spsr-supp7-table1.csv"), row.names=FALSE)

