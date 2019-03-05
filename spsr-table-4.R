####################################
# spsr-table-4.R

# spillover systematic review
# Table 4 - included texts

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
####################################
rm(list=ls())

source(paste0(here::here(), "/0-config.R"))

# load data
data = read.csv(paste0(data_dir,"spillover-sr-extracted-data.csv"))
ws.tab = readRDS(paste0(data_dir, "wsplot.RDS"))

# Fitzsimons et al., 2012
table4_fitzsimons = save_results(
  data = data,
  myid = "30528-617",
  param = "Cluster-level spillover effect among ineligibles",
  desc = c("Difference in height-for-age Z-score among those in treated vs. untreated clusters; ineligible",
           "Difference in weight-for-age Z-score among those in treated vs. untreated clusters; ineligible",
           "Difference in weight-for-height Z-score among those in treated vs. untreated clusters; ineligible",
           "Difference in diarrhea among those in treated vs. untreated clusters; ineligible",
           "Difference in vomiting among those in treated vs. untreated clusters; ineligible",
           "Difference in fast breathing among those in treated vs. untreated clusters; ineligible",
           "Difference in fever among those in treated vs. untreated clusters; ineligible",
           "Difference in chills among those in treated vs. untreated clusters; ineligible")
)

# Kazianga et al., 2014
table4_kazianga = save_results(
  data = data,
  myid = "31503",
  param = "Cluster-level spillover effect among ineligibles"
)

# Zivin et al., 2009
table4_zivin = save_results(
  data = data,
  myid = "30528-9",
  param = "Cluster-level spillover effect",
  desc = "Weight-for-height Z-score, Comparison within ARV households, ARVHH (<100 Days) * Round2, Regression 1"
)

# Buttenheim et al., 2011
# No disaggregated results
table4_buttenheim = save_results(
  data = data,
  myid = "18457",
  param = "Cluster-level spillover effect among ineligibles"
)

# Print table 4
table4 = bind_rows(table4_fitzsimons,
                   table4_kazianga,
                   table4_zivin,
                   table4_buttenheim
)

table4 = table4 %>% select(-c(cluster_size, coverage))

table4

write.csv(table4, file = paste0(tab_dir, "spsr-table-4.csv"), row.names=FALSE)





