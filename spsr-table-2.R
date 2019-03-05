####################################
# spsr-table-2.R

# spillover systematic review
# Table 2 - included texts

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
####################################
rm(list=ls())

source(paste0(here::here(), "/0-config.R"))

# load data
data = read.csv(paste0(data_dir,"spillover-sr-extracted-data.csv"))
ws.tab = readRDS(paste0(data_dir, "wsplot.RDS"))

# Baird et al., 2003 
table2_baird = save_results(
  data = data,
  myid = "30528-980",
  param = "Spillover effect conditional on treatment density"
)

# Shekhawat et al, 2014
# No quantitative estimates
table2_shekhawat = save_results(
    data = data,
    myid = "11528-11",
    param = "Cluster-level spillover effect**"
)

# Paul et al, 1962  
# No quantitative estimates
table2_paul = save_results(
  data = data,
  myid = "11096",
  param = "Vaccine efficacy**"
)

# Perez-Heydrich et al., 2014
table2_perez = save_results(
  data = data,
  myid = "2647-1",
  param = "Spillover effect conditional on treatment density"
)

# Hawley et al., 2003
table2_hawley = save_results(
  data = data,
  myid = "31031-4-r",
  param = "Spillover effect conditional on distance to nearest treated cluster",
  desc = c("Odds ratio for test of trend for malaria and distance to nearest ITN treated compound",
           "Odds ratio for test of trend for high-density parasitemia and distance to nearest ITN treated compound",
           "Odds ratio for test of trend for moderate anemia and distance to nearest ITN treated compound",
           "Odds ratio for test of trend for hemoglobin level and distance to nearest ITN treated compound",
           "Hazard ratio for test of trend for child mortality and distance to nearest ITN treated compound")
)

# Ziegelhofer et al, 2012
table2_ziegelhofer = save_results(
  data = data,
  myid = "30528-469",
  param = "Spillover effect conditional on treatment density"
)

# Miguel and Kremer, 2004 
table2_miguel = save_results(
  data = data,
  myid = "30528",
  param = "Spillover effect conditional on treatment density",
  desc = c("Across school externality on moderate-heavy helminth infection (students within 3km); Regression 1 (Corrected estimates in Aiken 2015 IJE / working paper)",
           "Across school externality on moderate-heavy helminth infection (students 3-6km); Regression 1 (Corrected estimates in Aiken 2015 IJE / working paper)")
)



# Print table 2
table2 = bind_rows(table2_baird,
                   table2_shekhawat,
                   table2_paul,
                   table2_perez,
                   table2_hawley,
                   table2_ziegelhofer,
                   table2_miguel
)

table2 = table2 %>% select(-c(cluster_size, coverage))

table2

write.csv(table2, file = paste0(tab_dir, "spsr-table-2.csv"), row.names=FALSE)





