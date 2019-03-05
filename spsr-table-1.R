####################################
# spsr-table-1.R

# spillover systematic review
# Table 1 - included texts

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
####################################
rm(list=ls())

source(paste0(here::here(), "/0-config.R"))

# load data
data = read.csv(paste0(data_dir,"spillover-sr-extracted-data.csv"))
ws.tab = readRDS(paste0(data_dir, "wsplot.RDS"))

# Ali et al, 2013
table1_ali = save_results(
  data = data,
  myid = "5513",
  param = "Cluster-level spillover effect",
  desc = "Indirect protection; adjusted"
)

# Baptista et al., 2006
table1_baptista = save_results(
  data = data,
  myid = "15212-23",
  param = c(
    "Vaccine efficacy for infectiousness",
    "Vaccine efficacy against illness"
  )
)

# Preziosi & Halloran, 2003
table1_preziosi = save_results(
  data = data,
  myid = "15212",
  param = "Vaccine efficacy for infectiousness"
)

# Ozier 2014
table1_ozier = save_results(
  data = data,
  myid = "30528-548",
  param = "Cluster-level spillover effect"
)

# Roca et al., 2011
table1_roca11 = save_results(
  data = data,
  myid = "15137",
  param = "Spillover effect conditional on exposure to treatment before and after treatment",
  desc = c("Odds ratio for vaccine type pneumococcal carriage among 2-<5 yrs, CSS-3 vs baseline",
           "Odds ratio for vaccine type pneumococcal carriage among 5-<15 yrs, CSS-3 vs baseline",
           "Odds ratio for vaccine type pneumococcal carriage among >=15 yrs, CSS-3 vs baseline")
)

# Roca et al., 2013
table1_roca13 = save_results(
  data = data,
  myid = "15137-11",
  param = "Spillover effect conditional on exposure to treatment before and after treatment",
  desc = c("Odds ratio for vaccine type pneumococcal carriage partially vaccinated villages before and 4 years after mass treatment; 2.5 - <5 years",
           "Odds ratio for vaccine type pneumococcal carriage partially vaccinated villages before and 4 years after mass treatment; 5 to <15 years",
           "Odds ratio for vaccine type pneumococcal carriage partially vaccinated villages before and 4 years after mass treatment; 15+ years")
)

# Khan et al., 2012
table1_khan = save_results(
  data = data,
  myid = "2147-16",
  param = "Cluster-level spillover effect")

# Sur et al., 2009
table1_sur = save_results(
  data = data,
  myid = "2147",
  param = "Cluster-level spillover effect",
  desc = "Indirect protection; adjusted")

# Miguel & Kremer, 2004
table1_miguel = save_results(
  data = data,
  myid = "30528",
  param = "Cluster-level spillover effect",
  desc = "Within school externality on moderate-heavy helminth infection; Regression 2 (Corrected estimates in Aiken 2015 IJE / working paper)")

# Hammitt et al, 2014
table1_hammitt = save_results(
  data = data,
  myid = "15137-14",
  param = "Spillover before and after treatment",
  desc = "Prevalence ratio for vaccine-type Streptococcus pneumoniae in individuals 5 years or older in vaccine vs. baseline period")

# House et al, 2009 - manual calculation
table1_house = ws.tab[ws.tab$id==11528,]
table1_house = table1_house %>% mutate(parameter.scale = "1-RR",
                 units.outcome = "Trachoma") %>%
  filter(parameter == "Cluster-level spillover effect among ineligibles") %>%
  mutate(cluster_size = "Administrative unit with ~ 1400 people", 
         coverage = "82%") %>%
  select(authyr, parameter, int1, cluster_size,
                                  coverage, parameter.scale, units.outcome, 
                                  pt.est, ci.lower, ci.upper)

# Chidambaram et al, 2004
table1_chidambaram = save_results(
  data = data,
  myid = "4204",
  param = "Cluster-level spillover effect",
  desc = "Odds ratio for trachoma among untreated in treated vs. untreated villages")

# Egere et al, 2012
table1_egere = save_results(
  data = data,
  myid = "15137-8",
  param = "Cluster-level spillover effect among ineligibles",
  desc = "Hazard ratio for vaccine type pneumococcal carriage in vaccinated vs. control group")


# Print table 1
table1 = bind_rows(table1_ali,
                   table1_baptista,
                   table1_preziosi,
                   table1_ozier,
                   table1_roca11,
                   table1_roca13,
                   table1_khan,
                   table1_sur,
                   table1_miguel,
                   table1_hammitt,
                   table1_house,
                   table1_chidambaram,
                   table1_egere)
table1

write.csv(table1, file = paste0(tab_dir, "spsr-table-1.csv"), row.names=FALSE)


