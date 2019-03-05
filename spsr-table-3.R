####################################
# spsr-table-3.R

# spillover systematic review
# Table 3 - included texts

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
####################################
rm(list=ls())

source(paste0(here::here(), "/0-config.R"))

# load data
data = read.csv(paste0(data_dir,"spillover-sr-extracted-data.csv"))
ws.tab = readRDS(paste0(data_dir, "wsplot.RDS"))

# Avitabile, 2012    
table3_avitabile = save_results(
  data = data,
  myid = "30971",
  param = "Cluster-level spillover effect among ineligibles",
  desc = c("Impact on non-poor (non-recipients) of health requirements of PROGRESA for cervical cancer screening. Non-poor in treatment villages are 6.1% more likely to get a cervical cancer screening.",
           "Impact on non-poor (non-recipients) of health requirements of PROGRESA for blood sugar screening. Non-poor in treatment villages are 1.0% more likely to get a blood sugar screening.",
           "Impact on non-poor (non-recipients) of health requirements of PROGRESA for blood pressure screening. Non-poor in treatment villages are 2.5% more likely to get a blood pressure screening.")
)

# Handa et al, 2001
table3_handa = save_results(
  data = data,
  myid = "31409",
  param = "Cluster-level spillover effect among ineligibles",
  desc = c("Differences in mean nutrition surveillance rates, Treatment Locality, Round 1-2 (Non-poor)",
           "Differences in mean nutrition surveillance rates, Treatment Locality, Round 1-3 (Non-poor)")
)

# Ribas et al, 2011
table3_ribas = save_results(
  data = data,
  myid = "30528-754",
  param = "Cluster-level spillover effect among ineligibles"
)

# Baird et al, 2013
table3_baird = save_results(
  data = data,
  myid = "30528-137",
  param = "Cluster-level spillover effect",
  desc = c("Change in probability of psychological distress among untreated in treatment group compared to controls; during intervention; adjusted",
           "Change in probability of psychological distress among untreated in treatment group compared to controls; after intervention; adjusted",
           "Change in probability of psychological distress among untreated in treatment group who did not live in treated household compared to controls; during intervention; adjusted",
           "Change in probability of psychological distress among untreated in treatment group who did not live in treated household compared to controls; after intervention; adjusted",
           "Change in probability of psychological distress among untreated in treatment group who did live in treated household compared to controls; during intervention; adjusted",
           "Change in probability of psychological distress among untreated in treatment group who did live in treated household compared to controls; after intervention; adjusted")
)

# Contreras and Maitra, 2013    PROBLEM
table3_contreras = save_results(
  data = data,
  myid = "30971-3",
  param = "Cluster-level spillover effect among ineligibles",
  desc = c("Short run spillover effects; ill in the prior 15 days; all; first follow-up",
           "Medium run spillover effects; ill in the prior 15 days; all; second follow-up",
           "Short run spillover effects; being in bed as a result of the illness; all; first follow-up",
           "Medium run spillover effects; being in bed as a result of the illness; all; second follow-up",
           "Short run spillover effects; hospitalized in the prior year; all; first follow-up",
           "Medium run spillover effects; hospitalized in the prior year; all; second follow-up")
)

# Bhattacharya et al, 2013       
# Exact quantitative estimates not included in paper
table3_bhattacharya = data.frame(
  authyr = "Bhattacharya et al., 2013",
  parameter = "Total effect conditional on treatment density",
  int1 = "Subsidized insecticide-treated nets",
  scale = "",
  coverage = "",
  parameter.scale = "",
  ref.pt.est.desc = "",
  units.outcome = "",
  pt.est = NA,
  ci.lower = NA,
  ci.upper = NA
)

# Banerjee et al., 2010     UPDATE SPREADSHEET PARAMETER
table3_banerjee = save_results(
  data = data,
  myid = "4048",
  param = "Spillover effect conditional on distance to clusters",
  desc = c("Relative risk for # immunizations in villages adjacent to intervention group A and control group",
           "Relative risk for # immunizations in villages adjacent to intervention group B and control group",
           "Relative risk for percent with at least one immunization in villages adjacent to intervention group A and control group",
           "Relative risk for percent with at least one immunization in villages adjacent to intervention group B and control group",
           "Relative risk for percent with BCG scar in villages adjacent to intervention group A and control group",
           "Relative risk for percent with BCG scar in villages adjacent to intervention group B and control group",
           "Relative risk for complete immunization in villages adjacent to intervention group A and control group",
           "Relative risk for complete immunization in villages adjacent to intervention group B and control group")
)

# Tontarawongsa et al., 2011 
table3_tontarawongsa1 = save_results(
  data = data,
  myid = "30528-736",
  param = "Spillover effect conditional on number of social network links",
  desc = c("Spillovers on untreated (Non-BISWA households) in treated areas; recently acquired at least one ITN",
           "Spillovers on untreated (Non-BISWA households) in treated areas; Fraction of Non-BISWA household members that slept under an ITN last night",
           "Spillovers on untreated (Non-BISWA households) in treated areas; recently acquired at least one ITN",
           "Spillovers on untreated (Non-BISWA households) in treated areas; Fraction of Non-BISWA household members that slept under an ITN last night")
)

table3_tontarawongsa2 = save_results(
  data = data,
  myid = "30528-736",
  param = "Spillover effect conditional on number of social network links' ITN acquisition and use",
  desc = c("IV Estimation of peer effects in bed net ownership and usage: Non-BISWA households in a village recently purchased at least one ITN given Average per capita bednets owned by peers",
           "IV Estimation of peer effects in bed net ownership and usage: Fraction of Non-BISWA household members that slept under an ITN last night given Average per capita bednets owned by peers",
           "IV Estimation of peer effects in bed net ownership and usage: Non-BISWA households in a village recently purchased at least one bednet given Average per capita bednets owned by peers",
           "IV Estimation of peer effects in bed net ownership and usage: Fraction of Non-BISWA household members that slept under at least one bednet last night given Average per capita bednets owned by peers")
)

table3_tontarawongsa3 = save_results(
  data = data,
  myid = "30528-736",
  param = "Spillover effect conditional on number of social network links' ITN acquisition and use",
  desc = c("IV Estimation of peer effects in ITN ownership and usage: Non-BISWA households in a village recently purchased at least one ITN given Average last night ITN usage among peers",
           "IV Estimation of peer effects in ITN ownership and usage: Fraction of Non-BISWA household members that slept under an ITN last night, given Average last night ITN usage among peers",
           "IV Estimation of peer effects in ITN ownership and usage: Non-BISWA households in a village recently purchased at least one ITN given Average last night ITN usage among peers",
           "IV Estimation of peer effects in ITN ownership and usage: Fraction of Non-BISWA household members that slept under an ITN last night, given Average last night ITN usage among peers")
)

# Godlonton and Thornton, 2012  
table3_godlonton = save_results(
  data = data,
  myid = "30528-179",
  param = "Total effect conditional on outcome density",
  desc = c("Probability of learning HIV test result associated with % of neighbors within 0.5 km learning test result; all respondents; IV; Table 2",
           "Probability of learning HIV test result associated with any incentive * % of neighbors within 0.5 km learning test result; all respondents IV")
)

# Chong et al., 2013 
table3_chong1 = save_results(
  data = data,
  myid = "31157",
  param = "Cluster-level spillover effect"
)

table3_chong2 = save_results(
  data = data,
  myid = "31157",
  param = "Spillover effect conditional on treatment density"
)

# German et al., 2012 
table3_german = save_results(
  data = data,
  myid = "3272",
  param = "Spillover effect among social network members",
  desc = c("Difference in CES-D among network controls; adjusted for baseline CES-D + covariates",
           "Difference in CES-D among network intervention group; adjusted for baseline CES-D + covariates")
)

# Kremer and Miguel, 2007
table3_kremer = save_results(
  data = data,
  myid = "30528-167",
  param = "Overall effect conditional on number of social network links",
  desc = "Social effect in parent networks; Association between child deworming and number of parent links to early treatment schools; adjusted"
)

# Janssens et al, 2006  
table3_janssens1 = save_results(
  data = data,
  myid = "16384",
  param = "Cluster-level spillover effect",
  desc = c("IV treatment-on-untreated tuberculosis immunization",
           "IV treatment-on-untreated DTP immunization",
           "IV treatment-on-untreated measles immunization")
)

table3_janssens2 = save_results(
  data = data,
  myid = "16384",
  param = "Cluster-level spillover effect in which controls are matched to the untreated",
  desc = c("Impact of Village dummy on TB vaccination. (PSM separately controls for selection into treatment.) This should have been the preferred specification. (Also: they omit the instruments used in the other specifications from the propensity score.)",
           "Impact of Village dummy on DTP vaccination. (PSM separately controls for selection into treatment.) This should have been the preferred specification. (Also: they omit the instruments used in the other specifications from the propensity score.)",
           "Impact of Village dummy on Measles vaccination. (PSM separately controls for selection into treatment.)  This should have been the preferred specification. (Also: they omit the instruments used in the other specifications from the propensity score.)")
)

# Dupas, 2006 
table3_dupas = save_results(
  data = data,
  myid = "30528-196",
  param = "Spillover effect conditional on treatment density",
  desc = c("Simple difference in probability untreated girls ever had sex and ever used a condom associated with the share of treated girls in their school; IV-2SLS",
           "Simple difference in probability untreated girls ever had sex and ever used a condom associated with the share of treated boys in their school; IV-2SLS",
           "Simple difference in probability untreated boys ever had sex and ever used a condom associated with the share of treated girls in their school; IV-2SLS",
           "Simple difference in probability untreated boys ever had sex and ever used a condom associated with the share of treated boys in their school; IV-2SLS")
)

# Azad, 2010   
# Manual calculation

azad = data %>% 
  filter(id == "27358") %>%
  select(description, n.exposed, N)

ndeaths_noheard = azad %>% 
  filter(description == "Neonatal deaths among non-group members") %>%
  select(n.exposed) %>%
  as.numeric()

ndeaths_heard = azad %>% 
  filter(description == "Neonatal deaths among women who had heard of groups") %>%
  select(n.exposed) %>%
  as.numeric()

ndeaths_control = azad %>% 
  filter(description == "Neonatal mortality rate in control cluster") %>%
  select(n.exposed) %>%
  as.numeric()

lbirths_noheard = azad %>% 
  filter(description == "Neonatal deaths among non-group members") %>%
  select(N) %>%
  as.numeric()

lbirths_heard = azad %>% 
  filter(description == "Neonatal deaths among women who had heard of groups") %>%
  select(N) %>%
  as.numeric()

lbirths_control = azad %>% 
  filter(description == "Neonatal mortality rate in control cluster") %>%
  select(N) %>%
  as.numeric()

nmr_control = ndeaths_control / lbirths_control
nmr_heard = ndeaths_heard / lbirths_heard
nmr_noheard = ndeaths_noheard / lbirths_noheard

# rd heard of
rd_heard = nmr_heard - nmr_control

# rd had not heard of
rd_noheard = nmr_noheard - nmr_control

table3_azad = data.frame(
  authyr = rep("Azad et al., 2010",2),
  parameter = rep("Cluster-spillover effect conditional on exposure to treatment",2),
  int1= "Women's groups and health service strengthening",
  parameter.scale = rep("",2),
  units.outcome = c("Neonatal mortality among women exposed to women's groups who did not participate in them but who heard of them", 
                    "Neonatal mortality among women exposed to women's groups who did not participate in them but who had not heard of them"),
  pt.est = c(rd_heard, rd_noheard),
  ci.lower = c(NA, NA),
  ci.upper = c(NA, NA)
)

# Singh, 2011   
table3_singh = save_results(
  data = data,
  myid = "26115",
  param = "Cluster-level spillover effect",
  desc = "Weight for age z-score change, diff-in-diff,spillover effect"
)

# Bjorkman and Svensson, 2009   RERUN and check other param name
table3_bjorkman = save_results(
  data = data,
  myid = "30528-184",
  param = "Overall effect conditional on distance",
  desc = c("Difference in mean outpatients per month in treatment vs. control clusters adjusting for control clinic within 10km of treatment clinic",
           "Difference in mean deliveries per month in treatment vs. control clusters adjusting for control clinic within 10km of treatment clinic")
)

# Print table 3
table3 = bind_rows(table3_avitabile,
                   table3_handa,
                   table3_ribas,
                   table3_baird,
                   table3_contreras,
                   table3_bhattacharya,
                   table3_banerjee,
                   table3_tontarawongsa1, 
                   table3_tontarawongsa2,
                   table3_tontarawongsa3,
                   table3_godlonton,
                   table3_chong1,
                   table3_chong2,
                   table3_german,
                   table3_kremer,
                   table3_janssens1,
                   table3_janssens2,
                   table3_dupas,
                   table3_azad,
                   table3_singh,
                   table3_bjorkman)

table3 = table3 %>% select(-c(cluster_size, coverage))

table3


write.csv(table3, file = paste0(tab_dir, "spsr-table-3.csv"), row.names=FALSE)



