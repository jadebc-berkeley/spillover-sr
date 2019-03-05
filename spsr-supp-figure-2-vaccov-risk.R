####################################
# spsr-supp-figure-2-vaccov-risk.R

# spillover systematic review
# figure showing vaccination coverage
# by cholera incidence

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
####################################
rm(list=ls())

source(paste0(here::here(), "/0-config.R"))

# load data
merged = read.csv(paste0(data_dir,"spillover-sr-extracted-data.csv"))

###################################################
#table: Association between vaccine coverage and risk of cholera 
#5513: Table 2; outcome is risk per 1,000 persons; compare vaccine and placebo recipients

#5514: Table 1; outcome is risk per 1,000 persons; compare vaccine and placebo recipients

#5572: Table 1; outcome is risk per 1,000 persons; compare vaccine and placebo recipients

#317: Table 3; outcome is "incidence rate" per 1,000 persons but it is also referred to ask 
#      "risk" in the table; compare no-dose and two-dose recipients

#24229-3: Table S5; risk of pneumonia per 1,000 persons; compare vaccine and placebo recipients

#80-Huq: similar data but cannot be synthesized because of how it was reported
###################################################
#subset datasets

# 5513 
tabvcrisk.5513 = merged %>%
  filter(id == 5513 &
         table == 2 & 
         ref.pt.est.desc == "Cumulative incidence") %>%
  select(description, id, authyr, pt.est, cholera_vacc_coverage_lower, cholera_vacc_coverage_upper) %>%
  mutate(cov_mid = (cholera_vacc_coverage_upper + cholera_vacc_coverage_lower)/2) 

tabvcrisk.5513$vacc[grep("vaccinee",tabvcrisk.5513$description)] = 1
tabvcrisk.5513$vacc[grep("placebo",tabvcrisk.5513$description)] = 0

# 5514
tabvcrisk.5514 = merged %>%
  filter(id == 5514 &
         table == 1 & 
         (column=="3 to 5" | column=="6 to 8") &
         ref.pt.est.desc == "Cumulative incidence") %>%
  select(description, id, authyr, pt.est, cholera_vacc_coverage_lower, cholera_vacc_coverage_upper) %>%
  mutate(cov_mid = (cholera_vacc_coverage_upper + cholera_vacc_coverage_lower)/2)

tabvcrisk.5514$vacc[grep("(vaccine)",tabvcrisk.5514$description)] = 1
tabvcrisk.5514$vacc[grep("(placebo)",tabvcrisk.5514$description)] = 0

# 29875
tabvcrisk.29875 = merged %>%
  filter(id == 29875 &
           table == 1 & 
           ref.pt.est.desc == "Cumulative incidence") %>%
  select(description, id, authyr, pt.est, cholera_vacc_coverage_lower, cholera_vacc_coverage_upper) %>%
  mutate(cov_mid = (cholera_vacc_coverage_upper + cholera_vacc_coverage_lower)/2) %>%
  mutate(vacc = 0)

# 5572 
tabvcrisk.5572 = merged %>%
  filter(id == 5572 &
           table == 1 & 
           ref.pt.est.desc == "Cumulative incidence") %>%
  select(description, id, authyr, pt.est, cholera_vacc_coverage_lower, cholera_vacc_coverage_upper) %>%
  filter(!is.na(cholera_vacc_coverage_lower)) %>%
  mutate(cov_mid = (cholera_vacc_coverage_upper + cholera_vacc_coverage_lower)/2)

tabvcrisk.5572$vacc[grep(", vaccine",tabvcrisk.5572$description)] = 1
tabvcrisk.5572$vacc[grep(", placebo",tabvcrisk.5572$description)] = 0

# 317
tabvcrisk.317 = merged %>%
  filter(id == 317 &
           table == 3 & 
           ref.pt.est.desc == "Incidence rate") %>%
  select(description, id, authyr, pt.est, cholera_vacc_coverage_lower, cholera_vacc_coverage_upper) %>%
  mutate(cov_mid = (cholera_vacc_coverage_upper + cholera_vacc_coverage_lower)/2)

tabvcrisk.317$vacc[grep("no-dose",tabvcrisk.317$description)] = 0
tabvcrisk.317$vacc[grep("two-dose",tabvcrisk.317$description)] = 1

# 24229-3
tabvcrisk.24229.3 = merged %>%
  filter(id == "24229-3" &
           table == "S5" & 
           ref.pt.est.desc == "Incidence rate" &
           units.outcome=="radiologic pneumonia") %>%
  select(description, id, authyr, pt.est, cholera_vacc_coverage_lower, cholera_vacc_coverage_upper) %>%
  mutate(cov_mid = (cholera_vacc_coverage_upper + cholera_vacc_coverage_lower)/2)

tabvcrisk.24229.3$vacc[grep(", Vaccine",tabvcrisk.24229.3$description)] = 1
tabvcrisk.24229.3$vacc[grep(", Placebo",tabvcrisk.24229.3$description)] = 0

#data formatting for plotting
tabvcrisk = bind_rows(tabvcrisk.5513,
                      tabvcrisk.5514,
                      tabvcrisk.5572,
                      tabvcrisk.317,
                      tabvcrisk.29875)

tabvcrisk = tabvcrisk %>%
  mutate(vacc.f = factor(case_when(
    vacc == 0 ~ "A. Unvaccinated",
    vacc == 1 ~ "B. Vaccinated"
  ))) %>%
  select(id, authyr, pt.est, cov_mid, vacc.f)

# ------------------------------------------
# create plot
# ------------------------------------------
#color blind palette
mypalette <- c("#737373", "#cc8b00",  "#009E73",  "#0072B2", "#CC79A7")
myshape <- c(4,17,18,15,16)

covplot = ggplot(tabvcrisk,
       aes(x = cov_mid,
           y = pt.est,
           group = interaction(authyr),
           color = authyr)) +
  geom_line() + 
  theme_complete_bw() + 
  ylab("Cholera Risk per 1,000 People") +
  xlab("% Cholera vaccine coverage") + 
  facet_wrap( ~ vacc.f) + 
  geom_point(aes(shape = authyr, size = authyr)) +
  scale_color_manual(name = "", values = mypalette) +
  scale_shape_manual(name = "", values = myshape) +
  scale_size_manual(name = "", values = c(2, 2, 2.5, 1.5, 2))

ggsave(
  plot = covplot,
  height = 4,
  width = 8,
  dpi = 200,
  filename = paste0(fig_dir, "spsr-figure-supp7-figure2.pdf"),
  useDingbats = FALSE
)

