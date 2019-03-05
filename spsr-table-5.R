####################################
# spsr-table-5-terms.R

# spillover systematic review
# Table 5 - search terms

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
####################################
rm(list=ls())

source(paste0(here::here(), "/0-config.R"))

# load data
terms = read.csv(paste0(data_dir,"spillover-sr-terms.csv"))
master = read.csv(paste0(data_dir,"spillover-sr-extracted-data.csv"))

master = master %>% select(id, field)
master = master[!duplicated(master),] %>%
  filter(!is.na(field))
  
#------------------------------------
# Table: terms used
#------------------------------------
# merge in fields
table.terms.data = full_join(terms, master, by="id")

table.terms = table.terms.data %>%
  group_by(field) %>%
  summarise_at(vars(contagion:seconda), sum, na.rm = TRUE)

table.terms.print = as.data.frame(t(table.terms %>% select(-field)))
colnames(table.terms.print) = names(table(table.terms$field))

table.terms.print = table.terms.print %>%
  mutate(label = rownames(table.terms.print)) %>%
  mutate(Total = Economics + Geography + `Public Health`) %>%
  arrange(-Total) %>%
  
  mutate(lab = case_when(
    label == "indirect.effect" ~ "Indirect effect*",
    label == "spillover" ~ "Spillover*",
    label == "externalit" ~ "Externalit*",
    label == "seconda" ~ "Seconda*",
    label == "indirect.protection" ~ "Indirect protection",
    label == "herd.protect" ~ "Herd protect*",
    label == "diffusion" ~ "Diffusion",
    label == "herd.effect" ~ "Herd effect*",
    label == "herd.immunity" ~ "Herd immunity",
    label == "peer.effect" ~ "Peer effect*",
    label == "unexpected" ~ "Unexpected",
    label == "indirect.protective" ~ "Indirect protective",
    label == "interference" ~ "Interference",
    label == "contagion" ~ "Contagion",
    label == "unexpected.benefit" ~ "Unexpected benefit*"
  )) %>%
  select(lab, Economics:`Public Health`, Total)

write.csv(table.terms.print, file = paste0(tab_dir, "spsr-table-5.csv"), row.names=FALSE)

