####################################
# spsr-supp-table-rob.R

# spillover systematic review
# table of risk of bias
# Supplement 6

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
####################################
rm(list=ls())

source(paste0(here::here(), "/0-config.R"))

rob_overall = read.csv(paste0(data_dir, "spillover-sr-rob-overall.csv"))
rob = read.csv(paste0(data_dir, "spillover-sr-rob-detailed.csv"))
master = read.csv(paste0(data_dir,"spillover-sr-extracted-data.csv"))

#------------------------------------
# Table: risk of bias
#------------------------------------
# reorder rows
rob_rows = c(
  # labels
  1, 
  
  # internal validity
  grep("iv",rob[,1]),
  
  # external validity
  grep("ev",rob[,1]),
  
  # construct validity
  grep("cv",rob[,1]),
  
  # other
  grep("-o-",rob[,1]),
  
  # regression analysis only
  grep("rob-r-",rob[,1])
)

# drop study-specific columns
rob_columns = c(
  # Labels
  2,3,
  
  # Percentages
  grep("% yes", t(as.vector(rob[1,]))),
  grep("% uncertain", t(as.vector(rob[1,]))),
  grep("%  no", t(as.vector(rob[1,]))),
  
  # Number of studies
  grep("no.", t(as.vector(rob[1,])))
)

rob_table = rob[rob_rows, rob_columns]

write.csv(rob_table, file = paste0(tab_dir, "spsr-supp6-table1.csv"), row.names=FALSE)


#---------------------------------------------------
# Overall risk of bias table
#---------------------------------------------------
# subset master data to publication year and id
master = master %>% select(id, pub.year) %>% filter(!is.na(pub.year))
master = master[!duplicated(master),]

# merge publication year onto risk of bias table
rob_ov_table=merge(rob_overall,master,by="id")  

# subset columns and sort by author and year
rob_ov_table = rob_ov_table %>% 
  mutate(authyr = paste0(authors,", ",pub.year)) %>%
  select(authyr, quality.rating) %>%
  arrange(authyr)

rob_ov_table$authyr[rob_ov_table$authyr=="Chaudhuri, NA"]="Chaudhuri (Year not listed)"
rob_ov_table$authyr[rob_ov_table$authyr=="Singh, NA"]="Singh (Year not listed)"

write.csv(rob_ov_table, file = paste0(tab_dir, "spsr-supp6-table2.csv"), row.names=FALSE)


