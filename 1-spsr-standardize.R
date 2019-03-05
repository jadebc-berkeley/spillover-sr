####################################
# 1-spsr-standardize.R

# spillover systematic review
# convert estimates to the same scale:
# (1 - RR) x 100

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
####################################
rm(list=ls())

source(paste0(here::here(), "/0-config.R"))

# load data
merged = read.csv(paste0(data_dir,"spillover-sr-extracted-data.csv"))
txcov = read.csv(paste0(data_dir,"spillover-sr-treatment-coverage.csv"))

###################################################
### cluster-level spillover 

# excluding 31157 and 30528-548 because mean in controls
# was not reported, so data is not comparable
###################################################
#subset to relevant columns
ws_parameter_list = c("Total effect", "Direct effect", "Cluster-level spillover effect")

ws.df = merged[merged$parameter %in% ws_parameter_list,]

# 2147 Sur -----------------
ws.2147=ws.df[ws.df$id=="2147" & ws.df$adjusted==1,]
ws.2147 = ws.2147[, c("id", "authyr", "int1", "outcome.primary",
                     "parameter", "pt.est", "p.value",
                     "ci.lower", "ci.upper", "SE")]
colnames(ws.2147)[6] = "pt.est.rr"
ws.2147$p.value = as.numeric(as.character(ws.2147$p.value))

# 2147-16 Khan -----------------
ws.2147.16=ws.df[ws.df$id=="2147-16",]
ws.2147.16=ws.2147.16[ws.2147.16$parameter=="Cluster-level spillover effect" |
                        ws.2147.16$parameter=="Total effect",]
ws.2147.16 = ws.2147.16[, c("id", "authyr", "int1", "outcome.primary",
                      "parameter", "pt.est", "p.value",
                      "ci.lower", "ci.upper", "SE")]
colnames(ws.2147.16)[6] = "pt.est.rr"
ws.2147.16$p.value = as.numeric(as.character(ws.2147.16$p.value))

# 5513 Ali -----------------
ws.5513=ws.df[ws.df$id=="5513" & ws.df$adjusted==1,]
ws.5513 = ws.5513[, c("id", "authyr", "int1", "outcome.primary",
                      "parameter", "pt.est", "p.value",
                      "ci.lower", "ci.upper", "SE")]
colnames(ws.5513)[6] = "pt.est.rr"
ws.5513$p.value = as.numeric(as.character(ws.5513$p.value))

# 26115 Singh----------------- 
# only keeping binary outcome so we can standardize measures
ws.26115=ws.df[ws.df$id==26115,]
ws.26115=ws.26115[grep("Probability",ws.26115$description,ignore.case=TRUE),]

#converting point estimates to 1-RR
mn.pr.control=as.numeric(merged$pt.est[merged$id==26115 & 
       merged$description=="Mean probability of underweight (z-score<-2) among controls"])

ws.26115$pt.est[grep("<-2, diff",ws.26115$description)]=
  -(as.numeric(ws.26115$pt.est[grep("<-2, diff",ws.26115$description)]) /
      mn.pr.control*100)

#removing control mean row
ws.26115=ws.26115[-grep("Mean",ws.26115$description),]

ws.26115 = ws.26115[, c("id", "authyr", "int1", "outcome.primary",
                        "parameter", "pt.est", "p.value",
                        "ci.lower", "ci.upper", "SE")]
colnames(ws.26115)[6] = "pt.est.rr"
ws.26115$p.value = as.numeric(as.character(ws.26115$p.value))

###################################################
### cluster-level spillovers conditional on eligibility 

# excluding 31409, 31503 because continuous outcome
# excluding 30528-754, 30528 because no control mean is available
###################################################
#subset to relevant columns
wse_parameter_list = c("Total effect among eligibles", "Direct effect among eligibles",
                   "Cluster-level spillover effect among ineligibles")

wse.df = merged[merged$parameter %in% wse_parameter_list,]

# 30528-617 -----------------
# exclude continuous outcomes (child growth); only include morbidity
wse.30528.617=wse.df[wse.df$id=="30528-617",]
drops=grep("Z-score",wse.30528.617$description)
wse.30528.617=wse.30528.617[-drops,]

#comparing to total effect for >6 months
# rows for ineligibles <=6 months
keep.30528.617.1=grep("ineligible",wse.30528.617$description)
# rows for eligibles >6 months
keep.30528.617.2=grep(">6",wse.30528.617$description)

wse.30528.617=wse.30528.617[c(keep.30528.617.1,keep.30528.617.2),]

# save mean outcome among the eligible in the control group
te.mn.diarr=as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Mean cases of diarrhea among control (>6 months); eligible"])
te.mn.vomit=as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Mean cases of vomiting among control (>6 months); eligible"])
te.mn.fast=as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Mean cases of fast breathing among control (>6 months); eligible"])
te.mn.fever=as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Mean cases of fever among control (>6 months); eligible"])
te.mn.chills=as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Mean cases of chills among control (>6 months); eligible"])

# save mean outcome among the ineligible in the control group
sp.mn.diarr=as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Mean cases of diarrhea among control; ineligible"])
sp.mn.vomit=as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Mean cases of vomiting among control; ineligible"])
sp.mn.fast=as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Mean cases of fast breathing among control; ineligible"])
sp.mn.fever=as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Mean cases of fever among control; ineligible"])
sp.mn.chills=as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Mean cases of chills among control; ineligible"])

# convert mean difference to 1 - RR by dividing by control mean
wse.30528.617$pt.est.rr=NA
wse.30528.617$pt.est.rr[wse.30528.617$description=="Difference in diarrhea among those in treated vs. untreated clusters (>6 months); eligible"]=
  as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Difference in diarrhea among those in treated vs. untreated clusters (>6 months); eligible"])/te.mn.diarr*100
wse.30528.617$pt.est.rr[wse.30528.617$description=="Difference in vomiting among those in treated vs. untreated clusters (>6 months); eligible"]=
  as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Difference in vomiting among those in treated vs. untreated clusters (>6 months); eligible"])/te.mn.vomit*100
wse.30528.617$pt.est.rr[wse.30528.617$description=="Difference in fast breathing among those in treated vs. untreated clusters (>6 months); eligible"]=
  as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Difference in fast breathing among those in treated vs. untreated clusters (>6 months); eligible"])/te.mn.fast*100
wse.30528.617$pt.est.rr[wse.30528.617$description=="Difference in fever among those in treated vs. untreated clusters (>6 months); eligible"]=
  as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Difference in fever among those in treated vs. untreated clusters (>6 months); eligible"])/te.mn.fever*100
wse.30528.617$pt.est.rr[wse.30528.617$description=="Difference in chills among those in treated vs. untreated clusters (>6 months); eligible"]=
  as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Difference in chills among those in treated vs. untreated clusters (>6 months); eligible"])/te.mn.chills*100

wse.30528.617$pt.est.rr[wse.30528.617$description=="Difference in diarrhea among those in treated vs. untreated clusters; ineligible"]=
  as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Difference in diarrhea among those in treated vs. untreated clusters; ineligible"])/sp.mn.diarr*100
wse.30528.617$pt.est.rr[wse.30528.617$description=="Difference in vomiting among those in treated vs. untreated clusters; ineligible"]=
  as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Difference in vomiting among those in treated vs. untreated clusters; ineligible"])/sp.mn.vomit*100
wse.30528.617$pt.est.rr[wse.30528.617$description=="Difference in fast breathing among those in treated vs. untreated clusters; ineligible"]=
  as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Difference in fast breathing among those in treated vs. untreated clusters; ineligible"])/sp.mn.fast*100
wse.30528.617$pt.est.rr[wse.30528.617$description=="Difference in fever among those in treated vs. untreated clusters; ineligible"]=
  as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Difference in fever among those in treated vs. untreated clusters; ineligible"])/sp.mn.fever*100
wse.30528.617$pt.est.rr[wse.30528.617$description=="Difference in chills among those in treated vs. untreated clusters; ineligible"]=
  as.numeric(wse.30528.617$pt.est[wse.30528.617$description=="Difference in chills among those in treated vs. untreated clusters; ineligible"])/sp.mn.chills*100

# drop control means
drops=grep("Mean",wse.30528.617$description)
wse.30528.617=wse.30528.617[-drops,]

# calculate Z score based on risk difference
wse.30528.617$z = wse.30528.617$pt.est / wse.30528.617$SE

# calculate p-value for 1-RR
wse.30528.617$p.value = 2*pnorm(-abs(wse.30528.617$z))

# subset to relevant columns
wse.30528.617=wse.30528.617[,c("id","authyr","int1","outcome.primary",
                               "parameter","pt.est.rr","p.value","ci.lower","ci.upper","SE")]

# add outcome labels
wse.30528.617$outcome.primary=rep(c("Diarrhea","Vomiting","Fast breathing","Fever","Chills"),2)



# 11528 -----------------
wse.11528=wse.df[wse.df$id=="11528",]

# save endline control group means
mn.c.elig.11528=as.numeric(wse.11528$pt.est[wse.11528$description==
    "Mean prevalence of trachoma among eligibles in the control group at endline"])
mn.c.inelig.11528=as.numeric(wse.11528$pt.est[wse.11528$description==
    "Mean prevalence of trachoma among ineligibles in the control group at endline"])

# calculate total effect and spillover effect among ineligibles 1-RR
te.11528=(mn.c.elig.11528-as.numeric(wse.11528$pt.est[wse.11528$description==
    "Mean prevalence of trachoma among treated in the treated group at 12 months"]))/mn.c.elig.11528*100
sp.11528=(mn.c.inelig.11528-as.numeric(wse.11528$pt.est[wse.11528$description==
    "Mean prevalence of trachoma among untreated in the treated group at 12 months"]))/mn.c.inelig.11528*100

# calculate total effect and spillover effect among ineligibles RR
te.rr.11528=as.numeric(wse.11528$pt.est[wse.11528$description==
    "Mean prevalence of trachoma among treated in the treated group at 12 months"])/mn.c.elig.11528
sp.rr.11528=as.numeric(wse.11528$pt.est[wse.11528$description==
    "Mean prevalence of trachoma among untreated in the treated group at 12 months"])/mn.c.inelig.11528

# save N
n0.e.11528=as.numeric(wse.11528$N.unexposed[wse.11528$description==
     "Mean prevalence of trachoma among eligibles in the control group at endline"])
n0.i.11528=as.numeric(wse.11528$N.unexposed[wse.11528$description==
     "Mean prevalence of trachoma among ineligibles in the control group at endline"])
n1.e.11528=as.numeric(wse.11528$N.exposed[wse.11528$description==
     "Mean prevalence of trachoma among treated in the treated group at 12 months"])
n1.i.11528=as.numeric(wse.11528$N.exposed[wse.11528$description==
     "Mean prevalence of trachoma among untreated in the treated group at 12 months"])

# save prevalence in control and treated groups
p0.e.11528=as.numeric(wse.11528$p0[wse.11528$description==
     "Mean prevalence of trachoma among eligibles in the control group at endline"])
p0.i.11528=as.numeric(wse.11528$p0[wse.11528$description==
     "Mean prevalence of trachoma among ineligibles in the control group at endline"])
p1.e.11528=as.numeric(wse.11528$p1[wse.11528$description==
     "Mean prevalence of trachoma among treated in the treated group at 12 months"])
p1.i.11528=as.numeric(wse.11528$p1[wse.11528$description==
     "Mean prevalence of trachoma among untreated in the treated group at 12 months"])

#calculate se(log(RR))
se.te.11528=sqrt(((1-p1.e.11528)/(n1.e.11528*p1.e.11528))+((1-p0.e.11528)/(n0.e.11528*p0.e.11528)))
se.sp.11528=sqrt(((1-p1.i.11528)/(n1.i.11528*p1.i.11528))+((1-p0.i.11528)/(n0.i.11528*p0.i.11528)))

#95% CIs for 1-RR
ci.u.te.11528=(1-exp(te.rr.11528-(qnorm(0.975)*se.te.11528)))*100
ci.l.te.11528=(1-exp(te.rr.11528+(qnorm(0.975)*se.te.11528)))*100
ci.u.sp.11528=(1-exp(log(sp.rr.11528)-(qnorm(0.975)*se.sp.11528)))*100
ci.l.sp.11528=(1-exp(log(sp.rr.11528)+(qnorm(0.975)*se.sp.11528)))*100

wse.11528=data.frame(id=rep("11528",2),
                     authyr=rep("House et al., 2009",2),
                     int1=rep("Mass azithromycin distribution",2),
                     outcome.primary=rep("Trachoma",2),
                     parameter=c("Total effect among eligibles","Cluster-level spillover effect among ineligibles"),
                     pt.est.rr=c(te.11528,sp.11528),
                     p.value=NA,
                     ci.lower=c(ci.l.te.11528,ci.l.sp.11528),
                     ci.upper=c(ci.u.te.11528,ci.u.sp.11528),
                     SE=c(NA,NA))

# 15137-8 Egere -----------------
# Convert to 1-RR % using Table 2
wse.15137.8=wse.df[wse.df$id=="15137-8",]
wse.15137.8=subset(wse.15137.8,wse.15137.8$parameter.scale=="RD")

# save N
n0.i.vt.15137.8=as.numeric(wse.15137.8$N.unexposed[wse.15137.8$description==
   "Risk difference for vaccine type pneumococcal carriage in vaccinated vs. control group"])
n1.i.vt.15137.8=as.numeric(wse.15137.8$N.exposed[wse.15137.8$description==
   "Risk difference for vaccine type pneumococcal carriage in vaccinated vs. control group"])
n0.i.nvt.15137.8=as.numeric(wse.15137.8$N.unexposed[wse.15137.8$description==
   "Risk difference for non-vaccine type pneumococcal carriage in vaccinated vs. control group"])
n1.i.nvt.15137.8=as.numeric(wse.15137.8$N.exposed[wse.15137.8$description==
   "Risk difference for non-vaccine type pneumococcal carriage in vaccinated vs. control group"])
n0.i.at.15137.8=as.numeric(wse.15137.8$N.unexposed[wse.15137.8$description==
   "Risk difference for any pneumococcus in vaccinated vs. control group"])
n1.i.at.15137.8=as.numeric(wse.15137.8$N.exposed[wse.15137.8$description==
   "Risk difference for any pneumococcus in vaccinated vs. control group"])

# save risk in control and treated
p0.i.vt.15137.8=as.numeric(wse.15137.8$p0[wse.15137.8$description==
   "Risk difference for vaccine type pneumococcal carriage in vaccinated vs. control group"])
p1.i.vt.15137.8=as.numeric(wse.15137.8$p1[wse.15137.8$description==
   "Risk difference for vaccine type pneumococcal carriage in vaccinated vs. control group"])
p0.i.nvt.15137.8=as.numeric(wse.15137.8$p0[wse.15137.8$description==
   "Risk difference for non-vaccine type pneumococcal carriage in vaccinated vs. control group"])
p1.i.nvt.15137.8=as.numeric(wse.15137.8$p1[wse.15137.8$description==
   "Risk difference for non-vaccine type pneumococcal carriage in vaccinated vs. control group"])
p0.i.at.15137.8=as.numeric(wse.15137.8$p0[wse.15137.8$description==
   "Risk difference for any pneumococcus in vaccinated vs. control group"])
p1.i.at.15137.8=as.numeric(wse.15137.8$p1[wse.15137.8$description==
   "Risk difference for any pneumococcus in vaccinated vs. control group"])

# calculate spillover effect among ineligible RR
sp.rr.vt.15137.8=p1.i.vt.15137.8/p0.i.vt.15137.8
sp.rr.nvt.15137.8=p1.i.nvt.15137.8/p0.i.nvt.15137.8
sp.rr.at.15137.8=p1.i.at.15137.8/p0.i.at.15137.8

# calculate spillover effect among ineligible 1 - RR
sp.1.rr.vt.15137.8=(1-(p1.i.vt.15137.8/p0.i.vt.15137.8))*100
sp.1.rr.nvt.15137.8=(1-(p1.i.nvt.15137.8/p0.i.nvt.15137.8))*100
sp.1.rr.at.15137.8=(1-(p1.i.at.15137.8/p0.i.at.15137.8))*100

# calculate se(log(RR))
se.sp.vt.15137.8 = sqrt(((1 - p1.i.vt.15137.8) / (n1.i.vt.15137.8 * p1.i.vt.15137.8)) +
                          ((1 - p0.i.vt.15137.8) / (n0.i.vt.15137.8 * p0.i.vt.15137.8)))
se.sp.nvt.15137.8 = sqrt(((1 - p1.i.nvt.15137.8) / (n1.i.nvt.15137.8 * p1.i.nvt.15137.8)) +
                           ((1 - p0.i.nvt.15137.8) / (n0.i.nvt.15137.8 * p0.i.nvt.15137.8)))
se.sp.at.15137.8 = sqrt(((1 - p1.i.at.15137.8) / (n1.i.at.15137.8 * p1.i.at.15137.8)) +
                          ((1 - p0.i.at.15137.8) / (n0.i.at.15137.8 * p0.i.at.15137.8)))

# calculate 95% CIs for 1-RR
ci.u.sp.vt.15137.8=(1-exp(log(sp.rr.vt.15137.8)-(qnorm(0.975)*se.sp.vt.15137.8)))*100
ci.l.sp.vt.15137.8=(1-exp(log(sp.rr.vt.15137.8)+(qnorm(0.975)*se.sp.vt.15137.8)))*100
ci.u.sp.nvt.15137.8=(1-exp(log(sp.rr.nvt.15137.8)-(qnorm(0.975)*se.sp.nvt.15137.8)))*100
ci.l.sp.nvt.15137.8=(1-exp(log(sp.rr.nvt.15137.8)+(qnorm(0.975)*se.sp.nvt.15137.8)))*100
ci.u.sp.at.15137.8=(1-exp(log(sp.rr.at.15137.8)-(qnorm(0.975)*se.sp.at.15137.8)))*100
ci.l.sp.at.15137.8=(1-exp(log(sp.rr.at.15137.8)+(qnorm(0.975)*se.sp.at.15137.8)))*100

# excluding non-vaccine type because it is expected to be in the other direction
# and also any pneumoccous because it pools across vaccine and non vaccine type
wse.15137.8=data.frame(id=rep("15137-8",1),
                       authyr=rep("Egere et al., 2012",1),
                       int1=rep("Pneumococcal conjugate vaccine",1),
                       outcome.primary=c("Any vaccine-type pneumoccocus"),
                       parameter=rep("Cluster-level spillover effect among ineligibles",1),
                       pt.est.rr=c(sp.1.rr.vt.15137.8),
                       p.value=NA,
                       ci.lower=c(ci.l.sp.vt.15137.8),
                       ci.upper=c(ci.u.sp.vt.15137.8), 
                       SE=c(se.sp.vt.15137.8))

###################################################
# bind together papers' results
###################################################
ws.tab=rbind(ws.2147,ws.2147.16,ws.5513, ws.26115,
             wse.30528.617,wse.11528,wse.15137.8)

# merge in cluster-level tx coverage
txcov = txcov %>% select(id, cov) %>%
  filter(id %in% c("5513","26115","2147","2147-16","11528","30528-617","15137-8"))

ws.tab=merge(ws.tab,txcov,by="id")

# recode as numeric
ws.tab$pt.est=as.numeric(ws.tab$pt.est)
ws.tab$SE=as.numeric(ws.tab$SE)
ws.tab$ci.lower=as.numeric(ws.tab$ci.lower)
ws.tab$ci.upper=as.numeric(ws.tab$ci.upper)
ws.tab$p.value=as.numeric(ws.tab$p.value)

# indicator for statistical significance
ws.tab$signif = NA
ws.tab$signif[!is.na(ws.tab$p.value) &
                ws.tab$p.value != "" & 
                ws.tab$p.value < 0.05] = "Yes"
ws.tab$signif[!is.na(ws.tab$p.value) &
                ws.tab$p.value != "" & 
                ws.tab$p.value >= 0.05] = "No"
ws.tab$signif[is.na(ws.tab$signif) &
                !is.na(ws.tab$ci.lower) & !is.na(ws.tab$ci.upper) &
                ws.tab$ci.lower <= 0 & ws.tab$ci.upper >= 0] = "No"
ws.tab$signif[is.na(ws.tab$signif) &
                !is.na(ws.tab$ci.lower) & !is.na(ws.tab$ci.upper) &
                ((ws.tab$ci.lower > 0 & ws.tab$ci.upper > 0) |
                (ws.tab$ci.lower < 0 & ws.tab$ci.upper < 0))] = "Yes"

# manually assigning Singh 26115 as not statistically significant
# because not significant on additive scale
ws.tab$signif[ws.tab$id==26115]="No"
ws.tab$signif=factor(ws.tab$signif,levels=c("Yes","No"))

saveRDS(ws.tab,file=paste0(data_dir, "wsplot.RDS"))
