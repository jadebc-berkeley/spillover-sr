####################################
# spsr-figure-3-funnelplot.R

# spillover systematic review
# funnel plot figures
# for main text and supplement

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
####################################
rm(list=ls())

source(paste0(here::here(), "/0-config.R"))
  
# load data
data = read.csv(paste0(data_dir,"spillover-sr-extracted-data.csv"))

#-------------------------------------
# RD funnel plot - spillover
#-------------------------------------
# list of study ids that estimated risk differences
rd.list=list("30528-137","30528-980","31031","30528-184","31157","30971-3",
             "30528-196","30528-617","3272","30528-179","31409","16384",
             "30528","30528-167","2647-1","26115","30528-736","30528-469",
             "30971","30528-670","30528-554","30528-548")

# subset to relevant columns
rd.df = data[data$id %in% rd.list, c(
  "id",
  "authors",
  "parameter",
  "continuous.outcome",
  "parameter.scale",
  "description",
  "pt.est",
  "SE",
  "ci.upper",
  "outcome.direction",
  "pt.est.desc",
  "sp.signif.param",
  "regression.type",
  "t.score"
)]

# subset to spillover effects
rd.df = rd.df[rd.df$parameter != "Total effect", ]
rd.df = rd.df[rd.df$parameter != "Total effect among eligibles", ]
rd.df = rd.df[rd.df$parameter != "Direct effect", ]
rd.df = rd.df[rd.df$parameter != "", ]

# drop point estimates that are not parameter estimates
rd.df = rd.df[rd.df$pt.est.desc != "mean", ]
rd.df = rd.df[rd.df$pt.est.desc != "Mean", ]
rd.df = rd.df[rd.df$pt.est.desc != "percentage", ]

rd.df$pt.est = as.numeric(rd.df$pt.est)

# only keep Godlonton & Thornton estimates from IV analysis
drop = which(rd.df$regression.type == "OLS" & rd.df$id == "30528-179")
rd.df = rd.df[-drop, ]

# dropping "never used condom" category from Dupas paper because
# it's not clear which direction the outcome is in
drops = grep("never used a condom", rd.df$description)
rd.df = rd.df[-drops, ]

# drop estimates with continuous outcomes
# While we could calculate a standardized mean difference for continuous
# outcomes (mean diff / SE), they may be correlated with SEs, producing spurious asymmetry.
# http://handbook.cochrane.org/chapter_10/10_4_1_funnel_plots.htm
drops = which(rd.df$continuous.outcome == 1)
rd.df = rd.df[-drops, ]

# drop rows with indicator significant=0 because they 
# should not be included (e.g. control mean)
rd.df=rd.df[rd.df$sp.signif.param==1,]

# manually calculate SE from CI
rd.df$SE[rd.df$id == "31031"& !is.na(rd.df$ci.upper)] = (rd.df$ci.upper[rd.df$id == "31031"  & !is.na(rd.df$ci.upper)] - 
        rd.df$pt.est[rd.df$id == "31031"  & !is.na(rd.df$pt.est)]) / qnorm(0.975)
rd.df$SE[rd.df$id == "31031-4-r" & !is.na(rd.df$SE)] = (rd.df$ci.upper[rd.df$id == "31031"  & !is.na(rd.df$ci.upper)] -
         rd.df$pt.est[rd.df$id == "31031"  & !is.na(rd.df$pt.est)]) / qnorm(0.975)

# manually calculate SE from t-statistic
rd.df$SE[rd.df$id == "30528-670" & !is.na(rd.df$pt.est)] = abs(rd.df$pt.est[rd.df$id == "30528-670"  & !is.na(rd.df$pt.est)] /
        rd.df$t.score[rd.df$id == "30528-670" & !is.na(rd.df$t.score)])
rd.df$SE[rd.df$id == "30528-554" & !is.na(rd.df$pt.est)] = abs(rd.df$pt.est[rd.df$id == "30528-554" & !is.na(rd.df$pt.est)] /
        rd.df$t.score[rd.df$id == "30528-554" & !is.na(rd.df$t.score)])

# reverse point estimate if desired outcome is positive
rd.df$pt.est[rd.df$outcome.direction=="positive" & !is.na(rd.df$pt.est)] = 
  -rd.df$pt.est[rd.df$outcome.direction=="positive" & !is.na(rd.df$pt.est)]

rd.df=rd.df[!is.na(rd.df$pt.est),]

funnel_rd_spill = ggplot(rd.df, aes(x = pt.est, y = SE)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  scale_x_continuous(
    limits = c(-1, 1),
    breaks = c(-1, -.75, -.5, -.25, 0, .25, .5, .75, 1),
    labels = c(-1, -.75, -.5, -.25, 0, .25, .5, .75, 1)
  ) +
  theme_complete_bw() +
  xlab("Risk difference") +
  ylab("SE(Risk difference)") +
  scale_y_continuous(limits = c(0, 0.7))

ggsave(
  plot = funnel_rd_spill,
  height = 5,
  width = 8,
  dpi = 200,
  filename = paste0(fig_dir, "spsr-figure-3a.pdf"),
  useDingbats = FALSE
)

#-------------------------------------
# RD funnel plot - total/direct effects
#-------------------------------------
rd.te.df = data[data$id %in% rd.list, c(
  "id",
  "authors",
  "parameter",
  "continuous.outcome",
  "parameter.scale",
  "description",
  "pt.est",
  "SE",
  "ci.upper",
  "outcome.direction",
  "pt.est.desc",
  "sp.signif.param",
  "regression.type",
  "t.score"
)]

# Subset to total and direct effects
keep1=which(rd.te.df$parameter=="Total effect")
keep2=which(rd.te.df$parameter=="Total effect among eligibles")
keep3=which(rd.te.df$parameter=="Direct effect")

rd.te.df=rd.te.df[c(keep1,keep2,keep3),]

# drop point estimates that are not parameter estimates
rd.te.df=rd.te.df[rd.te.df$parameter!="",]
rd.te.df=rd.te.df[rd.te.df$pt.est.desc!="mean",]
rd.te.df=rd.te.df[rd.te.df$pt.est.desc!="Mean",]
rd.te.df=rd.te.df[!rd.te.df$description=="Mean probability of cervical cancer screening in the control group at follow-up",]
rd.te.df=rd.te.df[rd.te.df$pt.est.desc!="percentage",]
rd.te.df$pt.est=as.numeric(rd.te.df$pt.est)

# dropping "ever used condom" category from Dupas paper because
# it's not clear which direction the outcome is in
drops=grep("ever used a condom",rd.te.df$description)
rd.te.df=rd.te.df[-drops,]

# drop estimates with continuous outcomes
# While we could calculate a standardized mean difference for continuous 
# outcomes (mean diff / SE), they may be correlated with SEs, producing spurious asymmetry. 
# http://handbook.cochrane.org/chapter_10/10_4_1_funnel_plots.htm

drops = which(rd.te.df$continuous.outcome == 1)
rd.te.df = rd.te.df[-drops, ]

# manually calculate SE from CI
rd.te.df$SE[rd.te.df$id == "31031"] = (rd.te.df$ci.upper[rd.te.df$id == "31031"] -
                                         rd.te.df$pt.est[rd.te.df$id == "31031"]) / qnorm(0.975)
rd.te.df$SE[rd.te.df$id == "31031-4-r"] = (rd.te.df$ci.upper[rd.te.df$id ==
                                                               "31031"] - rd.te.df$pt.est[rd.te.df$id == "31031"]) / qnorm(0.975)

# manually calculate SE from t-statistic
rd.te.df$SE[rd.te.df$id=="30528-670"]=abs(rd.te.df$pt.est[rd.te.df$id=="30528-670"]/rd.te.df$t.score[rd.te.df$id=="30528-670"])
rd.te.df$SE[rd.te.df$t.score==0 & rd.te.df$id=="30528-670"]=NA
rd.te.df$SE[rd.te.df$id=="30528-554"]=abs(rd.te.df$pt.est[rd.te.df$id=="30528-554"]/rd.te.df$t.score[rd.te.df$id=="30528-554"])

# reverse point estimate if desired outcome is positive
rd.te.df$pt.est[rd.te.df$outcome.direction=="positive"]=-rd.te.df$pt.est[rd.te.df$outcome.direction=="positive"]

# drop rows with no SE available
rd.te.df=rd.te.df[!is.na(rd.te.df$SE),]

funnel_rd_tot = ggplot(rd.te.df,aes(x=pt.est,y=SE))+geom_point()+geom_vline(xintercept=0)+
  scale_x_continuous(limits=c(-.35,.35),
  breaks=c(-1,-.75,-.5,-.25,0,.25,.5,.75,1),
  labels=c(-1,-.75,-.5,-.25,0,.25,.5,.75,1))+theme_complete_bw()+
  xlab("Risk difference")+ylab("SE(Risk difference)")+
scale_y_continuous(limits=c(0,.45))

ggsave(
  plot = funnel_rd_tot,
  height = 5,
  width = 8,
  dpi = 200,
  filename = paste0(fig_dir, "spsr-figure-supp7-3a.pdf"),
  useDingbats = FALSE
)

#-------------------------------------
# RR funnel plot - spillover
#-------------------------------------
# manually calculating RR for 11528
df.11528=data[data$id=="11528",c("id","authors","parameter","continuous.outcome",
                                     "description","pt.est","SE","ci.upper","outcome.direction","pt.est.desc",
                                     "parameter.scale","N.exposed","N.unexposed","p1","p0")]

n0.e.11528=as.numeric(df.11528$N.unexposed[df.11528$description==
    "Mean prevalence of trachoma among eligibles in the control group at endline"])
n0.i.11528=as.numeric(df.11528$N.unexposed[df.11528$description==
    "Mean prevalence of trachoma among ineligibles in the control group at endline"])
n1.e.11528=as.numeric(df.11528$N.exposed[df.11528$description==
    "Mean prevalence of trachoma among treated in the treated group at 12 months"])
n1.i.11528=as.numeric(df.11528$N.exposed[df.11528$description==
    "Mean prevalence of trachoma among untreated in the treated group at 12 months"])

p0.e.11528=as.numeric(df.11528$p0[df.11528$description==
    "Mean prevalence of trachoma among eligibles in the control group at endline"])
p0.i.11528=as.numeric(df.11528$p0[df.11528$description==
    "Mean prevalence of trachoma among ineligibles in the control group at endline"])
p1.e.11528=as.numeric(df.11528$p1[df.11528$description==
    "Mean prevalence of trachoma among treated in the treated group at 12 months"])
p1.i.11528=as.numeric(df.11528$p1[df.11528$description==
    "Mean prevalence of trachoma among untreated in the treated group at 12 months"])

mn.c.elig.11528=as.numeric(df.11528$pt.est[df.11528$description==
      "Mean prevalence of trachoma among eligibles in the control group at endline"])
mn.c.inelig.11528=as.numeric(df.11528$pt.est[df.11528$description==
      "Mean prevalence of trachoma among ineligibles in the control group at endline"])

te.11528=as.numeric(df.11528$pt.est[df.11528$description==
      "Mean prevalence of trachoma among treated in the treated group at 12 months"]) / mn.c.elig.11528
sp.11528=as.numeric(df.11528$pt.est[df.11528$description==
      "Mean prevalence of trachoma among untreated in the treated group at 12 months"]) / mn.c.inelig.11528

#calculate se(log(RR))
se.te.11528=sqrt(((1-p1.e.11528)/(n1.e.11528*p1.e.11528))+((1-p0.e.11528)/(n0.e.11528*p0.e.11528)))
se.sp.11528=sqrt(((1-p1.i.11528)/(n1.i.11528*p1.i.11528))+((1-p0.i.11528)/(n0.i.11528*p0.i.11528)))

rr.11528=data.frame(id=rep("11528",2),authors=rep("House et al., 2009",2),
  parameter=c("Total effect among eligibles","Cluster-level spillover effect among ineligibles"),
  continuous.outcome=c("",""),description=c("",""),
  pt.est=c(te.11528,sp.11528),SE=c(se.te.11528,se.sp.11528),ci.upper=c(NA,NA),
  outcome.direction=c("positive","positive"),pt.est.desc=c("",""),parameter.scale=c("RR","RR"))

# manually calculating RR for 27358
# 27358 -----------------
df.27358=data[data$id=="27358",c("id","authors","parameter","continuous.outcome",
                                     "description","pt.est","SE","ci.upper","outcome.direction","pt.est.desc",
                                     "parameter.scale","n.exposed","n.unexposed","p1","p0","page")]

keep.27358.1=which(df.27358$id=="27358" & df.27358$description=="NMR among non-group members"&
                     df.27358$page=="web appendix page 5" )

keep.27358.1b=which(df.27358$id=="27358" & df.27358$description=="NMR among women who had heard of groups"&
                     df.27358$page=="web appendix page 5" )

keep.27358.1c=which(df.27358$id=="27358" & df.27358$description=="NMR among women's group members"&
                      df.27358$page=="web appendix page 5" )

keep.27358.2=which(df.27358$id=="27358" & df.27358$description=="Neonatal mortality rate in intervention cluster")

keep.27358.3=which(df.27358$id=="27358" & df.27358$description=="ITT NMR in control clusters")

rr.27358=df.27358[c(keep.27358.1,keep.27358.1b,keep.27358.1c,keep.27358.2,keep.27358.3),]
rr.27358=rr.27358[,!names(rr.27358) %in% "page"]

# calculate unadjusted NMR for cluster-level spillovers, converting to relative scale
nmr.tx=as.numeric(rr.27358$pt.est[rr.27358$description=="NMR among women's group members"])
nmr.untx=as.numeric(rr.27358$pt.est[rr.27358$description=="NMR among non-group members"])
nmr.heardtx=as.numeric(rr.27358$pt.est[rr.27358$description=="NMR among women who had heard of groups"])
nmr.notx=as.numeric(rr.27358$pt.est[rr.27358$description=="ITT NMR in control clusters"])

rr.27358$pt.est[rr.27358$description=="NMR among non-group members"] = nmr.untx / nmr.notx
rr.27358$pt.est[rr.27358$description=="NMR among women who had heard of groups"] = nmr.heardtx / nmr.notx
rr.27358$pt.est[rr.27358$description=="NMR among women's group members"] =nmr.tx / nmr.notx

# calculate se(log(RR))
ws.p1=rr.27358$p1[rr.27358$description=="NMR among non-group members"]
wse.p1=rr.27358$p1[rr.27358$description=="NMR among women who had heard of groups"]
de.p1=rr.27358$p1[rr.27358$description=="NMR among women's group members"]

ws.n1=rr.27358$n.exposed[rr.27358$description=="NMR among non-group members"]
wse.n1=rr.27358$n.exposed[rr.27358$description=="NMR among women who had heard of groups"]
de.n1=rr.27358$n.exposed[rr.27358$description=="NMR among women's group members"]

p0=rr.27358$p1[rr.27358$description=="ITT NMR in control clusters"]
n0=rr.27358$n.exposed[rr.27358$description=="ITT NMR in control clusters"]

# calculate se(log(RR))
rr.27358$SE[rr.27358$parameter=="Cluster-level spillover effect"]=
  sqrt(((1-ws.p1)/ws.n1*ws.p1)+((1-p0)/n0*p0))
rr.27358$SE[rr.27358$parameter=="Cluster-level spillover effect conditional on exposure to treatment"]=
  sqrt(((1-wse.p1)/wse.n1*wse.p1)+((1-p0)/n0*p0))
rr.27358$SE[rr.27358$parameter=="Direct effect"]=
  sqrt(((1-de.p1)/de.n1*de.p1)+((1-p0)/n0*p0))

rr.27358=rr.27358[-which(rr.27358$description=="ITT NMR in control clusters"),]
rr.27358=rr.27358[-which(rr.27358$description=="Neonatal mortality rate in intervention cluster"),]

rr.27358$parameter.scale=c("RR","RR","RR")

rr.27358=rr.27358[,!names(rr.27358) %in% c("p1","p0","n.exposed","n.unexposed")]

# manually calculating RR for 4204
# 4204 -----------------
df.4204=data[data$id=="4204",c("id","authors","parameter","continuous.outcome",
                                     "description","pt.est","SE","ci.upper","outcome.direction","pt.est.desc",
                                     "parameter.scale","n.exposed","n.unexposed","p1","p0")]

drop.4204=grep("Odds ratio",df.4204$description)
df.4204=df.4204[-drop.4204,]

rrest.4204=as.numeric(df.4204$pt.est[df.4204$description=="Trachoma prevalence among untreated in treated villages"])/
  as.numeric(df.4204$pt.est[df.4204$description=="Trachoma prevalence in control villages"])
n1.4204=df.4204$n.exposed[df.4204$description=="Trachoma prevalence among untreated in treated villages"]
n0.4204=df.4204$n.exposed[df.4204$description=="Trachoma prevalence in control villages"]
p1.4204=as.numeric(df.4204$pt.est[df.4204$description=="Trachoma prevalence among untreated in treated villages"])/100
p0.4204=as.numeric(df.4204$pt.est[df.4204$description=="Trachoma prevalence in control villages"])/100

#calculate se(log(RR))
se.4204=sqrt(((1-p1.4204)/n1.4204*p1.4204)+((1-p0.4204)/n0.4204*p0.4204))

rr.4204=data.frame(id="4204",authors="Chidambaram et al.",parameter=c("Cluster-level spillover effect"),
                   continuous.outcome=NA,description="",pt.est=rrest.4204,SE=se.4204,ci.upper=NA,
                   outcome.direction=NA,pt.est.desc="",parameter.scale="RR")

# Manually calculating RR for 15137-8 Egere -----------------
rr.15137.8=data[data$id=="15137-8",]
rr.15137.8=subset(rr.15137.8,rr.15137.8$parameter.scale=="RD")

n0.i.vt.15137.8=as.numeric(rr.15137.8$N.unexposed[rr.15137.8$description==
    "Risk difference for vaccine type pneumococcal carriage in vaccinated vs. control group"])
n1.i.vt.15137.8=as.numeric(rr.15137.8$N.exposed[rr.15137.8$description==
    "Risk difference for vaccine type pneumococcal carriage in vaccinated vs. control group"])
n0.i.nvt.15137.8=as.numeric(rr.15137.8$N.unexposed[rr.15137.8$description==
    "Risk difference for non-vaccine type pneumococcal carriage in vaccinated vs. control group"])
n1.i.nvt.15137.8=as.numeric(rr.15137.8$N.exposed[rr.15137.8$description==
    "Risk difference for non-vaccine type pneumococcal carriage in vaccinated vs. control group"])
n0.i.at.15137.8=as.numeric(rr.15137.8$N.unexposed[rr.15137.8$description==
    "Risk difference for any pneumococcus in vaccinated vs. control group"])
n1.i.at.15137.8=as.numeric(rr.15137.8$N.exposed[rr.15137.8$description==
    "Risk difference for any pneumococcus in vaccinated vs. control group"])

p0.i.vt.15137.8=as.numeric(rr.15137.8$p0[rr.15137.8$description==
    "Risk difference for vaccine type pneumococcal carriage in vaccinated vs. control group"])
p1.i.vt.15137.8=as.numeric(rr.15137.8$p1[rr.15137.8$description==
    "Risk difference for vaccine type pneumococcal carriage in vaccinated vs. control group"])
p0.i.nvt.15137.8=as.numeric(rr.15137.8$p0[rr.15137.8$description==
    "Risk difference for non-vaccine type pneumococcal carriage in vaccinated vs. control group"])
p1.i.nvt.15137.8=as.numeric(rr.15137.8$p1[rr.15137.8$description==
    "Risk difference for non-vaccine type pneumococcal carriage in vaccinated vs. control group"])
p0.i.at.15137.8=as.numeric(rr.15137.8$p0[rr.15137.8$description==
    "Risk difference for any pneumococcus in vaccinated vs. control group"])
p1.i.at.15137.8=as.numeric(rr.15137.8$p1[rr.15137.8$description==
    "Risk difference for any pneumococcus in vaccinated vs. control group"])

sp.rr.vt.15137.8=p1.i.vt.15137.8/p0.i.vt.15137.8
sp.rr.nvt.15137.8=p1.i.nvt.15137.8/p0.i.nvt.15137.8
sp.rr.at.15137.8=p1.i.at.15137.8/p0.i.at.15137.8

# calculate se(log(RR))
se.sp.vt.15137.8=sqrt(((1-p1.i.vt.15137.8)/(n1.i.vt.15137.8*p1.i.vt.15137.8))+((1-p0.i.vt.15137.8)/(n0.i.vt.15137.8*p0.i.vt.15137.8)))
se.sp.nvt.15137.8=sqrt(((1-p1.i.nvt.15137.8)/(n1.i.nvt.15137.8*p1.i.nvt.15137.8))+((1-p0.i.nvt.15137.8)/(n0.i.nvt.15137.8*p0.i.nvt.15137.8)))
se.sp.at.15137.8=sqrt(((1-p1.i.at.15137.8)/(n1.i.at.15137.8*p1.i.at.15137.8))+((1-p0.i.at.15137.8)/(n0.i.at.15137.8*p0.i.at.15137.8)))

rr.15137.8=data.frame(id="15137-8",authors="Egere et al.",parameter=c("Cluster-level spillover effect among ineligibles"),
    continuous.outcome=NA,description="",pt.est=c(sp.rr.vt.15137.8,sp.rr.nvt.15137.8,
    sp.rr.at.15137.8),SE=c(se.sp.vt.15137.8,se.sp.nvt.15137.8,se.sp.at.15137.8),ci.upper=NA,
    outcome.direction=NA,pt.est.desc="",parameter.scale="RR")

# Manually calculating RR for 15137 Roca -----------------
df.15137=data[data$id=="15137",]

# subset to prevalence data
df.15137 = df.15137[df.15137$parameter=="",c("description","p0","n.unexposed","N.unexposed")]

# pre-program (control)
pre = grep("pre-", df.15137$description)
df.15137.pre = df.15137[pre,]

# during program (spillover)
during = grep("CSS-3", df.15137$description)
df.15137.int = df.15137[during,]

# calculate RR
rr.15137=df.15137.int$p0/df.15137.pre$p0  

# calculate se(log(RR))
se.15137=sqrt(((1-df.15137.int$p0)/(df.15137.int$N.unexposed*df.15137.int$p0))+
   ((1-df.15137.pre$p0)/(df.15137.pre$N.unexposed*df.15137.pre$p0)))

rr.15137=data.frame(id="15137",authors="Roca et al.",
    parameter="Spillover effect conditional on exposure to treatment before and after treatment",
    continuous.outcome=NA,description="",pt.est=rr.15137,SE=se.15137,ci.upper=NA,
    outcome.direction=NA,pt.est.desc="",parameter.scale="RR")

# Manually calculating RR for 15137-11 Roca -----------------
rr.15137.11=data[data$id=="15137-11",]

# subset to data that can be used to obtain RR
rr.15137.11 = rr.15137.11[rr.15137.11$description=="Odds ratio for vaccine type pneumococcal carriage in whole vs. partially vaccinated villages; all ages",]

# calculate RR
rr.15137.11$rr=rr.15137.11$p1/rr.15137.11$p0

# calculate se(log(RR))
rr.15137.11$se.rr=sqrt(((1-rr.15137.11$p1)/(rr.15137.11$N.exposed*rr.15137.11$p1))+
                      ((1-rr.15137.11$p0)/(rr.15137.11$N.unexposed*rr.15137.11$p0)))

rr.15137.11=data.frame(id="15137-11",authors="Roca et al.",
                    parameter="Total effect conditional on exposure to treatment",
                    continuous.outcome=NA,description="",pt.est=rr.15137.11$rr,
                    SE=rr.15137.11$se.rr,ci.upper=NA,
                    outcome.direction=NA,pt.est.desc="",parameter.scale="RR")

## compile RR funnel plot data frame
# 4048 excluded because rr is in positive direction and can't convert without p1 p0
rr.list=list("5513","5514","15212-23","2147-16","15212","24229-3","2147","15137-14")

rr.df = data[data$id %in% rr.list, c(
  "id",
  "authors",
  "parameter",
  "continuous.outcome",
  "description",
  "pt.est",
  "SE",
  "ci.upper",
  "outcome.direction",
  "pt.est.desc",
  "parameter.scale",
  "adjusted"
)]

# drop unadjusted for Sur since adjusted is available
drop=which(rr.df$id=="2147" & rr.df$adjusted==0)
rr.df=rr.df[-drop,]
rr.df$adjusted=NULL

rr.df=rbind(rr.df,rr.11528)
rr.df=rbind(rr.df,rr.27358)
rr.df=rbind(rr.df,rr.4204)
rr.df=rbind(rr.df,rr.15137.8)
rr.df=rbind(rr.df,rr.15137)
rr.df=rbind(rr.df,rr.15137.11)

rr.df=rr.df[rr.df$parameter!="Total effect",]
rr.df=rr.df[rr.df$parameter!="Total effect among eligibles",]
rr.df=rr.df[rr.df$parameter!="Direct effect",]
rr.df=rr.df[rr.df$parameter!="Total vaccine efficacy",]

rr.df=rr.df[rr.df$parameter!="",]
rr.df=rr.df[rr.df$pt.est.desc!="mean",]
rr.df$pt.est=as.numeric(rr.df$pt.est)
rr.df=rr.df[!rr.df$parameter.scale=="RD",]
rr.df=rr.df[!is.na(rr.df$parameter.scale),]
rr.df=rr.df[rr.df$parameter.scale!="",]
rr.df=rr.df[rr.df$pt.est.desc!="relative risk (B v A)",]

# convert 1-RR to RR
rr.df$rr = NA
rr.df$rr[rr.df$parameter.scale == "1-RR"] =
  ((rr.df$pt.est[rr.df$parameter.scale == "1-RR"] / 100) * -1) + 1
rr.df$rr[rr.df$parameter.scale == "RR"] =
  rr.df$pt.est[rr.df$parameter.scale == "RR"]

# obtain confidence interval lower bound for 1-RR
# using upper bound
rr.df$rr.cil = NA
rr.df$rr.cil[rr.df$parameter.scale == "1-RR"] = ((rr.df$ci.upper[rr.df$parameter.scale ==
                                                                   "1-RR"] / 100) * -1) + 1
# calculate standard error of log(RR) from lower bound
rr.df$logrr.se = NA
rr.df$logrr.se[!is.na(rr.df$rr.cil)] = (log(rr.df$rr.cil[!is.na(rr.df$rr.cil)]) -
                                          log(rr.df$rr[!is.na(rr.df$rr.cil)])) / -qnorm(0.975)

# calculate standard error of log(RR) from upper bound
rr.df$logrr.se[is.na(rr.df$rr.cil) & is.na(rr.df$SE)] =
  (log(rr.df$ci.upper[is.na(rr.df$rr.cil) & is.na(rr.df$SE)]) -
         log(rr.df$rr[is.na(rr.df$rr.cil) & is.na(rr.df$SE)])) / qnorm(0.975)

# manually set SE of log(RR) for the following papers because
# that is what is actually calculated above 
rr.df$logrr.se[rr.df$id == "11528"] = rr.df$SE[rr.df$id == "11528"]
rr.df$logrr.se[rr.df$id == "27358"] = rr.df$SE[rr.df$id == "27358"]
rr.df$logrr.se[rr.df$id == "4204"] = rr.df$SE[rr.df$id == "4204"]
rr.df$logrr.se[rr.df$id == "15137"] = rr.df$SE[rr.df$id == "15137"]
rr.df$logrr.se[rr.df$id == "15137-8"] = rr.df$SE[rr.df$id == "15137-8"]
rr.df$logrr.se[rr.df$id == "15137-11"] = rr.df$SE[rr.df$id == "15137-11"]

# define log RR
rr.df$logrr=NA
rr.df$logrr=log(rr.df$rr)

# keep only if SE of log RR is not missing
rr.df=rr.df[!is.na(rr.df$logrr.se),]

# excluded Hammitt non-vaccine type results from funnel plot to be consistent with table 
drop=which(rr.df$id=="15137-14" & rr.df$description=="Prevalence ratio for non-typeable Haemophilus influenzae in individuals 5 years or older in vaccine vs. baseline period")
rr.df=rr.df[-drop,]

funnel_rr_spill = ggplot(rr.df, aes(x = logrr, y = logrr.se)) + geom_point() +
  geom_vline(xintercept = 0) +
  scale_x_continuous(
    limits = c(-2.3, 2.3),
    breaks = c(-2.3, -1.39, -.69, 0, .69, 1.39, 2.3),
    labels = c(0.1, 0.25, 0.5, 1, 2, 4, 10)
  ) + theme_complete_bw() + scale_y_continuous(limits = c(0, 0.7)) +
  xlab("Relative risk") + ylab("SE(log(Relative risk))")

ggsave(
  plot = funnel_rr_spill,
  height = 5,
  width = 8,
  dpi = 200,
  filename = paste0(fig_dir, "spsr-figure-3b.pdf"),
  useDingbats = FALSE
)

#-------------------------------------
# RR funnel plot - total effect
#-------------------------------------
rr.list=list("5513","5514","15212-23","2147-16","15212","24229-3","2147")

rr.te.df = data[data$id %in% rr.list, c(
  "id",
  "authors",
  "parameter",
  "continuous.outcome",
  "description",
  "pt.est",
  "SE",
  "ci.upper",
  "outcome.direction",
  "pt.est.desc",
  "parameter.scale"
)]
rr.te.df=rbind(rr.te.df,rr.11528)
rr.te.df=rbind(rr.te.df,rr.27358)
rr.te.df=rbind(rr.te.df,rr.4204)

# subset to total and direct effects
keep1=grep("Total",rr.te.df$parameter)
keep2=grep("Direct",rr.te.df$parameter)
rr.te.df=rr.te.df[c(keep1,keep2),]

rr.te.df=rr.te.df[rr.te.df$parameter!="",]
rr.te.df$pt.est=as.numeric(rr.te.df$pt.est)
rr.te.df=rr.te.df[!is.na(rr.te.df$parameter.scale),]
rr.te.df=rr.te.df[rr.te.df$parameter.scale!="",]

# convert 1-RR to RR
rr.te.df$rr=NA
rr.te.df$rr[rr.te.df$parameter.scale == "1-RR"] = 
  ((rr.te.df$pt.est[rr.te.df$parameter.scale == "1-RR"] / 100) * -1) + 1
rr.te.df$rr[rr.te.df$parameter.scale == "RR"] = 
  rr.te.df$pt.est[rr.te.df$parameter.scale == "RR"]

# obtain lower bound from upper bound for 1-RR
rr.te.df$rr.cil = NA
rr.te.df$rr.cil[rr.te.df$parameter.scale == "1-RR"] = 
  ((rr.te.df$ci.upper[rr.te.df$parameter.scale == "1-RR"] / 100) * -1) + 1

# calculate standard error of log(RR) from lower bound
rr.te.df$logrr.se = NA
rr.te.df$logrr.se = (log(rr.te.df$rr.cil) - log(rr.te.df$rr)) / - qnorm(0.975)

# manually set SE of log(RR) for the following papers 
rr.te.df$logrr.se[rr.te.df$id == "11528"] = rr.te.df$SE[rr.te.df$id == "11528"]
rr.te.df$logrr.se[rr.te.df$id == "27358"] = rr.te.df$SE[rr.te.df$id == "27358"]

# define log RR
rr.te.df$logrr=NA
rr.te.df$logrr=log(rr.te.df$rr)

# keep only if SE of log RR is not missing
rr.te.df=rr.te.df[!is.na(rr.te.df$logrr.se),]

funnel_rr_tot = ggplot(rr.te.df, aes(x = logrr, y = logrr.se)) + geom_point() +
  geom_vline(xintercept = 0) +
  scale_x_continuous(
    limits = c(-3, 3),
    breaks = c(-2.3, -1.39, -.69, 0, .69, 1.39, 2.3),
    labels = c(0.1, 0.25, 0.5, 1, 2, 4, 10)
  ) + theme_complete_bw() +
  scale_y_continuous(limits = c(0, .45)) +
  xlab("Risk ratio") + ylab("SE(log(Risk ratio))")

ggsave(
  plot = funnel_rr_tot,
  height = 5,
  width = 8,
  dpi = 200,
  filename = paste0(fig_dir, "spsr-figure-supp7-3b.pdf"),
  useDingbats = FALSE
)
