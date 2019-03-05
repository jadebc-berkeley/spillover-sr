####################################
# spsr-figure-1-2-withinclus-sp.R

# spillover systematic review
# figures showing cluster-level
# spillover effects  

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
####################################
rm(list=ls())

source(paste0(here::here(), "/0-config.R"))

ws.tab = readRDS(paste0(data_dir, "wsplot.RDS"))

spillover.plot = ws.tab[ws.tab$parameter=="Cluster-level spillover effect among ineligibles"|
                          ws.tab$parameter=="Cluster-level spillover effect",]

# create label with outcome + int
spillover.plot$label = paste(spillover.plot$authyr,spillover.plot$int1,":",spillover.plot$outcome.primary)

#sort by spillover size
est.order=spillover.plot$label[rev(order(spillover.plot$pt.est))]

spillover.plot$label.f=factor(spillover.plot$label,levels=est.order)

# ------------------------------------
# Figure 1: Cluster-level spillover effects

# note: labels were manually cleaned in 
# Illustrator
# ------------------------------------
forestbw = ggplot(spillover.plot, aes(x = label.f, y = pt.est, group = signif)) +
  geom_errorbar(
    mapping = aes(
      x = label.f,
      y = pt.est,
      ymin = ci.lower,
      ymax = ci.upper,
      width = 0.2,
      linetype = parameter
    )
  ) +
  geom_point(aes(x = label.f, y = pt.est, shape = signif), size = 2.5) +
  coord_flip() +
  geom_hline(aes(yintercept = 0), linetype = 2) + theme_bw() +
  xlab("") + scale_shape_manual("Statistically significant", values = c(19, 17, 15)) +
  ylab("Spillover estimate (1-RR)*100%") +
  scale_linetype_manual("Parameter", values = c("solid", "longdash"))

forestbw

ggsave(
  plot = forestbw,
  height = 3.5,
  width = 12,
  dpi = 200,
  filename = paste0(fig_dir, "spsr-figure-1.pdf"),
  useDingbats = FALSE
)

# ------------------------------------
# Figure 2: Cluster-level spillover effects by treatment coverage level

# note: labels were manually cleaned in 
# Illustrator
# ------------------------------------
spillover.plot$cov = spillover.plot$cov * 100

covplotbw = ggplot(spillover.plot, aes(x = cov, y = pt.est)) +
  geom_point(aes(shape = parameter)) +
  geom_errorbar(
    mapping = aes(
      x = cov,
      y = pt.est,
      ymin = ci.lower,
      ymax = ci.upper,
      width = 2,
      linetype = parameter
    )
  ) +
  geom_text(aes(label = authyr, vjust = 2),
            size = 3,
            show.legend = FALSE) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  theme_bw() +
  scale_y_continuous(limits = c(-120, 100)) +
  scale_shape_manual("Parameter", values = c(16, 17)) +
  scale_linetype_manual("Parameter", values = c("solid", "longdash")) +
  xlab("Treatment coverage within treated clusters (%)") +
  ylab("Cluster-level spillover effect (1-RR)*100%") 

covplotbw

ggsave(
  plot = covplotbw,
  height = 4,
  width = 9,
  dpi = 200,
  filename = paste0(fig_dir, "spsr-figure-2.pdf"),
  useDingbats = FALSE
)
