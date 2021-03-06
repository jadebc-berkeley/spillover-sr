####################################
# 0_base_functions.R

# spillover systematic review
# base functions

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
####################################

save_results = function(data, myid, param, desc = NULL){
  out = data %>% filter(id == myid) %>%
    filter(parameter %in% param) 
  
  if(!is.null(desc)){
    out = out %>% filter(description %in% desc) 
  } 
  
  # calculate ci.lower and ci.upper if needed
  for(i in 1:nrow(out)){
    if(is.na(out$ci.lower[i])){
      if(out$parameter.scale[i] == "RD"){
        out$ci.lower = out$pt.est - (qnorm(0.975) * out$SE)
        out$ci.upper = out$pt.est + (qnorm(0.975) * out$SE)
      } 
    }
  }
  
  out = out %>% select(authyr, parameter, int1, cluster_size,
                       coverage, parameter.scale, 
                       ref.pt.est.desc, units.outcome, 
                       pt.est, ci.lower, ci.upper)
  
  return(out)
}



theme_complete_bw <- function(base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      axis.line =         element_blank(),
      axis.text.x =       element_text(size = base_size * 0.8 , lineheight = 0.9, colour = "black", vjust = 1, margin = margin(0.1,0.1,0.1,0.1,"cm")),
      axis.text.y =       element_text(size = base_size * 0.8, lineheight = 0.9, colour = "black", hjust = 1, margin = margin(0.1,0.1,0.1,0.1,"cm")),
      axis.ticks =        element_line(colour = "black"),
      axis.title.x =      element_text(size = base_size, vjust = 0.5),
      axis.title.y =      element_text(size = base_size, angle = 90, vjust = 0.5),
      axis.ticks.length = unit(0.15, "cm"),
      
      legend.background = element_rect(colour=NA), 
      legend.key =        element_rect(fill = NA, colour = "black", size = 0.25),
      legend.key.size =   unit(1.2, "lines"),
      legend.text =       element_text(size = base_size * 0.8),
      legend.title =      element_text(size = base_size * 0.8, face = "bold", hjust = 0),
      legend.position =   "right",
      
      panel.background = element_rect(fill = "white", colour = NA), 
      panel.border =     element_rect(fill = NA, colour = "grey50"), 
      panel.grid.major = element_line(colour = "grey80", size = 0.5, linetype="dashed"), 
      panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
      panel.spacing =     unit(0.5, "lines"),
      
      strip.background = element_rect(fill = NA, colour = NA), 
      strip.text.x =     element_text(colour = "black", size = base_size * 0.8, face="bold", 
                                      margin = margin(0.3,0.1,0.1,0.1,"cm")),
      strip.text.y =     element_text(colour = "black", size = base_size * 0.8, angle = -90),
      
      plot.background =  element_rect(colour = NA, fill = "white"),
      #plot.title =       element_text(size = base_size * 1.2),
      plot.margin =      unit(c(1, 1, 0.5, 0.5), "lines"))
}
