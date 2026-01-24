if(print_tags == 1){
  print("ModelSubs/TN.R")
}

NUE <- array(0,c(nyrs))
NUEws <- array(0,c(n_ws_NEEA,nyrs)) 
TN <- array(0,c(n_ws_NEEA,6,nyrs))
TNcmdt <- array(0,c(n_ws_NEEA,nyrs))
TNsum <- array(0,c(n_ws_NEEA,nyrs))
TNperkm <- array(0,c(n_ws_NEEA,nyrs))

for(n in 1:nyrs){
  TN[,1,n] = totNANIws[,1,n]            # atm dep
  TN[,2,n] = rowSums(CfixNwswE[,,n])    # N fix
  TN[,3,n] = sum_commod_spec_fertNsynth[,n]  # ag fertilizer
  TN[,4,n] = rowSums(kgmanureNrec450[,,n]) # manure
  TN[,5,n] = totNANIws[,4,n]            # non-ag fertilizer
  TN[,6,n] = totNANIws[,13,n]           # nonfood ag fertilizer
  TNsum[,n] = rowSums(TN[,,n])
  TNperkm[,n] = TNsum[,n] / areaws
}

for(n in 1:nyrs){
  TNcmdt[,n] = rowSums(CfixNwswE[,,n])+sum_commod_spec_fertNsynth[,n]+rowSums(kgmanureNlrs[,1:16,n])
}  

colnames(TNcmdt) <- c("1997", "2002", "2007", "2012", "2017", "2022")

TNcmdt <- as.data.frame(TNcmdt)

#breaks=c(0, 500, 500000, 1000000, 1500000, Inf)
breaks=c(0, 5e+03, 5e+04, 5e+05, 5.0e+06, Inf)

TNcmdt$discrete1997 <- cut(TNcmdt[,1], breaks=breaks, include.lowest = TRUE)
TNcmdt$discrete2002 <- cut(TNcmdt[,2], breaks=breaks, include.lowest = TRUE)
TNcmdt$discrete2007 <- cut(TNcmdt[,3], breaks=breaks, include.lowest = TRUE)
TNcmdt$discrete2012 <- cut(TNcmdt[,4], breaks=breaks, include.lowest = TRUE)
TNcmdt$discrete2017 <- cut(TNcmdt[,5], breaks=breaks, include.lowest = TRUE)
TNcmdt$discrete2022 <- cut(TNcmdt[,6], breaks=breaks, include.lowest = TRUE)
  
for(n in 1:nyrs){
  NUE[n] <- sum(CNprod[,,n])/sum(TNsum[,n])
  NUEws[,n] <- rowSums(CNprod[,,n])/TNsum[,n]
}

cbind(cbw_shp,TNcmdt)

consistent_colors <- c(
  "Very Low\n(0 - 5K)" = "#FFF7EC",    
  "Low\n(5K - 50K)" = "#FEE8C8",     
  "Medium\n(50K - 500K)" = "#FDD49E", 
  "High\n(500K - 5M)" = "#FD8D3C",  
  "Very High\n(5M - 10M)" = "#D94701"
)
library(patchwork)

# First, create all plots without legends (except one to extract the legend from)
# a <- ggplot() +
#   geom_sf(data = cbind(cbw_shp,TNcmdt), aes(fill = discrete2002))+
#   geom_sf(data = ctiescbw_shp,  fill=NA, color = "black")+
#   scale_fill_manual(values = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FD8D3C", "#D94701"), 
#                     labels = c("0 - 5e+03", 
#                                "5e+03 - 5e+04", 
#                                "5e+04 - 5e+05", 
#                                "5e+05 - 5e+06",
#                                "5e+06 - 1e+07"))+
#   labs(fill= "Anthropogenic Nitrogen (kg N)", title = "2002")+
#   theme(legend.title = element_text(color = "black", size = 16, face = "bold"),
#         legend.text = element_text(color = "black", size = 14),
#         axis.text = element_text(size = 14)) +
#   guides(fill = "none")  # Remove legend
# 
# b <- ggplot() +
#   geom_sf(data = cbind(cbw_shp,TNcmdt), aes(fill = discrete2017))+
#   geom_sf(data = ctiescbw_shp,  fill=NA, color = "black")+
#   scale_fill_manual(values = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FD8D3C", "#D94701"), 
#                     labels = c("0 - 5e+03", 
#                                "5e+03 - 5e+04", 
#                                "5e+04 - 5e+05", 
#                                "5e+05 - 5e+06",
#                                "5e+06 - 1e+07"))+
#   labs(fill= "Anthropogenic Nitrogen (kg N)", title = "2017")+
#   theme(legend.title = element_text(color = "black", size = 16, face = "bold"),
#         legend.text = element_text(color = "black", size = 14),
#         axis.text = element_text(size = 14)) +
#   guides(fill = "none")  # Remove legend
# 
# c <- ggplot() +
#   geom_sf(data = cbind(cbw_shp,TNcmdt), aes(fill = discrete2007))+
#   geom_sf(data = ctiescbw_shp,  fill=NA, color = "black")+
#   scale_fill_manual(values = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FD8D3C", "#D94701"), 
#                     labels = c("0 - 5e+03", 
#                                "5e+03 - 5e+04", 
#                                "5e+04 - 5e+05", 
#                                "5e+05 - 5e+06",
#                                "5e+06 - 1e+07"))+
#   labs(fill= "Anthropogenic Nitrogen (kg N)", title = "2007")+
#   theme(legend.title = element_text(color = "black", size = 16, face = "bold"),
#         legend.text = element_text(color = "black", size = 14),
#         axis.text = element_text(size = 14)) +
#   guides(fill = "none")  # Remove legend
# 
# d <- ggplot() +
#   geom_sf(data = cbind(cbw_shp,TNcmdt), aes(fill = discrete2012))+
#   geom_sf(data = ctiescbw_shp,  fill=NA, color = "black")+
#   scale_fill_manual(values = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FD8D3C", "#D94701"), 
#                     labels = c("0 - 5e+03", 
#                                "5e+03 - 5e+04", 
#                                "5e+04 - 5e+05", 
#                                "5e+05 - 5e+06",
#                                "5e+06 - 1e+07"))+
#   labs(fill= "Anthropogenic Nitrogen (kg N)", title = "2012")+
#   theme(legend.title = element_text(color = "black", size = 16, face = "bold"),
#         legend.text = element_text(color = "black", size = 14),
#         axis.text = element_text(size = 14)) +
#   guides(fill = "none")  # Remove legend
# 
# e <- ggplot() +
#   geom_sf(data = cbind(cbw_shp,TNcmdt), aes(fill = discrete2022))+
#   geom_sf(data = ctiescbw_shp,  fill=NA, color = "black")+
#   scale_fill_manual(values = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FD8D3C", "#D94701"), 
#                     labels = c("0 - 5e+03", 
#                                "5e+03 - 5e+04", 
#                                "5e+04 - 5e+05", 
#                                "5e+05 - 5e+06",
#                                "5e+06 - 1e+07"))+
#   labs(fill= "Anthropogenic Nitrogen (kg N)", title = "2022")+
#   theme(legend.title = element_text(color = "black", size = 16, face = "bold"),
#         legend.text = element_text(color = "black", size = 14),
#         axis.text = element_text(size = 14)) +
#   guides(fill = "none")  # Remove legend for now

# Combine the plots with equal sizing and common legend

# combined_plot <- wrap_plots(a, b, c, d, e, ncol = 2, nrow = 3) + 
#   plot_layout(guides = "collect") &
#   theme(legend.position = "bottom",
#         legend.title.position = "top",  # This puts the title above the legend
#         legend.title = element_text(hjust = 0.5))  # Center the title
# 
# combined_plot
# 
# ggsave("combined_plot.png", 
#        plot = combined_plot,
#        width = 12,      # Increased width for better proportion
#        height = 14,     # Increased height
#        units = "in",
#        dpi = 600,        # High resolution for publication
#        bg = "white")     # White background
