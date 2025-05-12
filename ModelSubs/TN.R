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
breaks=c(0, 500, 50000, 100000, 150000, Inf)

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

ggplot() +
  geom_sf(data = cbind(cbw_shp,TNcmdt), aes(fill = discrete2017))+
  geom_sf(data = ctiescbw_shp, linewidth = 0.9, fill=NA, color = "black")+
  scale_fill_manual(values = cols, labels = c("< 5.0e+02", "5.0e+02 - 5.0e+05", "5.0e+05 - 1.0e+06", "1.0e+06 - 1.5e+06",  "1.5e+06 - 9.8e+06"))+
  labs(fill= "Anthropogenic Nitrogen (kg N)")+
  theme(legend.title = element_text(color = "black", size = 16, face = "bold"),  # Adjust title appearance
        legend.text = element_text(color = "black", size = 14),
        axis.text = element_text(size = 14) )  # Adjust number appearance
