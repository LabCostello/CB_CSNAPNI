if(print_tags == 1){
  print("ModelSubs/TN.R")
}

NUE <- array(0,c(nyrs))
NUEws <- array(0,c(n_ws_NEEA,nyrs)) 
TN <- array(0,c(n_ws_NEEA,6,nyrs))
TNsum <- array(0,c(n_ws_NEEA,nyrs))
TNperkm <- array(0,c(n_ws_NEEA,nyrs))

for(n in 1:nyrs){
  TN[,1,n] = totNANIws[,1,n]            # atm dep
  TN[,2,n] = rowSums(CfixNwswE[,,n])    # N fix
  TN[,3,n] = sum_commod_spec_fertN[,n]  # ag fertilizer
  TN[,4,n] = rowSums(kgmanureNrec450[,,n]) # manure
  TN[,5,n] = totNANIws[,4,n]            # non-ag fertilizer
  TN[,6,n] = totNANIws[,13,n]           # nonfood ag fertilizer
  TNsum[,n] = rowSums(TN[,,n])
  TNperkm[,n] = TNsum[,n] / areaws
}

for(n in 1:nyrs){
  NUE[n] <- sum(CNprod[,,n])/sum(TNsum[,n])
  NUEws[,n] <- rowSums(CNprod[,,n])/TNsum[,n]
}
