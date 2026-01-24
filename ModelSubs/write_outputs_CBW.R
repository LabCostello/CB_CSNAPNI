# write_outputs.R
# this file writes some of the resulting calculations from CSNAPNI to text files

if(print_tags == 1){
  print("ModelSubs/write_outputs_CBW.R")
}

#OUTPUT MATRICES
NANIBtot_US = array(0,c(length(NANIBtot[1,,1]),nyrs))
NANIorigtot_US = array(0,c(length(NANIorigtot[1,,1]),nyrs))
NAPIBtot_US = array(0,c(length(NAPIBtot[1,,1]),nyrs))
NAPIorigtot_US = array(0,c(length(NAPIorigtot[1,,1]),nyrs))
NANI_wsspec = array(0,c(length(NANIBtot[1,,1]),nyrs))
NAPI_wsspec = array(0,c(length(NAPIBtot[1,,1]),nyrs))
kgfertNcnty=array(0,c(n_cnty,n_crops,nyrs))#fertilizer N by county
kgfixNcnty=array(0,c(n_cnty,n_crops,nyrs))#fixation N by county
kgfertPcnty=array(0,c(n_cnty,n_crops,nyrs))#fertilizer P by county
kgcropNcnty=array(0,c(n_cnty,n_crops,nyrs))# N in crop by county
kgcropPcnty=array(0,c(n_cnty,n_crops,nyrs))# P in crop by county
kgmeatNcnty = array(0,c(n_cnty,n_meats,nyrs))#animal N in meat by county
kgmeatPcnty = array(0,c(n_cnty,n_meats,nyrs))#animal P in meat by county
kganimNintakecnty = array(0,c(n_cnty,n_anims,nyrs)) #animal N intake by county
kganimPintakecnty = array(0,c(n_cnty,n_anims,nyrs)) #animal P intake by county

for(n in 1:nyrs){
  NANIBtot_US[,n] = colSums(NANIBtot[,,n])/(10^6)
  NANIorigtot_US[,n] = colSums(NANIorigtot[,,n])/(10^6)
  NAPIBtot_US[,n] = colSums(NAPIBtot[,,n])/(10^6)
  NAPIorigtot_US[,n] = colSums(NAPIorigtot[,,n])/(10^6)
  if (length(ws)>1){
    NANI_wsspec[,n] = colSums(NANIBtot[ws,,n])/(10^6)
    NAPI_wsspec[,n] = colSums(NAPIBtot[ws,,n])/(10^6)
  } else {
    NANI_wsspec[,n] = NANIBtot[ws,,n]/(10^6)
    NAPI_wsspec[,n] = NAPIBtot[ws,,n]/(10^6)
  }
}

total_commoddisag_fertN = array(c(t(colSums(CfertNwswE)), t(colSums(etohfertNws))),c(nyrs,(n_crops+1)))/(10^6) #total disaggregated fert N inputs (10^6 kg N)
total_commoddisag_fertP = array(c(t(colSums(CfertPwswE)), t(colSums(etohfertPws))),c(nyrs,(n_crops+1)))/(10^3) #total disaggregated fert P inputs (10^3 kg P)
total_commoddisag_fixN = colSums(CfixNwswE[,,])/10^6 #total commodity disaggregated fixation N inputs in millions kg N (10^6 kg N)

if (length(ws)>1){
  total_commoddisag_fertN_ws = array(c(t(colSums(CfertNwswE[ws,,])), t(colSums(etohfertNws[ws,]))),c(nyrs,(n_crops+1)))/(10^3) #total disaggregated fert N inputs (10^3 kg N)
  total_commoddisag_fertP_ws = array(c(t(colSums(CfertPwswE[ws,,])), t(colSums(etohfertPws[ws,]))),c(nyrs,(n_crops+1)))/(10^3) #total disaggregated fert P inputs (10^3 kg P)
  total_commoddisag_fixN_ws = colSums(CfixNwswE[ws,,])/10^3 #total commodity disaggregated fixation N inputs in thousand kg N
} else {
  total_commoddisag_fertN_ws = array(c(t(CfertNwswE[ws,,]), t(etohfertNws[ws,])),c(nyrs,(n_crops+1)))/(10^6) #total disaggregated fert N inputs (10^3 kg N)
  total_commoddisag_fertP_ws = array(c(t(CfertPwswE[ws,,]), t(etohfertPws[ws,])),c(nyrs,(n_crops+1)))/(10^6) #total disaggregated fert P inputs (10^3 kg P)
  total_commoddisag_fixN_ws = CfixNwswE[ws,,]/10^3 #total commodity disaggregated fixation N inputs in thousand kg N
}

write_name = paste("OutputFiles_CBW/total_commoddisag_fertN.txt",sep = "")
write.table(t(total_commoddisag_fertN), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

write_name = paste("OutputFiles_CBW/total_commoddisag_fertP.txt",sep = "")
write.table(t(total_commoddisag_fertP), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

write_name = paste("OutputFiles_CBW/total_commoddisag_fixN.txt",sep = "")
write.table(t(total_commoddisag_fixN), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

write_name = paste("OutputFiles_CBW/total_commoddisag_fertN_ws",ws_name,".txt",sep = "")
write.table(t(total_commoddisag_fertN_ws), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

write_name = paste("OutputFiles_CBW/total_commoddisag_fertP_ws",ws_name,".txt",sep = "")
write.table(t(total_commoddisag_fertP_ws), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

write_name = paste("OutputFiles_CBW/total_commoddisag_fixN_ws",ws_name,".txt",sep = "")
write.table(t(total_commoddisag_fixN_ws), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

#write commoddisag key
write_name = paste("OutputFileKeys/commoddisag_key.txt")
commoddisag_key = array(" ", c(1+n_crops+1,1+nyrs))
commoddisag_key[1,] = c(" ", run_yrs) #column headings
commoddisag_key[,1]=c("10^3 kg N or P","corn (etoh use removed)","corn silage","wheat","oats",
                      "barley","sorghum grain","sorghum silage","rice","rye","soybeans","potatoes",
                      "alfalfa hay","other hay","cropland pasture","noncropland pasture","peanuts","grass","winter rye",
                      "CGF","CGM","DGS","etoh") #row headings
write.table(commoddisag_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

#animal products 
total_meat_fertN = array(0,c(n_meats,nyrs))
total_meat_fertP = array(0,c(n_meats,nyrs))
total_meat_fixN = array(0,c(n_meats,nyrs))
total_meat_manureN = array(0,c(n_meats,nyrs))
total_meat_manureP = array(0,c(n_meats,nyrs))

for(n in 1:nyrs){
  total_meat_fertN[,n] = rowSums(fertNmeattot[,,n])/(10^3) #fertilizer inputs to meat products in each year
  total_meat_fertP[,n] = rowSums(fertPmeattot[,,n])/(10^3) #fertilizer inputs to meat products in each year
  total_meat_fixN[,n] = rowSums(fixNmeattot[,,n])/(10^3) #fertilizer inputs to meat products in each year
}
total_meat_suppP = Psupp4meat/10^3 #ton P
total_meat_manureN = colSums(manureNmeat)/(10^3) #thousand kg N
total_meat_manureP = colSums(manurePmeat)/(10^3) #thousand kg P

## Field loss proportions
Nincrop=Nincrop
NinperkgC=NinputtoC/drop(colSums(CkgwswE))
NoutperkgC=Nincrop
CNproplost=(NinperkgC-NoutperkgC)/NinperkgC

Pincrop=cropdata[,1]*cropdata[,3]
PinperkgC=PinputtoC/drop(colSums(CkgwswE))
PoutperkgC=Pincrop
CPproplost=(PinperkgC-PoutperkgC)/PinperkgC


for(n in 1:nyrs){
  #product and watershed specific outputs
  write_name = paste("OutputFiles_CBW/CfertNwswE",run_yrs[n],".txt",sep = "")
  write.table(CfertNwswE[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles_CBW/CfertPwswE",run_yrs[n],".txt",sep = "")
  write.table(CfertPwswE[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles_CBW/CfixNwswE",run_yrs[n],".txt",sep = "")
  write.table(CfixNwswE[,,], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles_CBW/fertNmeattot",run_yrs[n],".txt",sep = "")
  write.table(fertNmeattot[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles_CBW/fertPmeattot",run_yrs[n],".txt",sep = "")
  write.table(fertPmeattot[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles_CBW/fixNmeattot",run_yrs[n],".txt",sep = "")
  write.table(fixNmeattot[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles_CBW/ws_spec_manureNmeat",run_yrs[n],".txt",sep = "")
  write.table(manureNmeat[ws,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles_CBW/ws_spec_manurePmeat",run_yrs[n],".txt",sep = "")
  write.table(manurePmeat[ws,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles_CBW/atmNdep",run_yrs[n],".txt",sep = "")
  write.table(totNANIws[,1,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles_CBW/ws_spec_NANIBtot",run_yrs[n],".txt",sep = "")
  write.table(NANIBtot[ws,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles_CBW/ws_spec_NAPIBtot",run_yrs[n],".txt",sep = "")
  write.table(NAPIBtot[ws,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles_CBW/ws_spec_animNreqmeat",run_yrs[n],".txt",sep = "")
  write.table(animNreqpermeat[,n]*kgmeat[ws,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles_CBW/ws_spec_animPreqmeat",run_yrs[n],".txt",sep = "")
  write.table(animPreqpermeat[,n]*kgmeat[ws,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles_CBW/ws_spec_noanimwsdyn",run_yrs[n],".txt",sep = "")
  write.table(noanimwsdyn[ws,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #human N intake by county
  write_name = paste("OutputFiles_CBW/kghumanNintakecnty",run_yrs[n],".txt",sep = "")
  write.table(population_cnty[,n]*Nperhmn[n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #human P intake by county
  write_name = paste("OutputFiles_CBW/kghumanPintakecnty",run_yrs[n],".txt",sep = "")
  write.table(population_cnty[,n]*foodPperhmn[n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #human detergent P use by county
  write_name = paste("OutputFiles_CBW/kghumanPintakecnty",run_yrs[n],".txt",sep = "")
  write.table(population_cnty[,n]*detPperhmn[n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #calculate animal county-level outputs
  for(m in 1:n_meats){
    #meat to anim key
    if(m==1){
      a=1
    }else if(m==2){
      a=2
    }else if(m==3){
      a=4
    }else if(m==6){
      a=5
    }else if(m==7){
      a=8
    }else if(m==8){
      a=9
    }else if(m==4){
      a=17
    }else if(m==5){
      a=18
    }else if(m==9){
      a=19
    }
    #animal N in meat by county
    kgmeatNcnty[,m,n] = (noanimdyncty[, m, n] / totnoanimws[n, m]) * meatN[m, n]
    
    #animal P in meat by county
    kgmeatPcnty[,m,n] = (noanimdyncty[, m, n] / totnoanimws[n, m]) * meatP[m, n]
  }
  for(a in 1:n_anims){
    #animal N intake by county
    kganimNintakecnty[,a,n]=noanimdyncty[,a,n]*animdatadyn[a,8]
    
    #animal P intake by county
    kganimPintakecnty[,a,n]=noanimdyncty[,a,n]*animdatadyn[a,9]
  }
  #animal N in meat by county
  write_name = paste("OutputFiles_CBW/kgmeatNcnty",run_yrs[n],".txt",sep = "")
  write.table(kgmeatNcnty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #animal P in meat by county
  write_name = paste("OutputFiles_CBW/kgmeatPcnty",run_yrs[n],".txt",sep = "")
  write.table(kgmeatPcnty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #animal N intake by county
  write_name = paste("OutputFiles_CBW/kganimNintakecnty",run_yrs[n],".txt",sep = "")
  write.table(kganimNintakecnty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #animal P intake by county
  write_name = paste("OutputFiles_CBW/kganimPintakecnty",run_yrs[n],".txt",sep = "")
  write.table(kganimPintakecnty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #total manure N by county
  write_name = paste("OutputFiles_CBW/kgmanureNcnty",run_yrs[n],".txt",sep = "")
  write.table(kgmanureNcnty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #total manure P by county
  write_name = paste("OutputFiles_CBW/kgmanurePcnty",run_yrs[n],".txt",sep = "")
  write.table(kgmanurePcnty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #recoverable manure N by county
  write_name = paste("OutputFiles_CBW/kgmanureNreccnty",run_yrs[n],".txt",sep = "")
  write.table(kgmanureNrec[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

  #recoverable manure N by LRS
  write_name = paste("OutputFiles_CBW/kgmanureNreclrs",run_yrs[n],".txt",sep = "")
  write.table(kgmanureNrecws[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
    
  #recoverable manure P by county
  write_name = paste("OutputFiles_CBW/kgmanurePreccnty",run_yrs[n],".txt",sep = "")
  write.table(kgmanurePrec[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #calculate crop county-level outputs
  for(k in 1:n_crops){
    #fertilizer N by county
    kgfertNcnty[,k,n]=cropprodcnty[,k,n]*unitfertNC[k,n]
    
    #fixation N by county
    kgfixNcnty[,k,n]=cropprodcnty[,k,n]*unitfixNC[k,n]
    
    #fertilizer P by county
    kgfertPcnty[,k,n]=cropprodcnty[,k,n]*unitfertPC[k,n]
    
    # N in crop by county
    kgcropNcnty[,k,n]=cropprodcnty[,k,n]*Nincrop[k] #cropdata[,1] * cropdata[,2] = (% DM) * (% N in DM)
    
    # P in crop by county
    kgcropPcnty[,k,n]=cropprodcnty[,k,n]*Nincrop[k] #cropdata[,1] * cropdata[,3] = (% DM) * (% P in DM)
  }
  
  #fertilizer N by county
  write_name = paste("OutputFiles_CBW/kgfertNcnty",run_yrs[n],".txt",sep = "")
  write.table(kgfertNcnty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #fixation N by county
  write_name = paste("OutputFiles_CBW/kgfixNcnty",run_yrs[n],".txt",sep = "")
  write.table(kgfixNcnty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #fertilizer P by county
  write_name = paste("OutputFiles_CBW/kgfertPcnty",run_yrs[n],".txt",sep = "")
  write.table(kgfertPcnty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  # N in crop by county
  write_name = paste("OutputFiles_CBW/kgcropNcnty",run_yrs[n],".txt",sep = "")
  write.table(kgcropNcnty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE) #cropdata[,1] * cropdata[,2] = percent N in each crop (% DM) * (% N in DM)
  
  # P in crop by county
  write_name = paste("OutputFiles_CBW/kgcropPcnty",run_yrs[n],".txt",sep = "")
  write.table(kgcropPcnty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE) #cropdata[,1] * cropdata[,3] = percent P in each crop (% DM) * (% P in DM)
  
  # Crop prod by county
  write_name = paste("OutputFiles_CBW/kgcropcnty",run_yrs[n],".txt",sep = "")
  write.table(cropprodcnty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  # Crop area by county (km^2)
  write_name = paste("OutputFiles_CBW/cropareacnty",run_yrs[n],".txt",sep = "")
  write.table(cropareacnty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
}

#write US total NANI and NAPI to file
write_name = paste("OutputFiles_CBW/US_NANIBtot.txt",sep = "")
write.table(NANIBtot_US, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

write_name = paste("OutputFiles_CBW/US_NAPIBtot.txt",sep = "")
write.table(NAPIBtot_US, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

write_name = paste("OutputFiles_CBW/etohfertNtot.txt",sep = "")
write.table(t(etohfertNtot), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
write_name = paste("OutputFiles_CBW/etohfertPtot.txt",sep = "")
write.table(t(etohfertPtot), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

#write etohfert key
write_name = paste("OutputFileKeys/etohfertNtot_key.txt")
etohfert_key = array(" ", c(1,1+nyrs))
etohfert_key[1,] = c(" ", run_yrs) #column headings
etohfert_key[,1]=c("kg N or P") #row headings
write.table(etohfert_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

### PRODUCING PICTURES FOR THE RESULTS
# Graph for paper commodity specific results ####
fertNpermeatdom <- fertNpermeatnat <- fertNperMprotdom <- fertmanNpermeatdom <- fertmanNperMprotdom <- fertinNperMprotdom <- fertinNpermeatdom <- fertNperMprotnat <- array(0,c(9,21,6))
fixNpermeatdom <- fixNpermeatnat <- fixNperMprotdom <- fixNperMprotnat <-  array(0,c(9,21,6))
byprodpermeatprot <- array(0,c(n_meats,9,6))
dimnames(byprodpermeatprot) <- list(meat_labels,byproductnames,year_labels)

for(n in 1:nyrs){
  for(i in 1:n_meats){byprodpermeatprot[i,,n] = (byprodNpermeat[i,,n] / (meatdata[i,z]*c_dm[c(3,6,8,9,11,15,16,17,18)]*c_ncindm[c(3,6,8,9,11,15,16,17,18)] / 1000))}} #kg byprod/kg meat protein

beefnatfertN <- c(0.635,0.623,0.592,0.651,0.537,0.537)
beefnatfixN <- c(1.15,1.18,1.02,0.882,0.909,0.909)
beefnatmanN <- c(1.64,1.63,1.61,1.60,1.60,1.60)

broilernatfertN <- c(0.296,0.269,0.283,0.289,0.213,0.21)
broilernatfixN <- c(0.232,0.251,0.207,0.240,0.233,0.23)
broilernatmanN <- c(0.364,0.365,0.365,0.364,0.363,0.363)

fertNbyproductN <- fixNbyproductN <- manureNbyproductN <- array(0,c(9,nyrs))

byprod_beefalloc <- 0
byprod_broileralloc <- 0
ricealloc <- 0
hfalloc <- 0.35
canolaalloc <- 0.6 # based on literature (3 sources)
cottonseedalloc <- 0.56 # based on literature (1 sources)
cottonseedmealalloc <- cottonseedalloc*0.89 # based on literature (1 sources)

for(n in 1:nyrs){
  fertNbyproductN[,n] <-c(hfalloc*unitfertNC[1,n],cottonseedalloc*0.1,canolaalloc*0.07,cottonseedmealalloc*0.1,unitfertNC[15,n],beefnatfertN[n]*byprod_beefalloc,broilernatfertN[n]*byprod_broileralloc,beefnatfertN[n]*byprod_beefalloc,beefnatfertN[n]*byprod_beefalloc) # Update cotton and canola calculation
  fixNbyproductN[,n] <- c(0,0,0,0,0,beefnatfixN[n]*byprod_beefalloc,broilernatfixN[n]*byprod_broileralloc,beefnatfixN[n]*byprod_beefalloc,beefnatfixN[n]*byprod_beefalloc)
  manureNbyproductN[,n] <- c(0,0,0,0,0,beefnatmanN[n]*byprod_beefalloc,broilernatmanN[n]*byprod_broileralloc,beefnatmanN[n]*byprod_beefalloc,beefnatmanN[n]*byprod_beefalloc)}

fertNperbyprod <- fixNperbyprod <- manureNperbyprod <-  array(0,c(9,9,6))

for(n in 1:nyrs){
  for(j in 1:9){
    fertNperbyprod[,j,n] <- byprodpermeatprot[,j,n]*fertNbyproductN[j,n]
    fixNperbyprod[,j,n] <- byprodpermeatprot[,j,n]*fixNbyproductN[j,n]
    manureNperbyprod[,j,n] <- byprodpermeatprot[,j,n]*manureNbyproductN[j,n]
  }}

dimnames(fertNperbyprod) <- dimnames(fixNperbyprod) <- dimnames(manureNperbyprod) <- list(meat_labels,byproductnames,year_labels)


for(n in 1:nyrs){
  for(i in 1:21){
    #fertNpermeatdom[,i,n] <- (feedpermeatdom[,i,n] * unitfertNC[i,n]) # Original way to calculate where unitfertNC represents total fertilizer applied to the crop based solely on their demand of N, e.g. 150lbs acre for corn
    fertNpermeatdom[,i,n] <- (feedpermeatdom[,i,n] * (unitfertinNC[i,n]+unitfertmanNC[i,n])) # This way consider the total N available to the plant, the inorganic fertilizer needed to achieve unitfertNC and the manure applied but that the N end up not available to the plant
    fertinNpermeatdom[,i,n] <- (feedpermeatdom[,i,n] * (unitfertinNC[i,n]))
    fertmanNpermeatdom[,i,n] <- (feedpermeatdom[,i,n] * (unitfertmanNC[i,n]))
    fertNpermeatnat[,i,n] <- (feedpermeatimp[,i,n] * unitfertNCnational[i,n])
    fixNpermeatdom[,i,n] <- (feedpermeatdom[,i,n] * unitfixNC[i,n]) 
    fixNpermeatnat[,i,n] <- (feedpermeatimp[,i,n] * unitfixNC[i,n])}}

for(n in 1:nyrs){
  for(i in 1:n_meats){
    fertNperMprotdom[i,,n] = (fertNpermeatdom[i,,n] / (meatdata[i,z] / 1000))
    fertinNperMprotdom[i,,n] = (fertinNpermeatdom[i,,n] / (meatdata[i,z] / 1000))
    fertmanNperMprotdom[i,,n] = (fertmanNpermeatdom[i,,n] / (meatdata[i,z] / 1000))
    fertNperMprotnat[i,,n] = (fertNpermeatnat[i,,n] / (meatdata[i,z] / 1000))# kg N fert / kg protein in 1 kg meat
    fixNperMprotdom[i,,n] = (fixNpermeatdom[i,,n] / (meatdata[i,z] / 1000))
    fixNperMprotnat[i,,n] = (fixNpermeatnat[i,,n] / (meatdata[i,z] / 1000))}}

library(readxl)
cmdtcsnapninat<- read_excel(adjust_file_path("C:/Users/lds5498/OneDrive - The Pennsylvania State University/Desktop/Code/important_excel/cmdtcsnapni3nat2.xlsx"))

cmdtcsnapni3a <- data.frame("year"=rep(rep(year_labels,each = 9),2), 
                            "Nsource" = rep(c("Fertilizer", "Fixation"),each=54),
                            "model"=rep("cbw-csnani",108),
                            "animal"=rep(meat_labels,12),
                            "origin"=rep("national",108),
                            "values" = c(rowSums(fertNperMprotnat[,,1]),rowSums(fertNperMprotnat[,,2]),rowSums(fertNperMprotnat[,,3]),
                                         rowSums(fertNperMprotnat[,,4]),rowSums(fertNperMprotnat[,,5]),rowSums(fertNperMprotnat[,,6]),
                                         rowSums(fixNperMprotnat[,,1]),rowSums(fixNperMprotnat[,,2]),rowSums(fixNperMprotnat[,,3]),
                                         rowSums(fixNperMprotnat[,,4]),rowSums(fixNperMprotnat[,,5]),rowSums(fixNperMprotnat[,,6])))

cmdtcsnapni3a <- rbind(cmdtcsnapninat,cmdtcsnapni3a)

cmdtcsnapni3b <- data.frame("year" = rep(rep(year_labels,each = 9),3), 
                            "Nsource" = rep(c("Fertilizer", "Fixation","Manure"),each=54),
                            "model"=rep("cbw-csnani",162),
                            "animal"=rep(meat_labels,18),
                            "origin"=rep("cbw",162),
                            "values" = c(rowSums(fertNperMprotdom[,,1]),rowSums(fertNperMprotdom[,,2]),rowSums(fertNperMprotdom[,,3]),
                                         rowSums(fertNperMprotdom[,,4]),rowSums(fertNperMprotdom[,,5]),rowSums(fertNperMprotdom[,,6]),rowSums(fixNperMprotdom[,,1]),rowSums(fixNperMprotdom[,,2]),rowSums(fixNperMprotdom[,,3]),
                                         rowSums(fixNperMprotdom[,,4]),rowSums(fixNperMprotdom[,,5]),rowSums(fixNperMprotdom[,,6]),manureNperprot[,1],manureNperprot[,2],manureNperprot[,3],manureNperprot[,4],manureNperprot[,5],manureNperprot[,6]))


cmdtcsnapni3 <- rbind(cmdtcsnapni3a,cmdtcsnapni3b)
cmdtcsnapni3 <- cmdtcsnapni3[cmdtcsnapni3$animal %in% meat_labels[c(1,2,3,6,7,8)],]

cmdtcsnapni3 <- cmdtcsnapni3[cmdtcsnapni3$year %in% year_labels[2:6],]

cmdtcsnapni3$Nsources <- sprintf("%s (%s)", cmdtcsnapni3$Nsource, cmdtcsnapni3$origin)
cmdtcsnapni3$Nsources[cmdtcsnapni3$Nsources=="Manure (cbw)" | cmdtcsnapni3$Nsources=="Manure (national)"] <- "Non-recoverable manure"
#convert 'position' to factor and specify level order
cmdtcsnapni3$Nsources <- factor(cmdtcsnapni3$Nsources, levels=c("Non-recoverable manure", "Fixation (national)", "Fixation (cbw)", "Fertilizer (national)", "Fertilizer (cbw)"))


library(ggplot2)
library(ggpubr)
library(ggpattern)

cmdtcsnapni3$model[cmdtcsnapni3$model == "csnapni"] <- "national"
cmdtcsnapni3$model[cmdtcsnapni3$model == "cbw-csnani"] <- "CBW"


#####

zz <- data.frame(beef=colSums(colSums(kgmanureNrecplantLRS[,animal_manure,])[c(1,11,13,15),]),
                 dairy=colSums(colSums(kgmanureNrecplantLRS[,animal_manure,])[c(2,12,14,16),]),
                 swine=colSums(colSums(kgmanureNrecplantLRS[,animal_manure,])[c(3,4),]),
                 broilers=colSums(colSums(kgmanureNrecplantLRS[,animal_manure,])[c(8,7),]),
                 eggs=colSums(kgmanureNrecplantLRS[,animal_manure,])[5,],
                 turkey=colSums(colSums(kgmanureNrecplantLRS[,animal_manure,])[c(6,9),])
)

zzz <- data.frame(beef=colSums((colSums(kgmanureNrec450[,animal_manure,]-kgmanureNrecplantLRS[,animal_manure,]))[c(1,11,13,15),]),
                  dairy=colSums((colSums(kgmanureNrec450[,animal_manure,]-kgmanureNrecplantLRS[,animal_manure,]))[c(2,12,14,16),]),
                  swine=colSums((colSums(kgmanureNrec450[,animal_manure,]-kgmanureNrecplantLRS[,animal_manure,]))[c(3,4),]),
                  broilers=colSums((colSums(kgmanureNrec450[,animal_manure,]-kgmanureNrecplantLRS[,animal_manure,]))[c(8,7),]),
                  layers=colSums(kgmanureNrec450[,animal_manure,]-kgmanureNrecplantLRS[,animal_manure,])[5,],
                  turkey=colSums((colSums(kgmanureNrec450[,animal_manure,]-kgmanureNrecplantLRS[,animal_manure,]))[c(6,9),])
)


cmdtcsnapni4 <- data.frame(
  "year" = c(rep(rep(year_labels, each = 9), 3),  # From cmdtcsnapni4
             rep(rep(year_labels, each = 9), 4)),  # From original cmdtcsnapni3b
  
  "Nsource" = c(rep(c("Fertilizer (national)", "N Fixation (national)", "Non-recoverable Manure (national)"), each = 54),  # From cmdtcsnapni4
                rep(c("Inorganic Fertilizer (cbw)", "Manure Fertilizer (cbw)", "N Fixation (cbw)", "Non-recoverable Manure (cbw)"), each = 54)),  # From original cmdtcsnapni3b
  
  "animal" = c(rep(meat_labels, 18),  # From cmdtcsnapni4
               rep(meat_labels, 24)),  # From original cmdtcsnapni3b
  
  "origin" = c(rep("national", 162),  # From cmdtcsnapni4
               rep("cbw", 216)),  # From original cmdtcsnapni3b
  
  "values" = c(
    # Values from cmdtcsnapni4 (national)
    rowSums(fertNperMprotnat[,,1])+rowSums(fertNperbyprod[,,1]), rowSums(fertNperMprotnat[,,2])+rowSums(fertNperbyprod[,,2]), rowSums(fertNperMprotnat[,,3])+rowSums(fertNperbyprod[,,3]),
    rowSums(fertNperMprotnat[,,4])+rowSums(fertNperbyprod[,,4]), rowSums(fertNperMprotnat[,,5])+rowSums(fertNperbyprod[,,5]), rowSums(fertNperMprotnat[,,6])+rowSums(fertNperbyprod[,,6]),
    rowSums(fixNperMprotnat[,,1])+rowSums(fixNperbyprod[,,1]), rowSums(fixNperMprotnat[,,2])+rowSums(fixNperbyprod[,,2]), rowSums(fixNperMprotnat[,,3])+rowSums(fixNperbyprod[,,3]),
    rowSums(fixNperMprotnat[,,4])+rowSums(fixNperbyprod[,,4]), rowSums(fixNperMprotnat[,,5])+rowSums(fixNperbyprod[,,5]), rowSums(fixNperMprotnat[,,6])+rowSums(fixNperbyprod[,,6]),
    rowSums(manureNperbyprod[,,1]),rowSums(manureNperbyprod[,,2]),rowSums(manureNperbyprod[,,3]),rowSums(manureNperbyprod[,,4]),rowSums(manureNperbyprod[,,5]),rowSums(manureNperbyprod[,,6]),

    # Values from original cmdtcsnapni3b (cbw)
    rowSums(fertinNperMprotdom[,,1]), rowSums(fertinNperMprotdom[,,2]), rowSums(fertinNperMprotdom[,,3]),
    rowSums(fertinNperMprotdom[,,4]), rowSums(fertinNperMprotdom[,,5]), rowSums(fertinNperMprotdom[,,6]),
    rowSums(fertmanNperMprotdom[,,1]), rowSums(fertmanNperMprotdom[,,2]), rowSums(fertmanNperMprotdom[,,3]),
    rowSums(fertmanNperMprotdom[,,4]), rowSums(fertmanNperMprotdom[,,5]), rowSums(fertmanNperMprotdom[,,6]),
    rowSums(fixNperMprotdom[,,1]), rowSums(fixNperMprotdom[,,2]), rowSums(fixNperMprotdom[,,3]),
    rowSums(fixNperMprotdom[,,4]), rowSums(fixNperMprotdom[,,5]), rowSums(fixNperMprotdom[,,6]),
    manureNperprot[,1], manureNperprot[,2], manureNperprot[,3], manureNperprot[,4], manureNperprot[,5], manureNperprot[,6]
  )
)

cmdtcsnapni4 <- cmdtcsnapni4[cmdtcsnapni4$animal %in% meat_labels[c(1,2,3,6,7,8)],]

cmdtcsnapni4 <- cmdtcsnapni4[cmdtcsnapni4$year %in% year_labels[2:6],]

cmdtcsnapni4 <- cmdtcsnapni4[cmdtcsnapni4$Nsource != "Non-recoverable Manure (national)",] # Remove non-recoverable manure for the next plots
#cmdtcsnapni4[cmdtcsnapni4$Nsource == "Non-recoverable Manure (cbw)",2] <- "Manure Management"
# First, create the proportion dataframes
# zz/(zz+zzz) = proportion available (to plant)
# zzz/(zz+zzz) = proportion non-available

prop_available <- zz / (zz + zzz)
prop_nonavailable <- zzz / (zz + zzz)



# Extract the manure fertilizer rows
manure_fert_rows <- cmdtcsnapni4[cmdtcsnapni4$Nsource == "Manure Fertilizer (cbw)", ]

# Create available rows
manure_available <- manure_fert_rows
manure_available$Nsource <- "Manure Fertilizer Available (cbw)"
manure_available$values <- manure_available$values * rep(c(0.25,0.18,0.17,0.35,0.35,0.26),5)

# Create non-available rows
manure_nonavailable <- manure_fert_rows
manure_nonavailable$Nsource <- "Manure Fertilizer Non-available (cbw)"
manure_nonavailable$values <- manure_nonavailable$values * rep(1-c(0.25,0.18,0.17,0.35,0.35,0.26),5)

# Remove original manure fertilizer rows and add the new split rows
cmdtcsnapni4 <- cmdtcsnapni4[cmdtcsnapni4$Nsource != "Manure Fertilizer (cbw)", ]
cmdtcsnapni4 <- rbind(cmdtcsnapni4, manure_available, manure_nonavailable)


#cmdtcsnapni4$Nsources <- sprintf("%s (%s)", cmdtcsnapni4$Nsource, cmdtcsnapni4$origin)

#convert 'position' to factor and specify level order
#cmdtcsnapni4$Nsource <- factor(cmdtcsnapni4$Nsource, levels=c("Manure Management", "N Fixation (national)", "N Fixation (cbw)", "Fertilizer (national)", "Manure Fertilizer (cbw)", "Inorganic Fertilizer (cbw)"))
#cmdtcsnapni4$Nsource <- factor(cmdtcsnapni4$Nsource, levels=c("Non-recoverable Manure (cbw)", "N Fixation (national)", "N Fixation (cbw)", "Fertilizer (national)", "Manure Fertilizer (cbw)", "Inorganic Fertilizer (cbw)"))
cmdtcsnapni4$Nsource <- factor(cmdtcsnapni4$Nsource, levels=c("Non-recoverable Manure (cbw)", "N Fixation (national)", "N Fixation (cbw)", "Fertilizer (national)", "Manure Fertilizer Available (cbw)","Manure Fertilizer Non-available (cbw)", "Inorganic Fertilizer (cbw)"))

cmdtcsnapni4$yearprotprod <- rep(c(totMprot[c(1,2,3,6,7,8),2:6]),7)
cmdtcsnapni4$totprodkgProt <- cmdtcsnapni4$values*cmdtcsnapni4$yearprotprod


# =============================================================================
# USAGE AT THE END OF YOUR SCENARIO CODE:
# =============================================================================
variable_list = list(
  cmdtcsnapni4 = cmdtcsnapni4,
  cmdtcsnapni3 = cmdtcsnapni3,
  CfertNinorgtotlrs = CfertNinorgtotlrs,
  CfertNinorgtot = CfertNinorgtot,
  CfixNwswE = CfixNwswE,
  Cfertmantot = Cfertmantot,
  Ckgws = Ckgws,
  CkgwswE = CkgwswE,
  CNkgwswE = CNkgwswE,
  byproductimported = byproductimported,
  feedN4meatdom = feedN4meatdom,
  totfeeddom4meat = totfeeddom4meat,
  totfeedimp4meat = totfeedimp4meat,
  feedN4meatimp = feedN4meatimp,
  feedN4anim = feedN4anim,
  C4humanN = C4humanN,
  kgmanureNlrs = kgmanureNlrs,
  meatN = meatN,
  kgmanureNrec450 = kgmanureNrec450,
  kgmanureNlrsrecovnonavailableplant = kgmanureNlrsrecovnonavailableplant,
  kgmanureNlrsavailableplantavailable = kgmanureNlrsavailableplantavailable,
  kgNmanuretoeachcrop = kgNmanuretoeachcrop,
  kgmanurenotavailabletoeachcrop = kgmanurenotavailabletoeachcrop, 
  manureNmeattot = manureNmeattot,
  manurecty = manurecty,
  manurews = manurews,
  manurewsrec = manurewsrec,
  manurectyrec = manurectyrec,
  NANIBtot = NANIBtot,
  TN = TN,
  TNperkm = TNperkm,
  TNcmdt = TNcmdt,
  NUE = NUE,
  unitfertNC = unitfertNC,
  unitfertinNC = unitfertinNC,
  unitfertmanNC = unitfertmanNC,
  cropprodcnty = cropprodcnty,
  cropareacnty = cropareacnty,
  cropareaws = cropareaws,
  cropprodws = cropprodws,
  Nfert = Nfert,
  croparea = croparea,
  grasscty = grasscty,
  grassws = grassws,
  wrcty = wrcty,
  wrws = wrws,
  manurebiogascty = manurebiogascty,
  manurebiogasws = manurebiogasws,
  grassbiogascty = grassbiogascty,
  wrbiogascty = wrbiogascty,
  CH4cnty = CH4cnty,
  CH4ws = CH4ws,
  kwcnty = kwcnty,
  kwws = kwws,
  NANIcropN = NANIcropN,
  NANIanimN = NANIanimN,
  hmnNreqs = hmnNreqs,
  NANIanimreq = NANIanimreq,
  CNprodremaining = CNprodremaining,
  cropNtoanim = cropNtoanim,
  cropNtoanimtotal = cropNtoanimtotal,
  cropkgtoanimtotal_N = cropkgtoanimtotal_N,
  NInputs_crops_lrs = NInputs_crops_lrs,
  NLoads_lrs = NLoads_lrs,
  NLoads_lrs_sum_kg = NLoads_lrs_sum_kg,
  NLoads_anim_kgdom = NLoads_anim_kgdom,
  NLoads_anim_kgimp = NLoads_anim_kgimp,
  NInputs_mannrcv_kg = NInputs_mannrcv_kg,
  NLoads_mannrcv_kgpermeat = NLoads_mannrcv_kgpermeat,
  Nloads_anim_kgpermeatcbw = Nloads_anim_kgpermeatcbw,
  Nloads_anim_kgperprotcbw = Nloads_anim_kgperprotcbw
  
  
  # Add more variables as needed:
  # another_variable = another_variable,
  # yet_another = yet_another
)
# At the end of your scenario runs, save the results:
save_scenario_results(
  scenario_name = CURRENT_SCENARIO,
  variable_list = variable_list
)
