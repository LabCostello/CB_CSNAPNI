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
    fixNperMprotnat[i,,n] = (fixNpermeatnat[i,,n] / (meatdata[i,z] / 1000))
  }}

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

create_animal_plot <- function(data, animal_label, title) {
  # Order data consistently
  data_ordered <- with(data, data[order(year, Nsources, model),])
  
  # Create plot for the specified animal
  ggplot(data=data_ordered[data_ordered$animal == animal_label,], aes(x=model, y=values)) + 
    geom_bar_pattern(
      aes(pattern = Nsources, fill = Nsources),
      stat = "identity",
      color = "black",  # Add black outline to bars
      pattern_fill = "black",
      pattern_density = 0.2,
      pattern_spacing = 0.037
    ) +
    scale_pattern_manual(values = c("stripe","circle", "circle", "none", "none")) +
    scale_fill_manual(values = c("white", "grey", "white", "black", "white")) +
    facet_grid(~year) +
    labs(title = title,
         x = "",
         y = "kg N input/kg protein") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          legend.title = element_text(color = "black", size = 16, face = "bold"),
          legend.text = element_text(color = "black", size = 14),
          axis.text = element_text(size = 14),
          axis.title.y = element_text(size = 16),
          strip.text.x = element_text(size = 12))
}



# Usage:
beefcomp <- create_animal_plot(cmdtcsnapni3, meat_labels[1], "Beef")
dairycomp <- create_animal_plot(cmdtcsnapni3, meat_labels[2], "Milk")
porkcomp <- create_animal_plot(cmdtcsnapni3, meat_labels[3], "Pork")
layerscomp <- create_animal_plot(cmdtcsnapni3, meat_labels[6], "Eggs")
broilcomp <- create_animal_plot(cmdtcsnapni3, meat_labels[7], "Chicken")
turkcomp <- create_animal_plot(cmdtcsnapni3, meat_labels[8], "Turkey")

library(ggpubr)

figureR1 <- ggarrange(beefcomp,dairycomp,porkcomp,layerscomp, broilcomp,turkcomp,ncol=2, nrow=3, common.legend = TRUE, legend="bottom")
figureR1
#####

cmdtcsnapni4 <- data.frame(
  "year" = c(rep(rep(year_labels, each = 9), 2),  # From cmdtcsnapni4
             rep(rep(year_labels, each = 9), 4)),  # From original cmdtcsnapni3b
  
  "Nsource" = c(rep(c("Fertilizer (national)", "N Fixation (national)"), each = 54),  # From cmdtcsnapni4
                rep(c("Inorganic Fertilizer (cbw)", "Manure Fertilizer (cbw)", "N Fixation (cbw)", "Non-recoverable Manure"), each = 54)),  # From original cmdtcsnapni3b
  
  "animal" = c(rep(meat_labels, 12),  # From cmdtcsnapni4
               rep(meat_labels, 24)),  # From original cmdtcsnapni3b
  
  "origin" = c(rep("national", 108),  # From cmdtcsnapni4
               rep("cbw", 216)),  # From original cmdtcsnapni3b
  
  "values" = c(
    # Values from cmdtcsnapni4 (national)
    rowSums(fertNperMprotnat[,,1]), rowSums(fertNperMprotnat[,,2]), rowSums(fertNperMprotnat[,,3]),
    rowSums(fertNperMprotnat[,,4]), rowSums(fertNperMprotnat[,,5]), rowSums(fertNperMprotnat[,,6]),
    rowSums(fixNperMprotnat[,,1]), rowSums(fixNperMprotnat[,,2]), rowSums(fixNperMprotnat[,,3]),
    rowSums(fixNperMprotnat[,,4]), rowSums(fixNperMprotnat[,,5]), rowSums(fixNperMprotnat[,,6]),
    
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

#cmdtcsnapni4$Nsources <- sprintf("%s (%s)", cmdtcsnapni4$Nsource, cmdtcsnapni4$origin)

#convert 'position' to factor and specify level order
cmdtcsnapni4$Nsource <- factor(cmdtcsnapni4$Nsource, levels=c("Non-recoverable Manure", "N Fixation (national)", "N Fixation (cbw)", "Fertilizer (national)", "Manure Fertilizer (cbw)", "Inorganic Fertilizer (cbw)"))

  create_animal_plot2 <- function(data, animal_label, title) {
    # Order data consistently
    data_ordered <- with(data, data[order(year, Nsource),])
    
    # Create plot for the specified animal
    ggplot(data=data_ordered[data_ordered$animal == animal_label,], aes(x=animal,y=values)) + 
      geom_bar_pattern(
        aes(pattern = Nsource, fill = Nsource),
        stat = "identity",
        color = "black",  # Add black outline to bars
        pattern_fill = "black",
        pattern_density = 0.2,
        pattern_spacing = 0.037
      ) +
      scale_x_discrete(labels = NULL) + 
      scale_pattern_manual(values = c("stripe","circle", "circle", "none", "none", "none")) +
      scale_fill_manual(values = c("white", "grey", "white", "black", "grey", "white")) +
      facet_grid(~year) +
      labs(title = title,
           x = "",
           y = "kg N input/kg protein") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, hjust = 0.5),
            legend.title = element_text(color = "black", size = 16, face = "bold"),
            legend.text = element_text(color = "black", size = 14),
            axis.text = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            strip.text.x = element_text(size = 12))
  }



# Usage:
beefcomp2 <- create_animal_plot2(cmdtcsnapni4, meat_labels[1], "Beef")
dairycomp2 <- create_animal_plot2(cmdtcsnapni4, meat_labels[2], "Milk")
porkcomp2 <- create_animal_plot2(cmdtcsnapni4, meat_labels[3], "Pork")
layerscomp2 <- create_animal_plot2(cmdtcsnapni4, meat_labels[6], "Eggs")
broilcomp2 <- create_animal_plot2(cmdtcsnapni4, meat_labels[7], "Chicken")
turkcomp2 <- create_animal_plot2(cmdtcsnapni4, meat_labels[8], "Turkey")

figureR2 <- ggarrange(beefcomp2,dairycomp2,porkcomp2,layerscomp2, broilcomp2,turkcomp2,ncol=2, nrow=3, common.legend = TRUE, legend="bottom")
figureR2


create_animal_plot3 <- function(data, animal_label, title) {
  # Order data consistently
  data_ordered <- with(data, data[order(year, Nsource),])
  
  # Create plot for the specified animal
  ggplot(data=data_ordered[data_ordered$animal == animal_label,], aes(x=animal, y=values)) + 
    geom_bar(
      aes(fill = Nsource),
      stat = "identity",
      color = "black",  # Add black outline to bars
      position = "stack"
    ) +
    scale_x_discrete(labels = NULL) + 
    scale_fill_manual(values = c(
      "Fertilizer (national)" = "#780000",       # Dark blue
      "Fertilizer Inorganic (cbw)" = "#C1121F",  # Light blue
      "Fertilizer Manure (cbw)" = "#CD3E43",     # Medium blue
      "Fixation (national)" = "#003049",         # Deep purple
      "Fixation (cbw)" = "#669BBC",              # Light purple
      "Non-recoverable Manure" = "#FDF0D5"            # Tiffany blue
    )) +
    facet_grid(~year) +
    labs(title = title,
         x = "",
         y = "kg N input/kg protein") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          legend.title = element_text(color = "black", size = 16, face = "bold"),
          legend.text = element_text(color = "black", size = 14),
          axis.text = element_text(size = 14),
          axis.title.y = element_text(size = 16),
          strip.text.x = element_text(size = 12))
}

create_animal_plot3 <- function(data, animal_label, title) {
  # Order data consistently
  data_ordered <- with(data, data[order(year, Nsource),])
  
  # Filter data for the specified animal
  animal_data <- data_ordered[data_ordered$animal == animal_label,]
  
  # Calculate totals for each year
  totals <- aggregate(values ~ year, data = animal_data, FUN = sum)
  
  # Create plot for the specified animal
  ggplot(data=animal_data, aes(x=animal, y=values)) + 
    geom_bar(
      aes(fill = Nsource),
      stat = "identity",
      color = "black",  # Add black outline to bars
      position = "stack",
      width = 0.7      # Slightly narrower bars for better appearance
    ) +
    # Add total values as text on top of each bar
    geom_text(
      data = totals,
      aes(x = 1, y = values, label = format(values, digits = 3)),
      position = position_stack(vjust = 1.05),
      size = 4.5,
      fontface = "bold"  # Make totals bold for better visibility
    ) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1), expand = expansion(mult = c(0, 0.15))) +  # Add more space at top for labels
    scale_x_discrete(labels = NULL) + 
    scale_fill_manual(values = c(
      "Fertilizer (national)" = "#780000",
      "Inorganic Fertilizer (cbw)" = "#C1121F",
      "Manure Fertilizer (cbw)" = "#CD3E43",
      "N Fixation (national)" = "#003049",
      "N Fixation (cbw)" = "#669BBC",
      "Non-recoverable Manure" = "#FDF0D5"
    )) +
    facet_grid(~year) +
    labs(title = title,
         x = "",
         y = "kg N input/kg protein") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold", margin = margin(b = 12)),
      plot.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(color = "black", size = 18, face = "bold"),
      legend.text = element_text(color = "black", size = 16),
      axis.text = element_text(size = 16),
      axis.title.y = element_text(size = 18, face = "bold", margin = margin(r = 12)),
      strip.text.x = element_text(size = 16, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = "gray80"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),  # Remove minor grid lines for cleaner look
      panel.spacing = unit(1.5, "lines"),  # Add more space between facets
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
}


beefcomp3 <- create_animal_plot3(cmdtcsnapni4, meat_labels[1], "Beef")
dairycomp3 <- create_animal_plot3(cmdtcsnapni4, meat_labels[2], "Milk")
porkcomp3 <- create_animal_plot3(cmdtcsnapni4, meat_labels[3], "Pork")
layerscomp3 <- create_animal_plot3(cmdtcsnapni4, meat_labels[6], "Eggs")
broilcomp3 <- create_animal_plot3(cmdtcsnapni4, meat_labels[7], "Chicken")
turkcomp3 <- create_animal_plot3(cmdtcsnapni4, meat_labels[8], "Turkey")

figureR3 <- ggarrange(beefcomp3,dairycomp3,porkcomp3,layerscomp3, broilcomp3,turkcomp3,ncol=3, nrow=2, common.legend = TRUE, legend="bottom")
figureR3

# Export for poster
ggsave("figureR3_poster2.png", figureR3, 
       width = 41.38, height = 25.86, units = "cm", dpi = 600)

cmdtcsnapni4$yearprotprod <- rep(c(totMprot[c(1,2,3,6,7,8),2:6]),6)
cmdtcsnapni4$totprodkgProt <- cmdtcsnapni4$values*cmdtcsnapni4$yearprotprod

create_animal_plot4 <- function(data, animal_label, title) {
  # Order data consistently
  data_ordered <- with(data, data[order(year, Nsource),])
  
  # Filter data for the specified animal
  animal_data <- data_ordered[data_ordered$animal == animal_label,]
  
  # Calculate totals for each year
  totals <- aggregate(totprodkgProt ~ year, data = animal_data, FUN = sum)
  
  # Create plot for the specified animal
  ggplot(data=animal_data, aes(x=animal, y=totprodkgProt)) + 
    geom_bar(
      aes(fill = Nsource),
      stat = "identity",
      color = "black",  # Add black outline to bars
      position = "stack",
      width = 0.7      # Slightly narrower bars for better appearance
    ) +
    # Add total values as text on top of each bar
    geom_text(
      data = totals,
      aes(x = 1, y = totprodkgProt, label = format(totprodkgProt, scientific = TRUE, digits = 2)),
      position = position_stack(vjust = 1.05),
      size = 3.9,
      fontface = "bold"  # Make totals bold for better visibility
    ) +
    scale_y_continuous(
      labels = function(x) format(x, scientific = TRUE),
      expand = expansion(mult = c(0, 0.15))  # Add more space at top for labels
    ) +
    scale_x_discrete(labels = NULL) + 
    scale_fill_manual(values = c(
      "Fertilizer (national)" = "#780000",
      "Inorganic Fertilizer (cbw)" = "#C1121F",
      "Manure Fertilizer (cbw)" = "#CD3E43",
      "N Fixation (national)" = "#003049",
      "N Fixation (cbw)" = "#669BBC",
      "Non-recoverable Manure" = "#FDF0D5"
    )) +
    facet_grid(~year) +
    labs(title = title,
         x = "",
         y = "Anthropogenic nitrogen (kg N)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold", margin = margin(b = 12)),
      plot.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(color = "black", size = 18, face = "bold"),
      legend.text = element_text(color = "black", size = 16),
      axis.text = element_text(size = 16),
      axis.title.y = element_text(size = 18, face = "bold", margin = margin(r = 12)),
      strip.text.x = element_text(size = 16, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = "gray80"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),  # Remove minor grid lines for cleaner look
      panel.spacing = unit(1.5, "lines"),  # Add more space between facets
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
}

#create_animal_plot4 <- function(data, animal_label, title) {
  # Order data consistently
  data_ordered <- with(data, data[order(year, Nsource),])
  
  # Filter data for the specified animal
  animal_data <- data_ordered[data_ordered$animal == animal_label,]
  
  # Calculate totals for each year
  totals <- aggregate(totprodkgProt ~ year, data = animal_data, FUN = sum)
  
  # Create plot for the specified animal
  ggplot(data=animal_data, aes(x=animal, y=totprodkgProt)) + 
    geom_bar_pattern(
      aes(pattern = Nsource, fill = Nsource),
      stat = "identity",
      color = "black",  # Add black outline to bars
      pattern_fill = "black",
      pattern_density = 0.2,
      pattern_spacing = 0.037,
      position = "stack",
      width = 0.7      # Slightly narrower bars for better appearance
    ) +
    scale_y_continuous(
      labels = function(x) format(x, scientific = TRUE),
      expand = expansion(mult = c(0, 0.15))  # Add more space at top for labels
    ) +
    scale_x_discrete(labels = NULL) + 
    scale_pattern_manual(values = c(
      "Fertilizer (national)" = "none",
      "Inorganic Fertilizer (cbw)" = "none",
      "Manure Fertilizer (cbw)" = "none",
      "N Fixation (national)" = "circle",
      "N Fixation (cbw)" = "circle",
      "Non-recoverable Manure" = "stripe"
    )) +
    scale_fill_manual(values = c(
      "Fertilizer (national)" = "black",
      "Synthetic Fertilizer (cbw)" = "white",
      "Manure Fertilizer (cbw)" = "grey",
      "N Fixation (national)" = "grey",
      "N Fixation (cbw)" = "white",
      "Non-recoverable Manure" = "white"
    )) +
    facet_grid(~year) +
    labs(title = title,
         x = "",
         y = "kg of N emission") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold", margin = margin(b = 12)),
      plot.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(color = "black", size = 18, face = "bold"),
      legend.text = element_text(color = "black", size = 16),
      axis.text = element_text(size = 16),
      axis.title.y = element_text(size = 18, face = "bold", margin = margin(r = 12)),
      strip.text.x = element_text(size = 16, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = "gray80"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),  # Remove minor grid lines for cleaner look
      panel.spacing = unit(1.5, "lines"),  # Add more space between facets
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
}

beefcomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[1], "Beef")
dairycomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[2], "Milk")
porkcomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[3], "Pork")
layerscomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[6], "Eggs")
broilcomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[7], "Chicken")
turkcomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[8], "Turkey")

figureR4 <- ggarrange(beefcomp4,dairycomp4,porkcomp4,layerscomp4, broilcomp4,turkcomp4,ncol=3, nrow=2, common.legend = TRUE, legend="bottom")
figureR4
# For a high-quality PNG
#ggsave("figureR4_poster.png", figureR4, width = 10, height = 12, units = "in", dpi = 300)
# Export for poster
ggsave("figureR4_poster2.png", figureR4, 
       width = 41.38, height = 25.86, units = "cm", dpi = 600)

create_animal_plot_by_year <- function(data, year_value, title) {
  # Order data consistently
  data_ordered <- with(data, data[order(animal, Nsource),])
  
  # Filter data for the specified year
  year_data <- data_ordered[data_ordered$year == year_value,]
  
  # Create plot for the specified year with all animals
  ggplot(data=year_data, aes(x=animal, y=totprodkgProt)) + 
    geom_bar(
      aes(fill = Nsource),
      stat = "identity",
      color = "black",  # Add black outline to bars
      position = "stack"
    ) +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +  # Format y-axis labels scientifically
    scale_fill_manual(values = c(
      "Fertilizer (national)" = "#780000",       
      "Fertilizer Inorganic (cbw)" = "#C1121F",  
      "Fertilizer Manure (cbw)" = "#CD3E43",     
      "Fixation (national)" = "#003049",         
      "Fixation (cbw)" = "#669BBC",              
      "Non-recoverable Manure" = "#FDF0D5"            
    )) +
    labs(title = paste(title, year_value),
         x = "",
         y = "kg N input") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          legend.title = element_text(color = "black", size = 16, face = "bold"),
          legend.text = element_text(color = "black", size = 14),
          axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Angled labels for better readability
          axis.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 16))
}

# Get unique years from your dataset
unique_years <- unique(cmdtcsnapni4$year)

# Create a plot for each year
year_plot1 <- create_animal_plot_by_year(cmdtcsnapni4, unique_years[1], "Animal Comparison -")
year_plot2 <- create_animal_plot_by_year(cmdtcsnapni4, unique_years[2], "Animal Comparison -")
year_plot3 <- create_animal_plot_by_year(cmdtcsnapni4, unique_years[3], "Animal Comparison -")
year_plot4 <- create_animal_plot_by_year(cmdtcsnapni4, unique_years[4], "Animal Comparison -")

# Arrange all plots in a 2x2 grid with common legend
figureR4_revised <- ggarrange(year_plot1, year_plot2, year_plot3, year_plot4, 
                              ncol=2, nrow=2, 
                              common.legend = TRUE, 
                              legend="bottom")

# Display the final arrangement
figureR4_revised


create_animal_plot4 <- function(data, animal_label, title, percent_change_data) {
  # Order data consistently
  data_ordered <- with(data, data[order(year, Nsource),])
  
  # Filter data for the specified animal
  animal_data <- data_ordered[data_ordered$animal == animal_label,]
  
  # Calculate totals for each year
  totals <- aggregate(totprodkgProt ~ year, data = animal_data, FUN = sum)
  
  # Filter percentage change data for the specified animal
  animal_percent <- percent_change_data[percent_change_data$animal == animal_label,]
  
  # Calculate scaling factors for secondary axis
  max_totprodkgProt <- max(totals$totprodkgProt)
  max_percent <- max(abs(animal_percent$percent_change)) * 1.2  # Add 20% for padding
  scaling_factor <- max_totprodkgProt / max_percent
  
  # Create plot for the specified animal
  ggplot(data=animal_data, aes(x=animal, y=totprodkgProt)) + 
    geom_bar_pattern(
      aes(pattern = Nsource, fill = Nsource),
      stat = "identity",
      color = "black",  # Add black outline to bars
      pattern_fill = "black",
      pattern_density = 0.1,
      pattern_spacing = 0.04,
      position = "stack",
      width = 0.7      # Slightly narrower bars for better appearance
    ) +
   # Add points to the percentage change line
    geom_point(
      data = animal_percent,
      aes(x = 1, y = percent_change * scaling_factor),
      color = "red",
      size = 3,
      shape = 18
    ) +
    # Add percentage values as text
    geom_text(
      data = animal_percent,
      aes(x = 1, y = percent_change * scaling_factor, 
          label = sprintf("%+.1f%%", percent_change)),
      color = "red",
      vjust = -0.8,
      size = 3.5,
      fontface = "bold"
    ) +
    scale_y_continuous(
      name = "kg of N emission",
      labels = function(x) format(x, scientific = TRUE),
      expand = expansion(mult = c(0, 0.2)),  # Add more space at top for labels
      sec.axis = sec_axis(
        ~./scaling_factor, 
        name = "% Change in Population (Base 2002)",
        labels = function(x) paste0(x, "%")
      )
    ) +
    scale_x_discrete(labels = NULL) + 
    scale_pattern_manual(values = c("stripe","circle", "circle", "none", "none", "none")) +
    scale_fill_manual(values = c("white", "grey", "white", "black", "grey", "white")) +
    facet_grid(~year) +
    labs(title = title,
         x = "") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold", margin = margin(b = 12)),
      plot.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(color = "black", size = 18, face = "bold"),
      legend.text = element_text(color = "black", size = 16),
      axis.text = element_text(size = 16),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 12)),
      axis.title.y.right = element_text(size = 18, face = "bold", color = "black", margin = margin(l = 12)),
      axis.text.y.right = element_text(color = "black"),
      strip.text.x = element_text(size = 16, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = "gray80"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),  # Remove minor grid lines for cleaner look
      panel.spacing = unit(1.5, "lines"),  # Add more space between facets
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
}

# First, calculate the percentage change data using 2002 as baseline
animpoptotal_percent <- animpoptotal_structured %>%
  group_by(animal) %>%
  filter(year>=2002) %>% 
  mutate(
    baseline = population[year == 2002],  # Get the 2002 value for each animal
    percent_change = (population - baseline) / baseline * 100  # Calculate % change
  ) %>%
  ungroup()

# Now create your plots with the updated function
beefcomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[1], "Beef", animpoptotal_percent)
dairycomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[2], "Milk", animpoptotal_percent)
porkcomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[3], "Pork", animpoptotal_percent)
layerscomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[6], "Eggs", animpoptotal_percent)
broilcomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[7], "Chicken", animpoptotal_percent)
turkcomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[8], "Turkey", animpoptotal_percent)

# Create the combined figure
figureR4 <- ggarrange(beefcomp4, dairycomp4, porkcomp4, layerscomp4, broilcomp4, turkcomp4, 
                      ncol=3, nrow=2, common.legend = TRUE, legend="bottom")
figureR4


