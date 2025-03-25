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
fertNpermeatdom <- fertNpermeatnat <- fertNperMprotdom <- fertNperMprotnat <- array(0,c(9,21,6))
fixNpermeatdom <- fixNpermeatnat <- fixNperMprotdom <- fixNperMprotnat <-  array(0,c(9,21,6))

for(n in 1:nyrs){
  for(i in 1:21){
    fertNpermeatdom[,i,n] <- (feedpermeatdom[,i,n] * unitfertNC[i,n]) 
    fertNpermeatnat[,i,n] <- (feedpermeatimp[,i,n] * unitfertNCnational[i,n])
    fixNpermeatdom[,i,n] <- (feedpermeatdom[,i,n] * unitfixNC[i,n]) 
    fixNpermeatnat[,i,n] <- (feedpermeatimp[,i,n] * unitfixNC[i,n])}}

for(n in 1:nyrs){
  for(i in 1:n_meats){
    fertNperMprotdom[i,,n] = (fertNpermeatdom[i,,n] / (meatdata[i,z] / 1000))
    fertNperMprotnat[i,,n] = (fertNpermeatnat[i,,n] / (meatdata[i,z] / 1000))# kg N fert / kg protein in 1 kg meat
    fixNperMprotdom[i,,n] = (fixNpermeatdom[i,,n] / (meatdata[i,z] / 1000))
    fixNperMprotnat[i,,n] = (fixNpermeatnat[i,,n] / (meatdata[i,z] / 1000))
  }}
library(readxl)
cmdtcsnapninat<- read_excel(adjust_file_path("C:/Users/lds5498/OneDrive - The Pennsylvania State University/Desktop/Code/important_excel/cmdtcsnapni3nat.xlsx"))

cmdtcsnapni3a <- data.frame("year"=rep(rep(year_labels[1:5],each = 9),2), 
                            "Nsource" = rep(c("Fertilizer", "Fixation"),each=45),
                            "model"=rep("cbw-csnani",90),
                            "animal"=rep(meat_labels,10),
                            "origin"=rep("national",90),
                            "values" = c(rowSums(fertNperMprotnat[,,1]),rowSums(fertNperMprotnat[,,2]),rowSums(fertNperMprotnat[,,3]),
                                         rowSums(fertNperMprotnat[,,4]),rowSums(fertNperMprotnat[,,5]),
                                         rowSums(fixNperMprotnat[,,1]),rowSums(fixNperMprotnat[,,2]),rowSums(fixNperMprotnat[,,3]),
                                         rowSums(fixNperMprotnat[,,4]),rowSums(fixNperMprotnat[,,5])))

cmdtcsnapni3a <- rbind(cmdtcsnapninat,cmdtcsnapni3a)

cmdtcsnapni3b <- data.frame("year" = rep(rep(year_labels[1:5],each = 9),2), 
                            "Nsource" = rep(c("Fertilizer", "Fixation"),each=45),
                            "model"=rep("cbw-csnani",90),
                            "animal"=rep(meat_labels,10),
                            "origin"=rep("cbw",90),
                            "values" = c(rowSums(fertNperMprotdom[,,1]),rowSums(fertNperMprotdom[,,2]),rowSums(fertNperMprotdom[,,3]),
                                         rowSums(fertNperMprotdom[,,4]),rowSums(fertNperMprotdom[,,5]),rowSums(fixNperMprotdom[,,1]),rowSums(fixNperMprotdom[,,2]),rowSums(fixNperMprotdom[,,3]),
                                         rowSums(fixNperMprotdom[,,4]),rowSums(fixNperMprotdom[,,5])))

cmdtcsnapni3 <- rbind(cmdtcsnapni3a,cmdtcsnapni3b)
cmdtcsnapni3 <- cmdtcsnapni3[cmdtcsnapni3$animal %in% meat_labels[c(1,2,3,6,7,8)],]

cmdtcsnapni3 <- cmdtcsnapni3[cmdtcsnapni3$year %in% year_labels[2:6],]

cmdtcsnapni3$Nsources <- sprintf("%s (%s)", cmdtcsnapni3$Nsource, cmdtcsnapni3$origin)

library(ggplot2)
library(ggpubr)
library(ggpattern)

cmdtcsnapni3$model[cmdtcsnapni3$model == "csnapni"] <- "national"
cmdtcsnapni3$model[cmdtcsnapni3$model == "cbw-csnani"] <- "CBW"

# Beef
data2<-with(cmdtcsnapni3, cmdtcsnapni3[order(year, Nsources, model),])
beefcomp <- ggplot(data=data2[data2$animal == meat_labels[1],], aes(x=model, y=values)) + 
  geom_bar_pattern(
    aes(pattern = Nsources, fill = Nsources),
    stat = "identity",
    color = "black",  # Add black outline to bars
    pattern_fill = "black",
    pattern_density = 0.2,
    pattern_spacing = 0.037
  ) +
  scale_pattern_manual(values = c("none", "none", "circle", "stripe")) +
  scale_fill_manual(values = c("white", "black", "white", "white")) +
  facet_grid(~year) +
  labs(title = "Beef",
       x = "",
       y = "kg N input/kg protein") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16,hjust = 0.5),legend.title = element_text(color = "black", size = 16, face = "bold"),  # Adjust title appearance
        legend.text = element_text(color = "black", size = 14),axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16),strip.text.x = element_text(size = 12))

# Dairy
data2<-with(cmdtcsnapni3, cmdtcsnapni3[order(year, Nsources, model),])
dairycomp <- ggplot(data=data2[data2$animal == meat_labels[2],], aes(x=model, y=values)) + 
  geom_bar_pattern(
    aes(pattern = Nsources, fill = Nsources),
    stat = "identity",
    color = "black",  # Add black outline to bars
    pattern_fill = "black",
    pattern_density = 0.2,
    pattern_spacing = 0.037
  ) +
  scale_pattern_manual(values = c("none", "none", "circle", "stripe")) +
  scale_fill_manual(values = c("white", "black", "white", "white")) +
  facet_grid(~year) +
  labs(title = "Milk",
       x = "",
       y = "kg N input/kg protein") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16,hjust = 0.5),legend.title = element_text(color = "black", size = 16, face = "bold"),  # Adjust title appearance
        legend.text = element_text(color = "black", size = 14),axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16),strip.text.x = element_text(size = 12))

# Pork
data2<-with(cmdtcsnapni3, cmdtcsnapni3[order(year, Nsources, model),])
porkcomp <- ggplot(data=data2[data2$animal == meat_labels[3],], aes(x=model, y=values)) + 
  geom_bar_pattern(
    aes(pattern = Nsources, fill = Nsources),
    stat = "identity",
    color = "black",  # Add black outline to bars
    pattern_fill = "black",
    pattern_density = 0.2,
    pattern_spacing = 0.037
  ) +
  scale_pattern_manual(values = c("none", "none", "circle", "stripe")) +
  scale_fill_manual(values = c("white", "black", "white", "white")) +
  facet_grid(~year) +
  labs(title = "Pork",
       x = "",
       y = "kg N input/kg protein") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16,hjust = 0.5),legend.title = element_text(color = "black", size = 16, face = "bold"),  # Adjust title appearance
        legend.text = element_text(color = "black", size = 14),axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16),strip.text.x = element_text(size = 12))

# Layers
data2<-with(cmdtcsnapni3, cmdtcsnapni3[order(year, Nsources, model),])
layerscomp <- ggplot(data=data2[data2$animal == meat_labels[6],], aes(x=model, y=values)) + 
  geom_bar_pattern(
    aes(pattern = Nsources, fill = Nsources),
    stat = "identity",
    color = "black",  # Add black outline to bars
    pattern_fill = "black",
    pattern_density = 0.2,
    pattern_spacing = 0.037
  ) +
  scale_pattern_manual(values = c("none", "none", "circle", "stripe")) +
  scale_fill_manual(values = c("white", "black", "white", "white")) +
  facet_grid(~year) +
  labs(title = "Eggs",
       x = "",
       y = "kg N input/kg protein") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16,hjust = 0.5),legend.title = element_text(color = "black", size = 16, face = "bold"),  # Adjust title appearance
        legend.text = element_text(color = "black", size = 14),axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16),strip.text.x = element_text(size = 12))

# Chicken
data2<-with(cmdtcsnapni3, cmdtcsnapni3[order(year, Nsources, model),])
broilcomp <- ggplot(data=data2[data2$animal == meat_labels[7],], aes(x=model, y=values)) + 
  geom_bar_pattern(
    aes(pattern = Nsources, fill = Nsources),
    stat = "identity",
    color = "black",  # Add black outline to bars
    pattern_fill = "black",
    pattern_density = 0.2,
    pattern_spacing = 0.037
  ) +
  scale_pattern_manual(values = c("none", "none", "circle", "stripe")) +
  scale_fill_manual(values = c("white", "black", "white", "white")) +
  facet_grid(~year) +
  labs(title = "Chicken",
       x = "",
       y = "kg N input/kg protein") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16,hjust = 0.5),legend.title = element_text(color = "black", size = 16, face = "bold"),  # Adjust title appearance
        legend.text = element_text(color = "black", size = 14),axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16),strip.text.x = element_text(size = 12))

# Turkey
data2<-with(cmdtcsnapni3, cmdtcsnapni3[order(year, Nsources, model),])
turkcomp <- ggplot(data=data2[data2$animal == meat_labels[8],], aes(x=model, y=values)) + 
  geom_bar_pattern(
    aes(pattern = Nsources, fill = Nsources),
    stat = "identity",
    color = "black",  # Add black outline to bars
    pattern_fill = "black",
    pattern_density = 0.2,
    pattern_spacing = 0.037
  ) +
  scale_pattern_manual(values = c("none", "none", "circle", "stripe")) +
  scale_fill_manual(values = c("white", "black", "white", "white")) +
  facet_grid(~year) +
  labs(title = "Turkey",
       x = "",
       y = "kg N input/kg protein") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16,hjust = 0.5),legend.title = element_text(color = "black", size = 16, face = "bold"),  # Adjust title appearance
        legend.text = element_text(color = "black", size = 14),axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16),strip.text.x = element_text(size = 12))

library(ggpubr)

figureR1 <- ggarrange(beefcomp,dairycomp,porkcomp,layerscomp, broilcomp,turkcomp,ncol=2, nrow=3, common.legend = TRUE, legend="bottom")


