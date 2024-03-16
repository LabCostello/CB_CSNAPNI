if(print_tags == 1){
  print("CreateInputsSubs_CBW/grass_scenario.R")
}

grass_dry_matter <- (1-0.0816) # Not in use, because the yield provided above is in dry matter already
biogascnty <- array(0,c(n_cnty,length(import_yrs))) 
biogasws <- array(0,c(n_ws_NEEA,length(import_yrs))) 

# Area Harvested

cropareacty_old = array(0,c(n_cnty,n_crops,length(import_yrs)))
cropareacty = array(0,c(n_cnty,n_crops,length(import_yrs)))
cropareacty[,1:(n_crops-3),] = areas_array #crop harvested areas, in km2
cropareaws=array(0,c(n_ws_tbx,n_crops,length(import_yrs)))
cornareanoetoh=array(0,c(n_cnty,length(import_yrs)))
yr_col=array(0,c(length(import_yrs),1))
for(n in 1:length(import_yrs)){
  # calc amounts of etoh coproducts
  cropareacty[,(n_crops-2),n]=cornuse[5,n]*cropareacty[,1,n]*to_FC_wetmill[alloc_method]*wetmill_CGF
  cropareacty[,(n_crops-1),n]=cornuse[5,n]*cropareacty[,1,n]*to_FC_wetmill[alloc_method]*wetmill_CGM
  # calc new corn area
  cornareanoetoh[,n] = drop(cropareacty[,1,n])
  cropareacty[,1,n] = cornareanoetoh[,n]*(1-(cornuse[5,n])) #proportion of corn not allocated to fuel ethanol production
  #solve NaN issue
  for(m in 1:(n_crops-3)){
    ind=which(is.na(cropareacty[,m,n]) %in% 1)
    if(length(ind)>0){
      for(i in 1:length(ind)){
        cropareacty[ind[i],m,n]=0
      }
    }
  }
  
  #cropareaws[,,n]=t(cnty_ws)%*%cropareacty[,,n]
  dummy <- merge(as.data.frame(lrs_cdl_percents[n],check.names=FALSE),cbind(FIPS,cropareacty[,,n]))
  colnames(dummy)[17:36] <- cropareaharvested_key[-1,1]
  cropws <- data.frame("FIPS"=dummy$FIPS,
                       "LNDRVRSEG"=dummy$LNDRVRSEG,
                       "OBJECTID"=dummy$OBJECTID,
                       "REGION"=dummy$REGION,
                       "Corn.grain" = dummy$Corn*dummy$`corn for grain`*(1-(land_use_grass*0.4)),
                       "Corn.silage" = dummy$Corn*dummy$`corn for silage`*(1-(land_use_grass*0.4)),
                       "Wheat" = dummy$Wheat*dummy$wheat,
                       "Oats" = dummy$Oats*dummy$oats,
                       "Barley" = dummy$Barley*dummy$barley,
                       "Sorghum.grain" = dummy$Sorghum*dummy$`sorghum for grain`,
                       "Sorghum.grain" = dummy$Sorghum*dummy$`sorghum for silage`,
                       "Potatoes" = dummy$Potatoes*dummy$potatoes,
                       "Rye" = dummy$Rye*dummy$rye,
                       "Alfalfa" = dummy$Alfalfa*dummy$`alfalfa hay`,
                       "Other Hay/Non Alfalfa" = dummy$Other.Hay.Non.Alfalfa*dummy$`other hay`,
                       "Soybeans" = dummy$Soybeans*dummy$soybeans*(1-(land_use_grass*0.2)),
                       "Cropland pasture" = dummy$Grass.Pasture*dummy$`cropland pasture`,
                       "Noncropland pasture" = dummy$Grass.Pasture*dummy$`noncropland pasture`,
                       "Rice" = 0,
                       "Peanuts" = dummy$Peanuts*dummy$grass,
#                       "Grass" = ((0.5*dummy$Corn*dummy$V2)+(0.5*dummy$Corn*dummy$V3)+(0.25*dummy$Soybeans*dummy$V13))*land_use_grass, # CG-CG-S-CS-CS rotation
                       "Grass" = ((dummy[,Crotation[1]]*dummy[,Crotation[2]])+(dummy[,Crotation[3]]*dummy[,Crotation[4]])+(dummy[,Crotation[5]]*dummy[,Crotation[6]])+(dummy[,Crotation[7]]*dummy[,Crotation[8]])+(dummy[,Crotation[9]]*dummy[,Crotation[10]]))*0.2*land_use_grass, # CG-CG-S-CS-CS rotation
                       "CGM" = dummy$Corn*dummy$CGF,
                       "CGF" = dummy$Corn*dummy$CGM,
                       "DGS" = dummy$Corn*dummy$DGS)
  cropws <- cropws[cropws$REGION=="Chesapeake Bay Watershed",]
  
  cropws <- cropws %>% arrange(FIPS,LNDRVRSEG)
  
  cropareacty_old[,,n] <- cropareacty[,,n]
  
  cropareacty[,,n] <- as.matrix(subset(aggregate(cropws[,5:24], by=list(cropws$FIPS), FUN = sum),select=-1)) # Readjusting county information to only info inside CBW
  
  cropareaws[,,n] <- as.matrix(subset(cropws,select=-c(1:4)))
  
  write_name = paste("InputFiles_CBW/cropareaharvestedcnty",run_yrs[n],".txt",sep = "")
  write.table(cropareacty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
}
croparea=colSums(cropareaws)
cornareanoetohsum=colSums(cornareanoetoh)
#etoh_landuse = cornuse[5,]*cornareanoetohsum*(1-to_FC_wetmill[alloc_method])+cornuse[6,]*cornareanoetohsum*(1-to_FC_drymill[alloc_method]) #ag land use for etoh
etoh_landuse <- c(0,0,0,0,0) # There is no land use to produce ethanol in the region. The corn that is produced for etoh is from outside of the CBW (assumption)

#write files
write_name = paste("InputFiles_CBW/corntotareaharvested.txt")
write.table(cornareanoetohsum, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
write_name = paste("InputFiles_CBW/cropareaharvested.txt")
write.table(croparea, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
write_name = paste("InputFiles_CBW/etoh_landuse_harvestedarea.txt")
write.table(etoh_landuse, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

#write key
write_name = paste("InputFileKeys/cropareaharvested_key.txt")
cropareaharvested_key = array(" ", c(n_crops+1,length(import_yrs)+1))
cropareaharvested_key[1,] = c(" ", import_yrs) #column headings
cropareaharvested_key[,1]=c("crop", cropname) #row headings
write.table(cropareaharvested_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

#Production

for(n in 1:(length(import_yrs))){ 
  # build a matrix of extracted data
  for(j in 1:n_cnty){ #rows (counties)
    # calc amounts of etoh coproducts
    cropprodcnty[j,(n_crops-2),n] = cornuse[5,n]*cropprodcnty[j,1,n]*CGF_from_corn #CGF (the CGF produced by corn reported by the USDA in "alcohol for fuel" that is not DDGS)
    cropprodcnty[j,(n_crops-1),n] = cornuse[5,n]*cropprodcnty[j,1,n]*CGM_from_corn #CGM (the CGM produced by corn reported by the USDA in "alcohol for fuel" that is not DDGS)
    #cropprodcnty[j,n_crops,n] = cornuse[6,n]*cropprodcnty[j,1,n]*DGS_from_corn #DGS (the DDGS produced by corn from ethanol plants, as reported by the USDA)
    #etohprodcnty[j,n] = (cornuse[5,n]+cornuse[6,n])*cropprodcnty[j,1,n]*etoh_from_corn[n] #liters of etoh from corn for etoh, assumption that every county contributes equally to corn for ethanol
    
    # calc new corn total
    cornprodnoetoh[j,n] = drop(cropprodcnty[j,1,n])
    cropprodcnty[j,1,n] = cornprodnoetoh[j,n]*(1-(cornuse[5,n])) #proportion of corn not allocated to fuel ethanol production
    #pastures: "take half, leave half"
    cropprodcnty[j,13:14,n] = cropprodcnty[j,13:14,n]/2
  }
  
  # watershed crop production
  cropprodws[,,n] = t(cnty_ws)%*%cropprodcnty[,,n]
  etohprodws[,n] = t(cnty_ws)%*%etohprodcnty[,n]
  
  dummy <- merge(as.data.frame(lrs_cdl_percents[n],check.names=FALSE),cbind(FIPS,cropprodcnty[,,n]))
  cropws <- data.frame("FIPS"=dummy$FIPS,
                       "LNDRVRSEG"=dummy$LNDRVRSEG,
                       "OBJECTID"=dummy$OBJECTID,
                       "REGION"=dummy$REGION,
                       "Corn.grain" = dummy$Corn*dummy$V2*(1-(land_use_grass*0.4)),
                       "Corn.silage" = dummy$Corn*dummy$V3*(1-(land_use_grass*0.4)),
                       "Wheat" = dummy$Wheat*dummy$V4,
                       "Oats" = dummy$Oats*dummy$V5,
                       "Barley" = dummy$Barley*dummy$V6,
                       "Sorghum.grain" = dummy$Sorghum*dummy$V7,
                       "Sorghum.grain" = dummy$Sorghum*dummy$V8,
                       "Potatoes" = dummy$Potatoes*dummy$V9,
                       "Rye" = dummy$Rye*dummy$V10,
                       "Alfalfa" = dummy$Alfalfa*dummy$V11,
                       "Other Hay/Non Alfalfa" = dummy$Other.Hay.Non.Alfalfa*dummy$V12,
                       "Soybeans" = dummy$Soybeans*dummy$V13*(1-(land_use_grass*0.2)),
                       "Cropland pasture" = dummy$Grass.Pasture*dummy$V14,
                       "Noncropland pasture" = dummy$Grass.Pasture*dummy$V15,
                       "Rice" = 0,
                       "Peanuts" = dummy$Peanuts*dummy$V17,
                       "Grass" = 0,
                       "CGM" = dummy$Corn*dummy$V19,
                       "CGF" = dummy$Corn*dummy$V20,
                       "DGS" = dummy$Corn*dummy$V21)
  cropws <- cropws[cropws$REGION=="Chesapeake Bay Watershed",]
  
  cropws <- cropws %>% arrange(FIPS,LNDRVRSEG)
  
  cropprodcnty_old[,,n] <- cropprodcnty[,,n]
  
  cropprodcnty[,,n] <- as.matrix(subset(aggregate(cropws[,5:24], by=list(cropws$FIPS), FUN = sum),select=-1)) # Readjusting county information to only info inside CBW
  
  cropprodws[,,n] <- as.matrix(subset(cropws,select=-c(1:4)))
  
  dummy <- merge(as.data.frame(lrs_cdl_percents[n],check.names=FALSE)[,1:5],cbind(FIPS,etohprodcnty[,n]))
  cropwse <- data.frame("FIPS"=dummy$FIPS,
                        "LNDRVRSEG"=dummy$LNDRVRSEG,
                        "OBJECTID"=dummy$OBJECTID,
                        "REGION"=dummy$REGION,
                        "Ethanol" = dummy$Corn*dummy$V2)
  
  cropwse <- cropwse[cropwse$REGION=="Chesapeake Bay Watershed",]
  
  etohprodcnty[,n] <- as.matrix(subset(aggregate(cropwse[,5], by=list(cropwse$FIPS), FUN = sum),select=-1)) # Readjusting county information to only info inside CBW
  
  etohprodws[,n] <- as.matrix(subset(cropwse,select=-c(1:4)))  
  

  # Grass scenario
  cropprodcnty[,17,n] <- if(grass_fert_scenario == 1) {cropareacty[,17,n]*grass_yield_fert*100000} else {cropareacty[,17,n]*grass_yield_no_fert*100000} # Multiplying 10^5 to transform from Mg/ha to kg/km2
  cropprodws[,17,n] <- if(grass_fert_scenario == 1) {cropareaws[,17,n]*grass_yield_fert*100000} else {cropareaws[,17,n]*grass_yield_no_fert*100000} # Multiplying 10^5 to transform from Mg/ha to kg/km2
  
  # Winter crop scenario
  
  
  # Calculating densities of crops per area
  for(i in 1:(length(cropname))){ #columns (crops)
    cropproddensws[,i,n] = cropprodws[,i,n]/area
  }
  etohproddensws[,n] = etohprodws[,n]/area
  
  #write data files
  ##crop production
  write_name = paste("InputFiles_CBW/cropprod",run_yrs[n],".txt",sep = "")
  write.table(cropproddensws[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  write_name = paste("InputFiles_CBW/cropprodcnty",run_yrs[n],".txt",sep = "")
  write.table(cropprodcnty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  ##total corn production
  write_name = "InputFiles_CBW/cornprodnoetoh.txt"
  write.table(cornprodnoetoh, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
}
##ethanol production
write_name = paste("InputFiles_CBW/etohproddensws.txt",sep = "")
write.table(etohproddensws, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

#write keys
##crop production
write_name = "InputFileKeys/cropprod_key.txt"
cropprod_key = array(" ", c(n_ws_tbx+1,length(cropname)+1))
cropprod_key[1,]=c(" ", cropname) #column headings
cropprod_key[,1]=c("ws_num", 1:n_ws_tbx) #row headings
write.table(cropprod_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
cropprodtotal = colSums(cropprodcnty) #total crop prod in each year
##total corn production
write_name = "InputFileKeys/cornprodnoetoh_key.txt"
cornprodnoetoh_key = array(" ", c(n_cnty+1,length(import_yrs)+1))
cornprodnoetoh_key[1,]=c(" ", import_yrs) #column headings
cornprodnoetoh_key[,1]=c("total_corn", 1:n_cnty) #row headings
##ethanol production
write_name = "InputFileKeys/etohproddensws_key.txt"
etohproddensws_key = array(" ", c(n_ws_tbx+1,length(import_yrs)+1))
etohproddensws_key[1,]=c(" ", import_yrs) #column headings
etohproddensws_key[,1]=c("L/km^2", 1:n_ws_tbx) #row headings
write.table(etohproddensws_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)


# References ####
# Murphy, J., Bochmann, G., Weiland, P., & Wellinger, A. (2011). Biogas from Crop Digestion. IEA Bioenergy - Task 37, 24. (298 in the biogas calculation)
# Woodbury, P. B., Kemanian, A. R., Jacobson, M., & Langholtz, M. (2018). Improving water quality in the Chesapeake Bay using payments for ecosystem services for perennial biomass for bioenergy and biofuel production. Biomass and Bioenergy, 114, 132–142. https://doi.org/10.1016/j.biombioe.2017.01.024
# C.R. Stoof, B.K. Richards, P.B. Woodbury, et al., Untapped potential: opportunities and challenges for sustainable bioenergy production from marginal lands in the Northeast USA, Bioenergy Res. 8 (2015) 482e501.
# Brown, D., Shi, J., & Li, Y. (2012). Comparison of solid-state to liquid anaerobic digestion of lignocellulosic feedstocks for biogas production. Bioresource Technology, 124, 379–386. https://doi.org/10.1016/j.biortech.2012.08.051
