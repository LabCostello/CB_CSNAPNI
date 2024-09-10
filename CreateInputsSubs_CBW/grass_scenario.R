if(print_tags == 1){
  print("CreateInputsSubs_CBW/grass_scenario.R")
}

grass_dry_matter <- (1-0.0816) # Not in use, because the yield provided above is in dry matter already
biogascnty <- array(0,c(n_cnty,length(year_labels))) 
biogasws <- array(0,c(n_ws_NEEA,length(year_labels))) 

# Area Harvested

cropareacty_old = array(0,c(n_cnty,n_crops,length(year_labels)))
cropareacty = array(0,c(n_cnty,n_crops,length(year_labels)))
cropareacty[,1:(n_crops-3),] = areas_array #crop harvested areas, in km2
cropareaws=array(0,c(n_ws_tbx,n_crops,length(year_labels)))
cornareanoetoh=array(0,c(n_cnty,length(year_labels)))
yr_col=array(0,c(length(year_labels),1))
for(n in 1:length(year_labels)){
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
  colnames(dummy)[17:37] <- cropareaharvested_key[-1,1]
  cropws <- data.frame("FIPS"=dummy$FIPS,
                       "LNDRVRSEG"=dummy$LNDRVRSEG,
                       "OBJECTID"=dummy$OBJECTID,
                       "REGION"=dummy$REGION,
                       "Corn.grain" = dummy$Corn*dummy$`corn for grain`*(1-(land_use_grass*1)),
                       "Corn.silage" = dummy$Corn*dummy$`corn for silage`*(1-(land_use_grass*1)),
                       "Wheat" = dummy$Wheat*dummy$wheat,
                       "Oats" = dummy$Oats*dummy$oats,
                       "Barley" = dummy$Barley*dummy$barley,
                       "Sorghum.grain" = dummy$Sorghum*dummy$`sorghum for grain`,
                       "Sorghum.grain" = dummy$Sorghum*dummy$`sorghum for silage`,
                       "Potatoes" = dummy$Potatoes*dummy$potatoes,
                       "Rye" = dummy$Rye*dummy$rye,
                       "Alfalfa" = dummy$Alfalfa*dummy$`alfalfa hay`,
                       "Other Hay/Non Alfalfa" = dummy$Other.Hay.Non.Alfalfa*dummy$`other hay`,
                       "Soybeans" = dummy$Soybeans*dummy$soybeans*(1-(land_use_grass*0)),
                       "Cropland pasture" = dummy$Grass.Pasture*dummy$`cropland pasture`,
                       "Noncropland pasture" = dummy$Grass.Pasture*dummy$`noncropland pasture`,
                       "Rice" = 0,
                       "Peanuts" = dummy$Peanuts*dummy$grass,
                       "Grass" = ((0.5*dummy$Corn*dummy$`corn for grain`)+(0.5*dummy$Corn*dummy$`corn for silage`)+(0*dummy$Soybeans*dummy$soybeans))*land_use_grass, # CG-CG-S-CS-CS rotation
#                       "Grass" = ((dummy[,Crotation[1]]*dummy[,Crotation[2]])+(dummy[,Crotation[3]]*dummy[,Crotation[4]])+(dummy[,Crotation[5]]*dummy[,Crotation[6]])+(dummy[,Crotation[7]]*dummy[,Crotation[8]])+(dummy[,Crotation[9]]*dummy[,Crotation[10]]))*0.2*land_use_grass, # CG-CG-S-CS-CS rotation
#                       "Grass" = 
                       "WinterRye" = ((dummy$Corn*dummy$`corn for grain`*(1-(land_use_grass*0.4))+dummy$Corn*dummy$`corn for silage`*(1-(land_use_grass*0.4)))*wr_adoption_corn)+((dummy$Soybeans*dummy$soybeans*(1-(land_use_grass*0.2)))*wr_adoption_soybean),
                       "CGM" = dummy$Corn*dummy$CGF,
                       "CGF" = dummy$Corn*dummy$CGM,
                       "DGS" = dummy$Corn*dummy$DGS)
  cropws <- cropws[cropws$REGION=="Chesapeake Bay Watershed",]
  
  cropws <- cropws %>% arrange(FIPS,LNDRVRSEG)
  
  cropareacty_old[,,n] <- cropareacty[,,n]
  
  cropareacty[,,n] <- as.matrix(subset(aggregate(cropws[,5:length(cropws)], by=list(cropws$FIPS), FUN = sum),select=-1)) # Readjusting county information to only info inside CBW
  
  cropareaws[,,n] <- as.matrix(subset(cropws,select=-c(1:4)))
  
  write_name = paste("InputFiles_CBW/cropareaharvestedcnty",run_yrs[n],".txt",sep = "")
  write.table(cropareacty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
}
croparea=colSums(cropareaws)
cornareanoetohsum=colSums(cornareanoetoh)
#etoh_landuse = cornuse[5,]*cornareanoetohsum*(1-to_FC_wetmill[alloc_method])+cornuse[6,]*cornareanoetohsum*(1-to_FC_drymill[alloc_method]) #ag land use for etoh
etoh_landuse <- c(0,0,0,0,0,0) # There is no land use to produce ethanol in the region. The corn that is produced for etoh is from outside of the CBW (assumption)

#write files
write_name = paste("InputFiles_CBW/corntotareaharvested.txt")
write.table(cornareanoetohsum, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
write_name = paste("InputFiles_CBW/cropareaharvested.txt")
write.table(croparea, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
write_name = paste("InputFiles_CBW/etoh_landuse_harvestedarea.txt")
write.table(etoh_landuse, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

#write key
write_name = paste("InputFileKeys/cropareaharvested_key.txt")
cropareaharvested_key = array(" ", c(n_crops+1,length(year_labels)+1))
cropareaharvested_key[1,] = c(" ", year_labels) #column headings
cropareaharvested_key[,1]=c("crop", cropname) #row headings
write.table(cropareaharvested_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

#Production
# allocate space to matrices
cropprodcnty_old <- array(0,c(n_cnty,length(cropname),length(year_labels)))
cropprodcnty = array(0,c(n_cnty,length(cropname),length(year_labels)))
cropprodcnty[,1:(n_crops-3),] = prod_array
etohprodcnty = array(0,c(n_cnty,length(year_labels)))
cropprodws = array(0,c(n_ws_tbx,length(cropname),length(year_labels)))
etohprodws = array(0,c(n_ws_tbx,length(year_labels)))
cropproddensws = array(0,c(n_ws_tbx,length(cropname),length(year_labels)))
etohproddensws = array(0,c(n_ws_tbx,length(year_labels)))
cornprodnoetoh=array(0,c(n_cnty,length(year_labels)))

# Ethanol information 2012 (https://neo.ne.gov/programs/stats/122/2012/122_201201.htm) and 2017 (https://neo.ne.gov/programs/stats/122/2017/122_201702.htm)
etohprodcnty[58,] <- c(0, 0, 0, 416395296.24,416395296.24,416395296.24) # Clearfield plant produces 110 Mgal in 2012 and 2017
etohprodcnty[170,] <- c(0, 0, 0, 246051765.96,227124707.04,227124707.04) # Hopewell plant produces 65 Mgal in 2012 and 60Mgal in 2017 it is barley ethanol

# DGS information for 2012 and 2017
cropprodcnty[58,n_crops,] <- c(0, 0, 0, 258168.1*1000,258168.1*1000,258168.1*1000) # in kg (data comes from Supplementary Info Table 7 from Ruffatto et al. 2023)

for(n in 1:(length(year_labels))){ 
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
                       "Corn.grain" = dummy$Corn*dummy$V2*(1-(land_use_grass*0.8)),
                       "Corn.silage" = dummy$Corn*dummy$V3*(1-(land_use_grass*0.8)),
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
                       "WinterRye" = 0,
                       "CGM" = dummy$Corn*dummy$V20,
                       "CGF" = dummy$Corn*dummy$V21,
                       "DGS" = dummy$Corn*dummy$V22)
  cropws <- cropws[cropws$REGION=="Chesapeake Bay Watershed",]
  
  cropws <- cropws %>% arrange(FIPS,LNDRVRSEG)
  
  cropprodcnty_old[,,n] <- cropprodcnty[,,n]
  
  cropprodcnty[,,n] <- as.matrix(subset(aggregate(cropws[,5:length(cropws)], by=list(cropws$FIPS), FUN = sum),select=-1)) # Readjusting county information to only info inside CBW
  
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
  if (wr_scenario==1) {
    cropprodcnty[,18,n] <- if(wr_use == 1) {cropareacty[,18,n]*wr_yield_cc*100000} else {cropareacty[,18,n]*wr_yield_dc*100000} # Multiplying 10^5 to transform from Mg/ha to kg/km2
    cropprodws[,18,n] <- if(wr_use == 1) {cropareaws[,18,n]*wr_yield_cc*100000} else {cropareaws[,18,n]*wr_yield_dc*100000} # Multiplying 10^5 to transform from Mg/ha to kg/km2
  }
  
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
cornprodnoetoh_key = array(" ", c(n_cnty+1,length(year_labels)+1))
cornprodnoetoh_key[1,]=c(" ", year_labels) #column headings
cornprodnoetoh_key[,1]=c("total_corn", 1:n_cnty) #row headings
##ethanol production
write_name = "InputFileKeys/etohproddensws_key.txt"
etohproddensws_key = array(" ", c(n_ws_tbx+1,length(year_labels)+1))
etohproddensws_key[1,]=c(" ", year_labels) #column headings
etohproddensws_key[,1]=c("L/km^2", 1:n_ws_tbx) #row headings
write.table(etohproddensws_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)


# References ####
# Murphy, J., Bochmann, G., Weiland, P., & Wellinger, A. (2011). Biogas from Crop Digestion. IEA Bioenergy - Task 37, 24. (298 in the biogas calculation)
# Woodbury, P. B., Kemanian, A. R., Jacobson, M., & Langholtz, M. (2018). Improving water quality in the Chesapeake Bay using payments for ecosystem services for perennial biomass for bioenergy and biofuel production. Biomass and Bioenergy, 114, 132–142. https://doi.org/10.1016/j.biombioe.2017.01.024
# C.R. Stoof, B.K. Richards, P.B. Woodbury, et al., Untapped potential: opportunities and challenges for sustainable bioenergy production from marginal lands in the Northeast USA, Bioenergy Res. 8 (2015) 482e501.
# Brown, D., Shi, J., & Li, Y. (2012). Comparison of solid-state to liquid anaerobic digestion of lignocellulosic feedstocks for biogas production. Bioresource Technology, 124, 379–386. https://doi.org/10.1016/j.biortech.2012.08.051
# https://fyi.extension.wisc.edu/forage/files/2014/01/RyeForage-FOF.pdf