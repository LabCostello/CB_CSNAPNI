# Constants and Variables ####
land_use_grass <- 0.10 # Reference: Zhou et al.(2014)
grass_yield_no_fert <- 9.9 # Reference: Woodbury et al.(2018) Unit: M*100**g/ha (DM)
# grass_yield_fert <- 19.7 # Reference: Kering et al.(2012) Unit: Mg/ha

grass_dry_matter <- (1-0.0816)
biogascnty <- array(0,c(n_cnty,length(import_yrs))) 
biogasws <- array(0,c(n_ws_NEEA,length(import_yrs))) 

# Area that will receive grass#####
cropareacty = array(0,c(n_cnty,n_crops,length(import_yrs)))
cropareacty[,1:(n_crops-3),] = areas_array
cropareaws=array(0,c(n_ws_tbx,n_crops,length(import_yrs)))
cornareanoetoh=array(0,c(n_cnty,length(import_yrs)))
yr_col=array(0,c(length(import_yrs),1))

for(n in 1:length(import_yrs)){
  # calc amounts of etoh coproducts
  cropareacty[,(n_crops-2),n]=cornuse[5,n]*cropareacty[,1,n]*to_FC_wetmill[alloc_method]*wetmill_CGF
  cropareacty[,(n_crops-1),n]=cornuse[5,n]*cropareacty[,1,n]*to_FC_wetmill[alloc_method]*wetmill_CGM
  cropareacty[,(n_crops),n]=cornuse[6,n]*cropareacty[,1,n]*to_FC_drymill[alloc_method]
  # calc new corn area
  cornareanoetoh[,n] = drop(cropareacty[,1,n])
  cropareacty[,1,n] = cornareanoetoh[,n]*(1-(cornuse[5,n]+cornuse[6,n])) #proportion of corn not allocated to fuel ethanol production
  #solve NaN issue
  for(m in 1:(n_crops-3)){
    ind=which(is.na(cropareacty[,m,n]) %in% 1)
    if(length(ind)>0){
      for(i in 1:length(ind)){
        cropareacty[ind[i],m,n]=0
      }
    }
  }
  # Reduction in the corn area and production (grain & silage)
  if(grass_scenario == 1){
    # Build an if depending if it is grass fertilized or not
    cropareacty[,17,n] <- (cropareacty[,1,n]+cropareacty[,2,n])*land_use_grass
    if(grass_fert_scenario == 1){grass_prod <- (cropareacty[,17,n]*grass_yield_fert)}else{
      grass_prod <- (cropareacty[,17,n]*grass_yield_no_fert)} # In the future change 
    
    # Reduction of 90% of area harvested for corn
    cropareacty[,1,n] <- cropareacty[,1,n]*0.9
    cropareacty[,2,n] <- cropareacty[,2,n]*0.9
    
  }
  cropareaws[,,n]=t(cnty_ws)%*%cropareacty[,,n]
  
  write_name = paste("InputFiles_CBW/cropareaharvestedcnty",run_yrs[n],".txt",sep = "")
  write.table(cropareacty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
}
croparea=colSums(cropareaws)
cornareanoetohsum=colSums(cornareanoetoh)
etoh_landuse = cornuse[5,]*cornareanoetohsum*(1-to_FC_wetmill[alloc_method])+cornuse[6,]*cornareanoetohsum*(1-to_FC_drymill[alloc_method]) #ag land use for etoh

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


# Production of grass ####
cropprodcnty = array(0,c(n_cnty,length(cropname),length(import_yrs)))
cropprodcnty[,1:(n_crops-3),] = prod_array
etohprodcnty = array(0,c(n_cnty,length(import_yrs)))
cropprodws = array(0,c(n_ws_tbx,length(cropname),length(import_yrs)))
etohprodws = array(0,c(n_ws_tbx,length(import_yrs)))
cropproddensws = array(0,c(n_ws_tbx,length(cropname),length(import_yrs)))
etohproddensws = array(0,c(n_ws_tbx,length(import_yrs)))
cornprodnoetoh=array(0,c(n_cnty,length(import_yrs)))

for(n in 1:(length(import_yrs))){ 
  # build a matrix of extracted data
  for(j in 1:n_cnty){ #rows (counties)
    # calc amounts of etoh coproducts
    cropprodcnty[j,(n_crops-2),n] = cornuse[5,n]*cropprodcnty[j,1,n]*CGF_from_corn #CGF (the CGF produced by corn reported by the USDA in "alcohol for fuel" that is not DDGS)
    cropprodcnty[j,(n_crops-1),n] = cornuse[5,n]*cropprodcnty[j,1,n]*CGM_from_corn #CGM (the CGM produced by corn reported by the USDA in "alcohol for fuel" that is not DDGS)
    cropprodcnty[j,n_crops,n] = cornuse[6,n]*cropprodcnty[j,1,n]*DGS_from_corn #DGS (the DDGS produced by corn from ethanol plants, as reported by the USDA)
    etohprodcnty[j,n] = (cornuse[5,n]+cornuse[6,n])*cropprodcnty[j,1,n]*etoh_from_corn[n] #liters of etoh from corn for etoh, assumption that everry county contributes equally to corn for ethanol
    # calc new corn total
    cornprodnoetoh[j,n] = drop(cropprodcnty[j,1,n])
    cropprodcnty[j,1,n] = cornprodnoetoh[j,n]*(1-(cornuse[5,n]+cornuse[6,n])) #proportion of corn not allocated to fuel ethanol production
    #pastures: "take half, leave half"
    cropprodcnty[j,13:14,n] = cropprodcnty[j,13:14,n]/2
  }
  # Grass scenario
  if(grass_scenario == 1){
    if(grass_fert_scenario == 1){cropprodcnty[,17,n] <- (cropareacty[,17,n]*grass_yield_fert)}else{
      cropprodcnty[,17,n] <- (cropareacty[,17,n]*grass_yield_no_fert)} # In the future change 
    # Reduction of 90% of production for corn
    cropprodcnty[,1,n] <- cropprodcnty[,1,n]*0.9
    cropprodcnty[,2,n] <- cropprodcnty[,2,n]*0.9
  }
  # watershed crop production
  cropprodws[,,n] = t(cnty_ws)%*%cropprodcnty[,,n]
  etohprodws[,n] = t(cnty_ws)%*%etohprodcnty[,n]
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

# Biogas production #####
  # Calculate biogas
for (n in 1:length(import_yrs)){
  biogascnty[,n] <- cropprodcnty[,17,n]*1000*.834*111*0.001 #m3
  biogasws[,n] <- cropprodws[,17,n]*1000*.834*111*0.001 #m3 521,692.8 (Mg->kg->dm3->L)
}

biogas_electricitycnty <- biogascnty*2
biogas_electricityws <- biogasws*2

# References ####
# Murphy, J., Bochmann, G., Weiland, P., & Wellinger, A. (2011). Biogas from Crop Digestion. IEA Bioenergy - Task 37, 24. (298 in the biogas calculation)
# Woodbury, P. B., Kemanian, A. R., Jacobson, M., & Langholtz, M. (2018). Improving water quality in the Chesapeake Bay using payments for ecosystem services for perennial biomass for bioenergy and biofuel production. Biomass and Bioenergy, 114, 132–142. https://doi.org/10.1016/j.biombioe.2017.01.024
# C.R. Stoof, B.K. Richards, P.B. Woodbury, et al., Untapped potential: opportunities and challenges for sustainable bioenergy production from marginal lands in the Northeast USA, Bioenergy Res. 8 (2015) 482e501.
# Brown, D., Shi, J., & Li, Y. (2012). Comparison of solid-state to liquid anaerobic digestion of lignocellulosic feedstocks for biogas production. Bioresource Technology, 124, 379–386. https://doi.org/10.1016/j.biortech.2012.08.051
