#harvestedarea_data.r
#This script uses the county-level crop production data (3111 counties from the continental US) for 1997-2017
#from NANI_NAPI_NASS_pull.r
#uses "areas_array"
#and aggregates it to crop-specific totals for each year (which we may not want to do in the future)
#and allocates virtual areas to ethanol and coproducts

if(print_tags == 1){
  print("CreateInputsSubs_CBW/harvestedarea_data.R")
}

cropname=array("",c(n_crops))
cropname[1] = 'corn for grain'
cropname[2] = 'corn for silage'
cropname[3] = 'wheat'
cropname[4] = 'oats'
cropname[5] = 'barley'
cropname[6] = 'sorghum for grain'
cropname[7] = 'sorghum for silage'
cropname[8] = 'potatoes'
cropname[9] = 'rye'
cropname[10] = 'alfalfa hay'
cropname[11] = 'other hay'
cropname[12] = 'soybeans'
cropname[13] = 'cropland pasture'
cropname[14] = 'noncropland pasture'
cropname[15] = 'rice'
cropname[16] = 'peanuts'
cropname[17] = 'grass'
cropname[18] = 'winter rye'
cropname[19] = 'CGF'
cropname[20] = 'CGM'
cropname[21] = 'DGS'

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
  cropws <- data.frame("FIPS"=dummy$FIPS,
                       "LNDRVRSEG"=dummy$LNDRVRSEG,
                       "OBJECTID"=dummy$OBJECTID,
                       "REGION"=dummy$REGION,
                       "Corn.grain" = dummy$Corn*dummy$V2,
                       "Corn.silage" = dummy$Corn*dummy$V3,
                       "Wheat" = dummy$Wheat*dummy$V4,
                       "Oats" = dummy$Oats*dummy$V5,
                       "Barley" = dummy$Barley*dummy$V6,
                       "Sorghum.grain" = dummy$Sorghum*dummy$V7,
                       "Sorghum.grain" = dummy$Sorghum*dummy$V8,
                       "Potatoes" = dummy$Potatoes*dummy$V9,
                       "Rye" = dummy$Rye*dummy$V10,
                       "Alfalfa" = dummy$Alfalfa*dummy$V11,
                       "Other Hay/Non Alfalfa" = dummy$Other.Hay.Non.Alfalfa*dummy$V12,
                       "Soybeans" = dummy$Soybeans*dummy$V13,
                       "Cropland pasture" = dummy$Grass.Pasture*dummy$V14,
                       "Noncropland pasture" = dummy$Grass.Pasture*dummy$V15,
                       "Rice" = 0,
                       "Peanuts" = dummy$Peanuts*dummy$V17,
                       "Grass" = 0,
                       "WinterRye" = ((dummy$Corn*dummy$V2+dummy$Corn*dummy$V3)*wr_adoption_corn)+((dummy$Soybeans*dummy$V13)*wr_adoption_soybean),
                       "CGM" = dummy$Corn*dummy$V20,
                       "CGF" = dummy$Corn*dummy$V21,
                       "DGS" = dummy$Corn*dummy$V22)
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
cropareaharvested_key = array(" ", c(n_crops+1,length(import_yrs)+1))
cropareaharvested_key[1,] = c(" ", import_yrs) #column headings
cropareaharvested_key[,1]=c("crop", cropname) #row headings
write.table(cropareaharvested_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
