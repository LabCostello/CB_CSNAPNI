# Using cropscape to replace cnty_ws

# Creating dataframe based on cropscape####
# We need now to allocate this crops inside of each LRS/DA. For that we will use cropscape
da_crop_area <- da_cdl # Selecting Objectid, FIPS, and 3 types of development lands (low/medium/high) 

# Selecting crops that we got from NASS and are on CSNAPNI
crops_interested <- c(colnames(da_crop_area[,c(1,2,3,5,6,8,10,11,12,13,14,15,16,21,22,25,57,68,69,71,72,73,74,75,80)]))

da_crop_area <- da_crop_area[,crops_interested]

#suming the lands that have corn to the corn column
# corn, Pop or Orn Corn, Dbl Crop WinWht/Corn, Dbl Crop Oats/Corn, Dbl Crop Barley/Corn, Dbl Crop Corn/Soybeans
da_crop_area[,3] <- da_crop_area[,3]+da_crop_area[,7]+da_crop_area[,18]+da_crop_area[,19]+da_crop_area[,21]+da_crop_area[,24]

#suming the lands that have soybeans
# Soybeans, Dbl Crop WinWht/Soybeans, Dbl Crop Soybeans/Oats, Dbl Crop Corn/Soybeans, Dbl Crop Barley/Soybeans
da_crop_area[,5] <- da_crop_area[,5]+da_crop_area[,11]+da_crop_area[,23]+da_crop_area[,24]+da_crop_area[,25]

#suming the lands that have sorghum
# Sorghum, Dbl Crop WinWht/Sorghum
da_crop_area[,4] <- da_crop_area[,4]+da_crop_area[,20]

#suming the lands that have wheat
# Spring Wheat, Winter Wheat, Dbl Crop WinWht/Soybeans,Dbl Crop WinWht/Corn,  Dbl Crop WinWht/Sorghum
da_crop_area[,9] <- da_crop_area[,9]+da_crop_area[,10]+da_crop_area[,11]+da_crop_area[,18]+da_crop_area[,22]
colnames(da_crop_area)[9] <- "WHEAT"

#suming the lands that have oats
# Oats, Dbl Crop Oats/Corn, Dbl Crop Soybeans/Oats
da_crop_area[,13] <- da_crop_area[,13]+da_crop_area[,19]+da_crop_area[,23]

#suming the lands that have barley
# Barley, Dbl Crop Barley/Cornm, Dbl Crop Barley/Soybean
da_crop_area[,8] <- da_crop_area[,8]+da_crop_area[,21]+da_crop_area[,25]


#excluding extra lines
da_crop_area <- da_crop_area[,-c(7,10,11,18,19,20,21,22,23,24,25)]

# Separating corn, sorghum and pasture using NASS info ####
#Corn operation
corn_area_percent<- cropareacty[,c(1,2,18,19,20),5]/rowSums(cropareacty[,c(1,2,18,19,20),5]) # In the future use for different years
corn_area_percent[is.na(corn_area_percent)] <- 0
corn_area_percent <- cbind(FIPS= CB_counties$FIPS,corn_area_percent)

cropscape_corn <- da_crop_area[,1:3]

merged_df2 <- merge(cropscape_corn,corn_area_percent, by="FIPS")

corn_cropscape <- cbind("OBJECTID_12" = merged_df2[,2], merged_df2[,3]*merged_df2[,c(4,5,6,7,8)])
colnames(corn_cropscape)[c(2,3,4,5,6)] <- c("Corn_grain", "Corn_sillage", "CGF","CGM","DGS")
corn_cropscape <- corn_cropscape[order(corn_cropscape[,1]),]
rownames(corn_cropscape) <- NULL
#corn_cropscape <- as.matrix(corn_cropscape)

# Sorghum operation
sorg_area_percent <- cropareacty[,c(6,7),5]/rowSums(cropareacty[,c(6,7),5])
sorg_area_percent[is.na(sorg_area_percent)] <- 0
sorg_area_percent <- cbind(FIPS= CB_counties$FIPS, sorg_area_percent)

cropscape_sorg <- da_crop_area[,c(1,2,4)]

merged_df2 <- merge(cropscape_sorg,sorg_area_percent, by="FIPS")

sorg_cropscape <- cbind("OBJECTID_12" = merged_df2[,2], merged_df2[,3]*merged_df2[,c(4,5)])
colnames(sorg_cropscape)[c(2,3)] <- c("Sorghum_grain", "Sorghum_silage")
sorg_cropscape <- sorg_cropscape[order(sorg_cropscape[,1]),]
rownames(sorg_cropscape) <- NULL

# Grassland/Pasture
past_area_percent <- cropareacty[,c(13,14),5]/rowSums(cropareacty[,c(13,14),5])
past_area_percent[is.na(past_area_percent)] <- 0
past_area_percent <- cbind(FIPS= CB_counties$FIPS, past_area_percent)

cropscape_past <- da_crop_area[,c(1,2,14)]

merged_df2 <- merge(cropscape_past,past_area_percent, by="FIPS")

past_cropscape <- cbind("OBJECTID_12" = merged_df2[,2], merged_df2[,3]*merged_df2[,c(4,5)])
colnames(past_cropscape)[c(2,3)] <- c("Cropland pasture", "Noncropland pasture")
past_cropscape <- past_cropscape[order(past_cropscape[,1]),]
rownames(past_cropscape) <- NULL

# Organizing data
cropscape_crops_da <- data.frame(
  "corn for grain" = corn_cropscape[,2],
  "corn for silage" = corn_cropscape[,3],
  "wheat" = da_crop_area[,8],
  "oats" = da_crop_area[,10],
  "barley" = da_crop_area[,7],
  "sorghum for grain" = sorg_cropscape[,2],
  "sorghum for silage" = sorg_cropscape[,3],
  "potatoes" = da_crop_area[,13],
  "rye" = da_crop_area[,9],
  "alfalfa hay" = da_crop_area[,11],
  "other hay" = da_crop_area[,12],
  "soybeans" = da_crop_area[,5],
  "cropland pasture" = past_cropscape[,2],
  "noncropland pasture" = past_cropscape[,3],
  "rice" = 0,
  "peanuts" = da_crop_area[,6],
  "grass" = 0, # Dont forget to add an if statement after this to replace grass with 10% of corn
  "CGF" = corn_cropscape[,4],
  "CGM" = corn_cropscape[,5],
  "DGS" = corn_cropscape[,6])

if (grass_scenario == 1){
  cropscape_crops_da <- data.frame(
    "corn for grain" = corn_cropscape[,2]*0.9,
    "corn for silage" = corn_cropscape[,3],
    "wheat" = da_crop_area[,8],
    "oats" = da_crop_area[,10],
    "barley" = da_crop_area[,7],
    "sorghum for grain" = sorg_cropscape[,2],
    "sorghum for silage" = sorg_cropscape[,3],
    "potatoes" = da_crop_area[,13],
    "rye" = da_crop_area[,9],
    "alfalfa hay" = da_crop_area[,11],
    "other hay" = da_crop_area[,12],
    "soybeans" = da_crop_area[,5],
    "cropland pasture" = past_cropscape[,2],
    "noncropland pasture" = past_cropscape[,3],
    "rice" = 0,
    "peanuts" = da_crop_area[,6],
    "grass" = (corn_cropscape[,2]+corn_cropscape[,3])*0.1, 
    "CGF" = corn_cropscape[,4],
    "CGM" = corn_cropscape[,5],
    "DGS" = corn_cropscape[,6])
}

# da_prop_developed_area <- cbind(da_crop_area[,c(1,2)],TOTAL = rowSums(da_crop_area[,3:5]))
setdiff(FIPS,unique(da_crop_area[,2]))
dummy <- cbind(da_crop_area[,1:2],cropscape_crops_da)

# Merging population by county and proportion of developed area per da (The result gives a little bit more population in DA, beacause of rounding numbers)

# Excluding those FIPS 51037 (Charlotte) 51111 (Lunenburg) 54025 (Greenbrier) 54075 (Pocahontas) 54083 (Randolph)
#cropareacty <- cropareacty[-c(106,133,191,199,201),,]

# For Area harvested - cropareaws / do etohprodws
for (i in 1:5){
  proportion <- 0
  cropws <- 0
  dummy <- cbind(da_crop_area[,1:2],cropscape_crops_da)
  total_dummy <- 0
  dummy <- merge(cbind(FIPS,cropareacty[,,i]),dummy, by="FIPS")
  total_dummy <- dummy %>% group_by(FIPS) %>% summarize(TOTAL_Corn_g = sum(corn.for.grain),
                                                        TOTAL_Corn_s = sum(corn.for.silage),
                                                        TOTAL_wheat = sum(wheat),
                                                        TOTAL_oats = sum(oats),
                                                        TOTAL_barley = sum(barley),
                                                        TOTAL_sorg_g = sum(sorghum.for.grain),
                                                        TOTAL_sorg_s = sum(sorghum.for.silage),
                                                        TOTAL_potatoes = sum(potatoes),
                                                        TOTAL_rye = sum(rye),
                                                        TOTAL_alfalfa = sum(alfalfa.hay),
                                                        TOTAL_hay = sum(other.hay),
                                                        TOTAL_soy = sum(soybeans),
                                                        TOTAL_past = sum(cropland.pasture),
                                                        TOTAL_pastn = sum(noncropland.pasture),
                                                        TOTAL_rice = sum(rice),
                                                        TOTAL_peanuts = sum(peanuts),
                                                        TOTAL_grass = sum(grass),
                                                        TOTAL_cgf = sum(CGF),
                                                        TOTAL_cgm = sum(CGM),
                                                        TOTAL_dgs = sum(DGS))
  dummy <- merge(dummy, total_dummy, by="FIPS") 
  proportion <- cbind(dummy[,c(1,22)],dummy[,23:42] / dummy[,43:62])
  proportion[is.na(proportion)] <- 0
  proportion_cty_da <- merge(cbind(FIPS,cropareacty[,,i]),proportion, by="FIPS")
  cropws <- cbind(proportion_cty_da[,c(1,22)],proportion_cty_da[,2:21]*proportion_cty_da[,23:42])
  cropws <- cropws[order(cropws[,2]),]
  # dummy <- dummy[,2:6]*dummy[,9]
  rownames(cropws) <- NULL
  cropareaws[,,i] <- as.matrix(subset(cropws,select=-c(1,2)))
}

# For crop production - cropprodws # Do also cropproddensws and etohprodws
for (i in 1:5){
  dummy <- cbind(da_crop_area[,1:2],cropscape_crops_da)
  proportion_cty_da <- 0
  dummy <- merge(cbind(FIPS,cropprodcnty[,,i]),dummy, by="FIPS")
  total_dummy <- dummy %>% group_by(FIPS) %>% summarize(TOTAL_Corn_g = sum(corn.for.grain),
                                                        TOTAL_Corn_s = sum(corn.for.silage),
                                                        TOTAL_wheat = sum(wheat),
                                                        TOTAL_oats = sum(oats),
                                                        TOTAL_barley = sum(barley),
                                                        TOTAL_sorg_g = sum(sorghum.for.grain),
                                                        TOTAL_sorg_s = sum(sorghum.for.silage),
                                                        TOTAL_potatoes = sum(potatoes),
                                                        TOTAL_rye = sum(rye),
                                                        TOTAL_alfalfa = sum(alfalfa.hay),
                                                        TOTAL_hay = sum(other.hay),
                                                        TOTAL_soy = sum(soybeans),
                                                        TOTAL_past = sum(cropland.pasture),
                                                        TOTAL_pastn = sum(noncropland.pasture),
                                                        TOTAL_rice = sum(rice),
                                                        TOTAL_peanuts = sum(peanuts),
                                                        TOTAL_grass = sum(grass),
                                                        TOTAL_cgf = sum(CGF),
                                                        TOTAL_cgm = sum(CGM),
                                                        TOTAL_dgs = sum(DGS))
  dummy <- merge(dummy, total_dummy, by="FIPS") 
  proportion <- cbind(dummy[,c(1,22)],dummy[,23:42] / dummy[,43:62])
  proportion[is.na(proportion)] <- 0
  proportion_cty_da <- merge(cbind(FIPS,cropprodcnty[,,i]),proportion, by="FIPS")
  proportion_cty_da_etoh <- merge(cbind(FIPS,etohprodcnty[,i]),proportion[,1:3], by="FIPS")
  cropproduction <- cbind(proportion_cty_da[,c(1,22)],proportion_cty_da[,2:21]*proportion_cty_da[,23:42])
  cropproduction <- cropproduction[order(cropproduction[,2]),]
  etohproduction <- cbind(proportion_cty_da_etoh[,c(1,3)],proportion_cty_da_etoh[,2]*proportion_cty_da_etoh[,4])
  etohproduction <- etohproduction[order(etohproduction[,2]),]
  
  rownames(cropproduction) <- NULL
  rownames(etohproduction) <- NULL
  cropprodws[,,i] <- as.matrix(subset(cropproduction,select=-c(1,2)))
  etohprodws[,i] <- as.matrix(subset(etohproduction,select=-c(1,2)))
  
}


for(i in 1:5){
  for(j in 1:(length(cropname))){ #columns (crops)
    cropproddensws[,j,i] = cropprodws[,j,i]/area
  }
  etohproddensws[,i] = etohprodws[,i]/area
  #write data files
  ##crop production
  write_name = paste("InputFiles_CBW/cropprod",run_yrs[n],".txt",sep = "")
  write.table(cropproddensws[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
}


# For livestock
for (i in 1:5){
  dummy_liv <- 0
  dummy_cow <- 0
  dummy <- da_cdl[,c(1:45,57:80)] # cbind(da_cdl[,c(2,1)],da_cdl[,c(1:45,57:80)])
  dummy <- merge(cbind(FIPS,animpopcnty[,,i]),dummy, by="FIPS")

  dummy_liv <- cbind(dummy[,c(1,21)], Cropland = rowSums(dummy[,22:88]))
  dummy_cow <- cbind(dummy[,c(1,21)], Cropland = rowSums(dummy[,c(22,40)]))
  total_dummy_liv <- dummy_liv %>% group_by(FIPS) %>% summarize(TOTAL_Cropland = sum(Cropland))
  total_dummy_cow <- dummy_cow %>% group_by(FIPS) %>% summarize(TOTAL_Cropland = sum(Cropland))
  dummy_liv <- merge(dummy_liv, total_dummy_liv, by="FIPS") 
  dummy_cow <- merge(dummy_cow, total_dummy_cow, by="FIPS")
  
  proportion_liv <- cbind(dummy_liv[,c(1,2)],proportion = dummy_liv[,3] / dummy_liv[,4])
  proportion_cow <- cbind(dummy_cow[,c(1,2)],proportion = dummy_cow[,3] / dummy_cow[,4])
  proportion_liv[is.na(proportion_liv)] <- 0  
  proportion_cow[is.na(proportion_cow)] <- 0
  animpop <- dummy[,c(1,21,2:20)]
  animpop[,5:21] <- animpop[,5:21]*proportion_liv[,3]
  animpop[,3:4] <- animpop[,3:4]*proportion_cow[,3] # cattle
  animpop <- animpop[order(animpop[,2]),]
  rownames(animpop) <- NULL
  animpopws[,,i] <- as.matrix(subset(animpop,select=-c(1,2)))
  
  #write data files
  ##animal population
  write_name = paste("InputFiles_CBW/noanimdyn",run_yrs[n],".txt",sep = "")
  write.table(animpopws[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

}

# Saving this matrix as files    
# harvestedarea_data
write_name = paste("InputFiles_CBW/cropareaharvested.txt")
write.table(croparea, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)


#write key

#cropprod_data
##ethanol production
write_name = paste("InputFiles_CBW/etohproddensws.txt",sep = "")
write.table(etohproddensws, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

