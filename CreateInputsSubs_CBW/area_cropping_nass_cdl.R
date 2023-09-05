# Cropscape Counties #######
# Adjusting area inside and outside of CBW (Counties - [part of the Counties outside of the CB])
# Here it is done we calculate a proportion for each crop based on cropscape (CDL) data
# It is calculated how much pixels is inside of each county in the CB and how much is in the whole county
# Then it is calculated a proportion, based on a division of the former by the second.
# That proportion is multiplied by the area_harvested and production

cnty_cdl <- read_excel(path = "C:/Users/lds5498/OneDrive - The Pennsylvania State University/Desktop/Code/cnty_da_cdl.xlsx", sheet = "cnty_cdl")
cbw_cnty_cdl <- read_excel(path = "C:/Users/lds5498/OneDrive - The Pennsylvania State University/Desktop/Code/cnty_da_cdl.xlsx", sheet = "cbw_cnty_cdl")
crops_cnty_cdl <- colnames(cnty_cdl)
crops_cbw_cnty_cdl <- colnames(cbw_cnty_cdl)

# Groupping columns for crops to allocate livestock ####
# Groupping columns of cropland (Booth et al. approach)
# Checking which county is in both
cnty_cdl_liv <- cnty_cdl
cbw_cnty_cdl_liv <- cbw_cnty_cdl

cnty_cdl_liv[,2:82] <- cnty_cdl[,2:82]/1000000
cbw_cnty_cdl_liv[,2:79] <- cbw_cnty_cdl[,2:79]/1000000

not_in_cbw <- cbw_cnty_cdl_liv$FIPS[!(cnty_cdl_liv$FIPS %in% cbw_cnty_cdl_liv$FIPS)] # Cayuga County, Roanoke City, Salem City
indices <- which(cbw_cnty_cdl_liv$FIPS %in% not_in_cbw)

cnty_cdl_liv <- cnty_cdl_liv[-indices,] # taking off the 3 extra counties

cnty_cdl_liv <- rowSums(cnty_cdl_liv[,-c(1,47:56,58,59)])
cnty_cdl_liv <- cbind(FIPS = cbw_cnty_cdl$FIPS, cnty_cdl_liv)
cbw_cnty_cdl_liv <- rowSums(cbw_cnty_cdl_liv[,-c(1,46:55,57,58)])
cbw_cnty_cdl_liv <- cbind(FIPS = cbw_cnty_cdl$FIPS, cbw_cnty_cdl_liv)

# Proportion for the crops inside of CBW (for livestock allocation)
proportion_cnty_cbw_liv <- cbw_cnty_cdl_liv[,2]/cnty_cdl_liv[,2]
proportion_cnty_cbw_liv <- cbind(FIPS= cbw_cnty_cdl$FIPS, proportion_cnty_cbw_liv)

# Groupping columns for crops (double cropping, same crops and excluding the unecessary) ####
cnty_cdl[,2:82] <- cnty_cdl[,2:82]/1000000
cbw_cnty_cdl[,2:79] <- cbw_cnty_cdl[,2:79]/1000000

#suming the lands that have corn to the corn column
# corn, Pop or Orn Corn, Dbl Crop WinWht/Corn, Dbl Crop Oats/Corn, Dbl Crop Barley/Corn, Dbl Crop Corn/Soybeans
cnty_cdl[,2] <- cnty_cdl[,2]+cnty_cdl[,9]+cnty_cdl[,70]+cnty_cdl[,71]+cnty_cdl[,74]+cnty_cdl[,77]
cbw_cnty_cdl[,2] <- cbw_cnty_cdl[,2]+cbw_cnty_cdl[,9]+cbw_cnty_cdl[,67]+cbw_cnty_cdl[,68]+cbw_cnty_cdl[,71]+cbw_cnty_cdl[,74]

#suming the lands that have soybeans
# Soybeans, Dbl Crop WinWht/Soybeans, Dbl Crop Soybeans/Oats, Dbl Crop Corn/Soybeans, Dbl Crop Barley/Soybeans
cnty_cdl[,5] <- cnty_cdl[,5]+cnty_cdl[,13]+cnty_cdl[,76]+cnty_cdl[,77]+cnty_cdl[,82]
cbw_cnty_cdl[,5] <- cbw_cnty_cdl[,5]+cbw_cnty_cdl[,13]+cbw_cnty_cdl[,73]+cbw_cnty_cdl[,74]+cbw_cnty_cdl[,79]

#suming the lands that have sorghum
# Sorghum, Dbl Crop WinWht/Sorghum
cnty_cdl[,4] <- cnty_cdl[,4]+cnty_cdl[,73]
cbw_cnty_cdl[,4] <- cbw_cnty_cdl[,4]+cbw_cnty_cdl[,70]

#suming the lands that have wheat
# Spring Wheat, Winter Wheat, Dbl Crop WinWht/Soybeans, Dbl Crop WinWht/Corn,  Dbl Crop WinWht/Sorghum, DBL_CROP_WINWHT_COTTON
cnty_cdl[,11] <- cnty_cdl[,11]+cnty_cdl[,12]+cnty_cdl[,13]+cnty_cdl[,70]+cnty_cdl[,73]+cnty_cdl[,75]
colnames(cnty_cdl)[11] <- "WHEAT"
cbw_cnty_cdl[,11] <- cbw_cnty_cdl[,11]+cbw_cnty_cdl[,12]+cbw_cnty_cdl[,13]+cbw_cnty_cdl[,67]+cbw_cnty_cdl[,70]+cbw_cnty_cdl[,72]
colnames(cbw_cnty_cdl)[11] <- "WHEAT"

#suming the lands that have oats
# Oats, Dbl Crop Oats/Corn, Dbl Crop Soybeans/Oats
cnty_cdl[,15] <- cnty_cdl[,15]+cnty_cdl[,71]+cnty_cdl[,76]
cbw_cnty_cdl[,15] <- cbw_cnty_cdl[,15]+cbw_cnty_cdl[,68]+cbw_cnty_cdl[,73]

#suming the lands that have barley
# Barley, Dbl Crop Barley/Cornm, Dbl Crop Barley/Soybean
cnty_cdl[,10] <- cnty_cdl[,10]+cnty_cdl[,74]+cnty_cdl[,82]
cbw_cnty_cdl[,10] <- cbw_cnty_cdl[,10]+cbw_cnty_cdl[,71]+cbw_cnty_cdl[,79]

#excluding extra lines
# Corn (2), Sorghum (4), Soybeans (5), Peanuts (7), Barley (10), Wheat (11), Rye (14), Oats (15), Alfalfa (20), Other hay (21), Potatoes (24/25), Grassland/Pasture (56/57)
cnty_cdl <- cnty_cdl[,c(1,2,4,5,7,10,11,14,15,20,21,25,57)]
cbw_cnty_cdl <- cbw_cnty_cdl[,c(1,2,4,5,7,10,11,14,15,20,21,24,56)]

# Checking the difference between the columns and lines ####
setdiff(names(cnty_cdl),names(cbw_cnty_cdl))

# Checking which county is in both
not_in_cbw <- cnty_cdl$FIPS[!(cnty_cdl$FIPS %in% cbw_cnty_cdl$FIPS)] # Cayuga County, Roanoke City, Salem City
indices <- which(cnty_cdl$FIPS %in% not_in_cbw)

cnty_cdl <- cnty_cdl[-indices,] # taking off the 3 extra counties

# If following 2 condiitions are true then I can divide one dataframe by the other
condition1 <- sum(cnty_cdl$FIPS == cbw_cnty_cdl$FIPS) #It has to be equal to 202
condition2 <- sum(colnames(cnty_cdl) == colnames(cbw_cnty_cdl)) #It has to be equal to 79

# Proportion for the crops inside of CBW ####
proportion_cnty_cbw <- cbw_cnty_cdl[,2:13]/cnty_cdl[,2:13]
proportion_cnty_cbw <- cbind(FIPS= cbw_cnty_cdl$FIPS, proportion_cnty_cbw)
proportion_cnty_cbw[is.na(proportion_cnty_cbw)] <- 0


# Adapting NASS_County to cropped area ####
# Crops (area harvested and production)
# [1] Corn [1997-2017]
index <- grep("CORN", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw[,2]

# [2] Wheat [1997-2017]
index <- grep("WHEAT", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw[,7]

# [3] Oats [1997-2017]
index <- grep("OATS_ACRES", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw[,9]
index <- grep("OATS_PRODUCTION", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw[,9]

# [4] Barley [1997-2017]
index <- grep("BARLEY", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw[,6]

# [5] Sorghum [1997-2017]
index <- grep("SORGHUM", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw[,3]

# [6] Potatoes [1997-2017]
index <- grep("POTATOES", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw[,12]

# [7] Rye [1997-2017]
index <- grep("RYE", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw[,8]

# [8] Alfalfa [1997-2017]
index <- grep("HAY_ALFALFA", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw[,10]

# [9] Other hay [1997-2017]
index <- grep("HAY_ACRES", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw[,11]
index <- grep("HAY_PRODUCTION", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw[,11]

# [10] Soybean [1997-2017]
index <- grep("SOYBEANS", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw[,4]

# [11] Ag_land_cropland_pastured [1997-2017] Check
index <- grep("ag_land", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw[,13]

# [12] Peanuts [1997-2017]
index <- grep("PEANUTS", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw[,5]

# Livestock
proportion_cnty_cbw_liv
# [1] Cattle
index <- grep("CATTLE", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw_liv[,2]

# [2] Hogs
index <- grep("HOGS", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw_liv[,2]

# [1] Chickens
index <- grep("CHICKENS", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw_liv[,2]

# [1] Turkeys
index <- grep("TURKEYS", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw_liv[,2]

# [1] Sheep
index <- grep("SHEEP", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw_liv[,2]

# [1] Equine
index <- grep("EQUINE", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw_liv[,2]

# [1] Goats
index <- grep("GOATS", names(NASS_County))
NASS_County[,index] <- NASS_County[,index]*proportion_cnty_cbw_liv[,2]

