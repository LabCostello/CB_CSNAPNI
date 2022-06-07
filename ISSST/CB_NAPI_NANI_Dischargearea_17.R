#R code for the baseline NAPI/NANI for the discarge areas in the CB 
#Extracting data for 2017

kgfertPcnty_17 <- kgfertPcnty[,,5]
kganimPintakecnty_17 <- kganimPintakecnty[,,5]
kghumanPintakecnty_17 <-read.delim(file.choose("kghumanPintakecnty17.txt"), header = FALSE) # Need to choose this file from the outputfiles folder
kgcropPcnty_17 <- kgcropPcnty[,,5]
kgmeatPcnty_17 <- kgmeatPcnty[,,5]

library(readxl)
countydatalevel <- read_xlsx("CommonDataLabels.xlsx") # This file contains county FIPS, and all CSNAPNI OUTPUT keys 
CB_counties <- read_xlsx("CB_Counties.xlsx") # this file contains CB data such as FIPS and atmospheric deposition data for all counties 

#Atmospheric deposition: first col>kgN/km2/yr; 2col>county area in km2; 3rd col > atmNdep in kg N as NOy N
atmNdep_areainkm2 <- read_excel("CB_Counties.xlsx", sheet = "atmNdep_kgN_km2_yr_2008",col_names = FALSE)
CB_DF <- read_xlsx("CB_DF.xlsx", sheet = "Matrix", col_names = FALSE) # discharge factor Concordance matrix for CB counties [created by collins]
#CB_counties discharge area total for each county
#CB_countiesdischargearea = rowSums(CB_DF[,c(1:1902)])

#converting the FIPS column to vector as a requirement for filtering from dataframe
CB_counties_FIPS <- as.vector(unlist(CB_counties[,1]))

#Adding the FIPS as the first col of each NAPI component
kgfertPcnty_17_fips <- cbind(countydatalevel[,1],kgfertPcnty_17[,1:19])
kganimPintakecnty_17_fips <- cbind(countydatalevel[,1],kganimPintakecnty_17[,1:19])
kghumanPintakecnty_17_fips <- cbind(countydatalevel[,1],kghumanPintakecnty_17[,1])
kgcropPcnty_17_fips <- cbind(countydatalevel[,1],kgcropPcnty_17[,1:19])
kgmeatPcnty_17_fips <- cbind(countydatalevel[,1],kgmeatPcnty_17[,1:9])

#Filtering out CB NAPI components from all counties
#output as dataframe
CB_kgfertPcnty_17_fips <- kgfertPcnty_17_fips[kgfertPcnty_17_fips[,1]%in% CB_counties_FIPS,]
CB_kganimPintakecnty_17_fips <- kganimPintakecnty_17_fips[kganimPintakecnty_17_fips[,1]%in% CB_counties_FIPS,]
CB_kghumanPintakecnty_17_fips <- kghumanPintakecnty_17_fips[kghumanPintakecnty_17_fips[,1]%in% CB_counties_FIPS,]
CB_kgcropPcnty_17_fips <- kgcropPcnty_17_fips[kgcropPcnty_17_fips[,1]%in% CB_counties_FIPS,]
CB_kgmeatPcnty_17_fips <- kgmeatPcnty_17_fips[kgmeatPcnty_17_fips[,1]%in% CB_counties_FIPS,]

#The following outputs then processed in the excel. Total of each crops by counties (with and without cropped area coefficient) multiplied with the concerned 
#cropscape coefficient to get results by discharge area unit 

write.csv(CB_kgcropPcnty_17_fips,"ZIA/NANI-NAPI/CB_kgcropPcnty_17_fips.csv")
write.csv(CB_kgfertPcnty_17_fips,"ZIA/NANI-NAPI/CB_kgfertPcnty_17_fips.csv")
write.csv(CB_kghumanPintakecnty_17_fips,"ZIA/NANI-NAPI/CB_kghumanPintakecnty_17_fips.csv")


#Summing columns of each CB_NAPI components to get the total
# CB_kgfertPcnty_17_fips$total <- rowSums(CB_kgfertPcnty_17_fips[, c(2:20)])
# CB_kganimPintakecnty_17_fips$total <- rowSums(CB_kganimPintakecnty_17_fips[, c(2:20)])
# CB_kgcropPcnty_17_fips$total <- rowSums(CB_kgcropPcnty_17_fips[, c(2:20)])
# CB_kgmeatPcnty_17_fips$total <- rowSums(CB_kgmeatPcnty_17_fips[, c(2:10)])

# #Creating CB_NAPI
# NAPI_components <- cbind(CB_kgfertPcnty_17_fips[,1],CB_kgfertPcnty_17_fips[,21], CB_kganimPintakecnty_17_fips[,21],CB_kghumanPintakecnty_17_fips[,2],CB_kgcropPcnty_17_fips[,21],CB_kgmeatPcnty_17_fips[,11])
# colnames(NAPI_components) <- c("FIPS_CB", "kgP_Fertilizer", "kganimalPintake", "kghumanPintake", "kgPincrop", "kgPinmeat")
# NAPI_components <- as.data.frame(NAPI_components) #Converting to data Frame to match math operations. 
# 
# NAPI_components$Netfoodandfeed <- (NAPI_components$kganimalPintake+NAPI_components$kghumanPintake)-(NAPI_components$kgPincrop+NAPI_components$kgPinmeat)
# NAPI_components$NAPI <- (NAPI_components$kgP_Fertilizer+NAPI_components$Netfoodandfeed)
# NAPI_components$NAPI_CB <- CB_countiesdischargearea*NAPI_components[,8] #Multiplying cropped area to get the NAPI of CB
# write.csv(NAPI_components,"ZIA/CB_Counties_NAPI_17.csv")
# write.csv(CB_countiesdischargearea,"ZIA/CB_Counties_Dischargeareas_REVISED.csv")

#NANI for CB
#Extracting data for 2017

kgfertNcnty_17 <- kgfertNcnty[,,5]
kganimNintakecnty_17 <- kganimNintakecnty[,,5]
kghumanNintakecnty_17 <-read.delim(file.choose("kghumanNintakecnty17.txt"), header = FALSE)# Need to choose this file from the outputfiles folder
kgcropNcnty_17 <- kgcropNcnty[,,5]
kgmeatNcnty_17 <- kgmeatNcnty[,,5]
kgfixNcnty_17 <- kgfixNcnty[,,5]

#Adding the FIPS as the first col of each NANI component
kgfertNcnty_17_fips <- cbind(countydatalevel[,1],kgfertNcnty_17[,1:19])
kganimNintakecnty_17_fips <- cbind(countydatalevel[,1],kganimNintakecnty_17[,1:19])
kghumanNintakecnty_17_fips <- cbind(countydatalevel[,1],kghumanNintakecnty_17[,1])
kgcropNcnty_17_fips <- cbind(countydatalevel[,1],kgcropNcnty_17[,1:19])
kgmeatNcnty_17_fips <- cbind(countydatalevel[,1],kgmeatNcnty_17[,1:9])
kgfixNcnty_17_fips <- cbind(countydatalevel[,1],kgfixNcnty_17[,1:19])
kgatmNdepcnty_17_fips <- cbind(countydatalevel[,1],atmNdep_areainkm2[,3])

#Filtering out CB NANI components from all counties
#output as dataframe
CB_kgfertNcnty_17_fips <- kgfertNcnty_17_fips[kgfertNcnty_17_fips[,1]%in% CB_counties_FIPS,]
CB_kganimNintakecnty_17_fips <- kganimNintakecnty_17_fips[kganimNintakecnty_17_fips[,1]%in% CB_counties_FIPS,]
CB_kghumanNintakecnty_17_fips <- kghumanNintakecnty_17_fips[kghumanNintakecnty_17_fips[,1]%in% CB_counties_FIPS,]
CB_kgcropNcnty_17_fips <- kgcropNcnty_17_fips[kgcropNcnty_17_fips[,1]%in% CB_counties_FIPS,]
CB_kgmeatNcnty_17_fips <- kgmeatNcnty_17_fips[kgmeatNcnty_17_fips[,1]%in% CB_counties_FIPS,]
CB_kgfixNcnty_17_fips <- kgfixNcnty_17_fips[kgfixNcnty_17_fips[,1]%in% CB_counties_FIPS,]
CB_kgatmNdepcnty_17_fips <- kgatmNdepcnty_17_fips[kgatmNdepcnty_17_fips[,1]%in% CB_counties_FIPS,]

#The following outputs then processed in the excel. totals of each crops by counties multiplied with the concerned 
#cropscape coefficient to get results by discharge area unit 
write.csv(CB_kgcropNcnty_17_fips,"ZIA/NANI-NAPI/CB_kgcropNcnty_17_fips.csv")
write.csv(CB_kgfixNcnty_17_fips,"ZIA/NANI-NAPI/CB_kgfixNcnty_17_fips.csv")
write.csv(CB_kgfertNcnty_17_fips,"ZIA/NANI-NAPI/CB_kgfertNcnty_17_fips.csv")
write.csv(CB_kghumanNintakecnty_17_fips,"ZIA/NANI-NAPI/CB_kghumanNintakecnty_17_fips.csv")



# #Summing columns of each CB_NANI components to get the total
# CB_kgfertNcnty_17_fips$total <- rowSums(CB_kgfertNcnty_17_fips[, c(2:20)])
# CB_kganimNintakecnty_17_fips$total <- rowSums(CB_kganimNintakecnty_17_fips[, c(2:20)])
# CB_kgcropNcnty_17_fips$total <- rowSums(CB_kgcropNcnty_17_fips[, c(2:20)])
# CB_kgmeatNcnty_17_fips$total <- rowSums(CB_kgmeatNcnty_17_fips[, c(2:10)])
# CB_kgfixNcnty_17_fips$total <- rowSums(CB_kgfixNcnty_17_fips[, c(2:20)])
# 
# #Creating CB_NANI
# NANI_components <- cbind(CB_kgfertNcnty_17_fips[,1],CB_kgfertNcnty_17_fips[,21], CB_kgfixNcnty_17_fips[,21], CB_kgatmNdepcnty_17_fips[,2], CB_kganimNintakecnty_17_fips[,21],CB_kghumanNintakecnty_17_fips[,2],CB_kgcropNcnty_17_fips[,21],CB_kgmeatNcnty_17_fips[,11])
# colnames(NANI_components) <- c("FIPS_CB", "kgN_Fertilizer", "kgN_Fixation","kg_NOy_N","kganimalNintake", "kghumanNintake", "kgNincrop", "kgNinmeat")
# NANI_components <- as.data.frame(NANI_components) #Converting to data Frame to match math operations. 
# NANI_components$Netfoodandfeed <- (NANI_components$kganimalNintake+NANI_components$kghumanNintake)-(NANI_components$kgNincrop+NANI_components$kgNinmeat)
# NANI_components$NANI <- (NANI_components$kgN_Fertilizer+NANI_components$kgN_Fixation+NANI_components$kg_NOy_N+NANI_components$Netfoodandfeed)
# NANI_components$NANI_CB <- CB_countiesdischargearea*NANI_components[,10] #Multiplying cropped area to get the NANI of CB
# 
# 
# write.csv(NANI_components,"ZIA/CB_Counties_NANI_17.csv")


##Calculation of NANI/NAPI components by dicharge area

CB_DAU_kganimalNintake_17_matrix <- as.matrix(subset(CB_kganimNintakecnty_17_fips, select = -c(FIPS)))#Droping the FIPS Column and creating matrix
CB_DAU_kganimalNintake_17 <- t(as.matrix(CB_DF))%*%CB_DAU_kganimalNintake_17_matrix

CB_DAU_kganimalPintake_17_matrix <- as.matrix(subset(CB_kganimPintakecnty_17_fips, select = -c(FIPS)))#Droping the FIPS Column and creating matrix
CB_DAU_kganimalPintake_17 <- t(as.matrix(CB_DF))%*%CB_DAU_kganimalPintake_17_matrix

CB_DAU_kgmeatNcnty_17_matrix <- as.matrix(subset(CB_kgmeatNcnty_17_fips, select = -c(FIPS)))#Droping the FIPS Column and creating matrix
CB_DAU_kgmeatNcnty_17 <- t(as.matrix(CB_DF))%*%CB_DAU_kgmeatNcnty_17_matrix

CB_DAU_kgmeatPcnty_17_matrix <- as.matrix(subset(CB_kgmeatPcnty_17_fips, select = -c(FIPS)))#Droping the FIPS Column and creating matrix
CB_DAU_kgmeatPcnty_17 <- t(as.matrix(CB_DF))%*%CB_DAU_kgmeatPcnty_17_matrix

CB_DAU_NOy_17_matrix <- as.matrix(subset(CB_kgatmNdepcnty_17_fips, select = -c(FIPS)))#Droping the FIPS Column and creating matrix
CB_DAU_NOy_17 <- t(as.matrix(CB_DF))%*%CB_DAU_NOy_17_matrix


write.csv(CB_DAU_kganimalNintake_17,"ZIA/NANI-NAPI/CB_DAU_kganimalNintake_17.csv")
write.csv(CB_DAU_kganimalPintake_17,"ZIA/NANI-NAPI/CB_DAU_kganimalPintake_17.csv")
write.csv(CB_DAU_kgmeatNcnty_17,"ZIA/NANI-NAPI/CB_DAU_kgmeatNcnty_17.csv")
write.csv(CB_DAU_kgmeatPcnty_17,"ZIA/NANI-NAPI/CB_DAU_kgmeatPcnty_17.csv")
write.csv(CB_DAU_NOy_17,"ZIA/NANI-NAPI/CB_DAU_kgmeatPcnty_17.csv")

 
























