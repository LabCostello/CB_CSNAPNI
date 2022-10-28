#initialize constants for CreateInputs.R
if(print_tags == 1){
  print("CreateInputsSubs_CBW/constants.R")
}

library(readxl)


countydatalevel <- read_xlsx("InputFiles_CBW/CommonDataLabels.xlsx") # This file contains county FIPS, and all CSNAPNI OUTPUT keys 
CB_counties <- read_xlsx("InputFiles_CBW/CB_Counties.xlsx") # this file contains CB data such as FIPS and atmospheric deposition data for all counties 

#converting the FIPS column to vector as a requirement for filtering from dataframe
CB_counties_FIPS <- as.vector(unlist(CB_counties[,1])) # Before it was 203 but included Fairfax city, Richmond
# #Adding the FIPS as the first col of each NAPI component
# n_ws_tbx_cbw <- cbind(countydatalevel[,1],cnty_ws[,1:450])
# 
# #Filtering out CB NAPI components from all counties
# #output as dataframe
# n_ws_tbx_cbw_fips <- n_ws_tbx_cbw[n_ws_tbx_cbw[,1]%in%CB_counties_FIPS,]
# 
# n_ws_tbx_cbw <- n_ws_tbx_cbw_fips[-c(1)]
# write.table(n_ws_tbx_cbw, 'InputFiles/cnty_ws_cbw.txt', append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE)

n_cnty = 203 #number of counties (in CBW)
n_ws_tbx = 1902 #number of Discharge Areas of CBW
n_crops = 20 #number of crops tracked (includes etoh coproducts)
n_anims = 19 #number of animal types tracked
n_meats = 9 #number of meat products tracked
data_yrs = 5 #number of years of data
year_labels = c(1997,2002,2007,2012,2017)

cnty_ws = t(array(scan('InputFiles_CBW/concordance_matrix_cbw.txt'), c(n_ws_tbx,n_cnty)))
area = t(array(scan('InputFiles_CBW/area_cbw_delivery_factors.txt'), c(1,n_ws_tbx))) #It is a multiplication of the (ratio of each DA area by the total 
                                                                                     #sum of DA area) and then multiplied by the total area of CBW
#conversion factors
bushelperton_corn = 39.368 #bushels / (metric)ton
bushelperton_sorghum = 39.368 #bushels / (metric)ton
bushelperton_barley = 45.9296 #bushels / (metric)ton
bushelperton_wheat = 36.7437 #bushels / (metric)ton
bushelperton_soybeans = 36.7437 #bushels / (metric)ton
bushelperton_oats = 64.8420 #bushels / (metric)ton
bushelperton_rye = 39.3680 #bushels / (metric)ton
lbsperkg = 2.20462 #lbs/kg
km2peracre = 0.00405 #km2/acre
literspergal = 3.78541 #liters/gallon

#etoh and coproducts from corn
CGF_from_corn = 0.22 #kg CGF from 1 kg wetmilled corn (O’Brien et al. 2010)
CGM_from_corn = 0.04 #kg CGM from 1 kg wetmilled corn (O’Brien et al. 2010)
DGS_from_corn = 0.28 #kg CGM from 1 kg drymilled corn (Bremer et al. 2010)
etoh_from_corn = array(0,c(6,1))
etoh_from_corn[1] = 1/2.63 #L etoh from 1 kg corn, 1987
etoh_from_corn[2] = 1/2.58 #L etoh from 1 kg corn, 1992
etoh_from_corn[3] = 1/2.53 #L etoh from 1 kg corn, 1997
etoh_from_corn[4] = 1/2.48 #L etoh from 1 kg corn, 2002
etoh_from_corn[5] = 1/2.43 #L etoh from 1 kg corn, 2007
etoh_from_corn[6] = 1/2.38 #L etoh from 1 kg corn, 2012

#allocation method
to_FC_drymill = array(0,c(5,1))
to_FC_wetmill = array(0,c(5,1))
#1=output masses
to_FC_drymill[1] = 0.49 
to_FC_wetmill[1] = 0.48
#2=energy content
to_FC_drymill[2] = 0.43
to_FC_wetmill[2] = 0.39
#3=market value
to_FC_drymill[3] = 0.24
to_FC_wetmill[3] = 0.30
#4=mass of corn grain used
to_FC_drymill[4] = 0.28
to_FC_wetmill[4] = 0.26
#5=no allocation to ethanol
to_FC_drymill[5] = 1
to_FC_wetmill[5] = 1
#6=no allocation to ethanol coproducts
to_FC_drymill[6] = 0
to_FC_wetmill[6] = 0

#allocation proportions between the wetmilled coproducts
wetmill_CGF = CGF_from_corn/(CGF_from_corn+CGM_from_corn)
wetmill_CGM = 1-wetmill_CGF

