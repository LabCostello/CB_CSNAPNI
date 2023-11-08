#initialize constants for CreateInputs.R
if(print_tags == 1){
  print("CreateInputsSubs_CBW/constants.R")
}

library(readxl)
library(tidyverse)

countydatalevel <- read_xlsx("RawData/CommonDataLabels.xlsx") # This file contains county FIPS for the whole US. Important for filtering only CBW_Counties from NASS_County 

n_cnty = 197 #number of counties (in CBW)
n_ws_tbx = 1925 #number of Discharge Areas of CBW
n_crops = 20 #number of crops tracked (includes etoh coproducts)
n_anims = 19 #number of animal types tracked
n_meats = 9 #number of meat products tracked
data_yrs = 5 #number of years of data
year_labels = c(1997,2002,2007,2012,2017)

cnty_ws = t(array(0, c(n_ws_tbx,n_cnty)))

# Fresh information
lrs_shp <- read_excel("RawData/table_lrs_information.xls") # This file contains info from land river segments, their FIPS, names, acreage, basins. It comes from CAST Source Data.
CBW_lrs_shp <- lrs_shp[lrs_shp$Region=="Chesapeake Bay Watershed",] # Filtering the file to have only CBW counties
CBW_lrs_shp <- CBW_lrs_shp[order(CBW_lrs_shp$FIPS),] # Ascending order for FIPS

# Area of each Land River Segment based in the Acreage information available in the shapefile table provided from CAST
area <- as.matrix(unlist(CBW_lrs_shp$Acres*0.0040468564224, use.names = FALSE)) # Convert from acres to km2

# FIPS filtered considering only the LRS that are inside of the CBW
FIPS <- unique(CBW_lrs_shp$FIPS)

# Dictionary for assisting on identification of counties and LRS
FIPS_names <- countydatalevel[countydatalevel$FIPS %in% FIPS,][,1:3]

LRS_by_county <- cbind(OBJECTID2 = rownames(CBW_lrs_shp), CBW_lrs_shp) %>%
  group_by(FIPS) %>%
  summarize(OBJECTID2 = paste(OBJECTID2, collapse = ", "))

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

