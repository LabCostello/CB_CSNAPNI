# LoadData.R
## This file loads all of the input files for CSNAPNI. 
## The input files are created using CreateInputs.R.

if(print_tags == 1){
  print("ModelSubs/LoadData_CBW.R")
}
library(readxl)
n_cnty = 197 #number of counties (in continental US)
n_ws_NEEA = 1925 #number of NEEA watersheds
n_ws_tbx = 1925 #number of watersheds in NANI Toolbox watershed set
n_crops = 20 #number of crops tracked (includes etoh coproducts)
n_anims = 19 #number of animal types tracked
n_meats = 9 #number of meat products tracked

#COUNTY-WATERSHED INTERSECTION
lrs_shp <- read_excel("RawData/table_lrs_information.xls") # This file contains info from land river segments, their FIPS, names, acreage, basins. It comes from CAST Source Data.
CBW_lrs_shp <- lrs_shp[lrs_shp$Region=="Chesapeake Bay Watershed",] # Filtering the file to have only CBW counties
CBW_lrs_shp <- CBW_lrs_shp[order(CBW_lrs_shp$FIPS),] # Ascending order for FIPS
area <- as.matrix(unlist(CBW_lrs_shp$Acres*0.0040468564224, use.names = FALSE)) # Convert from acres to km2

#wsNum = t(array(scan('InputFiles_CBW/wsNum.txt'), c(2,n_ws_tbx))) #key for 450 watersheds-->NEEA watersheds, region codes also included
wsNum = t(array(1:n_ws_tbx, c(1,n_ws_tbx)))

#CORN USE
#percentage values for allocating corn production to various uses in load years
#key is in cornuse_key.txt
cornuse = t(array(scan("InputFiles_CBW/cornuse.txt"), c(nyrs,11)))

#POPULATION
#human population densities (people/km2) in watersheds (450 set)
#key is in population_key.txt
population = t(array(scan("InputFiles_CBW/population.txt"), c(nyrs,n_ws_tbx)))
population_cnty = t(array(scan("InputFiles_CBW/population_cnty.txt"), c(nyrs,n_cnty)))

#CROP PRODUCTION
#crop production densities (kg/km2) in watersheds (450 set)
#key is in cropprod_key.txt
cropprod=array(0,c(n_ws_tbx,n_crops,nyrs))
cropprodcnty=array(0,c(n_cnty,n_crops,nyrs))
for(n in 1:length(run_yrs)){
  file_name = paste("InputFiles_CBW/cropprod",run_yrs[n],".txt",sep = "")
  cropprod[,,n]=t(array(scan(file_name), c(n_crops,n_ws_tbx)))
  file_name = paste("InputFiles_CBW/cropprodcnty",run_yrs[n],".txt",sep = "")
  cropprodcnty[,,n]=t(array(scan(file_name), c(n_crops,n_cnty)))
}
#key is in cornprodnoetoh_key.txt
cornprodnoetoh = t(array(scan("InputFiles_CBW/cornprodnoetoh.txt"), c(nyrs,n_cnty)))
#key is in etohproddensws_key.txt
etohproddensws = t(array(scan("InputFiles_CBW/etohproddensws.txt"), c(nyrs,n_ws_tbx)))

#ANIMAL POPULATION
#animal populations in watersheds (450 set)
#key is in noanimdyn_key.txt
noanimdyn=array(0,c(n_ws_tbx,19,nyrs))
for(n in 1:nyrs){
  file_name = paste("InputFiles_CBW/noanimdyn",run_yrs[n],".txt",sep = "")
  noanimdyn[,,n]=t(array(scan(file_name), c(n_anims,n_ws_tbx)))
}
#animal populations in counties
#key is in noanimdyncty_key.txt
noanimdyncty=array(0,c(n_cnty,n_anims,nyrs))
for(n in 1:nyrs){
  file_name = paste("InputFiles_CBW/noanimdyncty",run_yrs[n],".txt",sep = "")
  noanimdyncty[,,n]=t(array(scan(file_name), c(n_anims,n_cnty)))
}

#CROP EXPORTS
#export quantities (kg) for crops and ethanol feed coproducts
#key is in exports_key.txt
exports = t(array(scan("InputFiles_CBW/exports.txt"), c(nyrs,n_crops)))

#CROP DATA
#crop and ethanol feed coproduct data
#key is in cropdata_key.txt
cropdata = t(array(scan("InputFiles_CBW/cropdata.txt"), c(18,n_crops)))

#N-BASED CROP ALLOCATION TO ANIMALS
#kg N from each crop per animal in each livestock category
#key is croptoanim_key.txt
cropNtoanim=array(0,c(n_anims,n_crops,nyrs))
for(n in 1:nyrs){
  file_name = paste("InputFiles_CBW/cropNtoanim",run_yrs[n],".txt",sep = "")
  cropNtoanim[,,n]=t(array(scan(file_name), c(n_crops,n_anims)))
}
#kg P from each crop per animal in each livestock category, using N-requirement-based allocation
#key is croptoanim_key.txt
cropPtoanim=array(0,c(n_anims,n_crops,nyrs))
for(n in 1:nyrs){
  file_name = paste("InputFiles_CBW/cropPtoanim",run_yrs[n],".txt",sep = "")
  cropPtoanim[,,n]=t(array(scan(file_name), c(n_crops,n_anims)))
}
file_name = paste("InputFiles_CBW/Psupp_animtotals.txt",sep = "")
Psupp_animtotals=t(array(scan(file_name), c(nyrs,n_anims)))
file_name = paste("InputFiles_CBW/Psupp_peranim.txt",sep = "")
Psupp_peranim=t(array(scan(file_name), c(nyrs,n_anims)))

#NANI TOTALS FROM VB MODEL
#Atm_N_Dep in kg/km2/yr (column 1)
#Fert_N_App in kgN/km2/yr (columns 2, 3, 4)
#Ag_N_Fix in kgN/km2/yr (columns 5, 6, 7, 8, 9, 10, 11)
#Food_Feed_N in kgN/km2/yr (column 12)
#Non_Food_Crops in kgN/km2/yr (columns 13, 14, 15)
#key is in NANIdata_key.txt
NANIdata=array(0,c(n_ws_tbx,15,nyrs))
for(n in 1:nyrs){
  file_name = paste("InputFiles_CBW/NANIdata",run_yrs[n],".txt",sep = "")
  NANIdata[,,n]=t(array(scan(file_name), c(15,n_ws_tbx)))
}

#NAPI TOTALS FROM VB MODEL
#Fert_P_App in kgP/km2/yr (columns 1, 2, 3)
#Food_Feed_P in kgP/km2/yr (column 4)
#Non_Food_Crops in kgP/km2/yr (columns 5, 6, 7)
#key is in NAPIdata_key.txt
NAPIdata=array(0,c(n_ws_tbx,7,nyrs))
for(n in 1:nyrs){
  file_name = paste("InputFiles_CBW/NAPIdata",run_yrs[n],".txt",sep = "")
  NAPIdata[,,n]=t(array(scan(file_name), c(7,n_ws_tbx)))
}

#HARVESTED AREA
#crop-specific harvested area data from the US agricultural census in km2
#key is in cropareaharvested_key.txt
croparea = t(array(scan("InputFiles_CBW/cropareaharvested.txt"), c(nyrs,n_crops)))
cropareacnty=array(0,c(n_cnty,n_crops,nyrs))
for(n in 1:length(run_yrs)){
  file_name = paste("InputFiles_CBW/cropareaharvestedcnty",run_yrs[n],".txt",sep = "")
  cropareacnty[,,n]=t(array(scan(file_name), c(n_crops,n_cnty)))
}
#ethanol's virtual land use in each year
etoh_landuse = t(array(scan("InputFiles_CBW/etoh_landuse_harvestedarea.txt")))

#CROP DATA
#citations in cropdata_master.xlsx
#key in cropdata_key.txt
cropdata = t(array(scan("InputFiles_CBW/cropdata.txt"), c(18,20)))

#ANIMAL DATA
# key is in animdatadyn_master.txt (in RawData)
# animdatadyn version is selected and imported in Settings.R

#FERTILIZER DATA
#N and P2O5 fertilization rates for crops
#See Nfert_key.txt and Pfert_key.txt
Nfert = t(array(scan('InputFiles_CBW/Nfert.txt'),c(nyrs,n_crops-3)))
Pfert = t(array(scan('InputFiles_CBW/Pfert.txt'),c(nyrs,n_crops-3)))

#STATE DATA
#state FIPS code for each county in animal data
FIPS <-  unique(CBW_lrs_shp$FIPS)
FIPSws <- CBW_lrs_shp$FIPS
FPs <-  array(0,c(n_cnty))
for (i in 1:197) {FPs[i] <- (as.numeric(substr(as.character(unique(FIPS)[i]), 1, 2)))}

#percent of manure from confinement and recovery by state
manurefactor = t(array(scan("InputFiles_CBW/manurefactor.txt"),c(n_anims,50))) #data from 2002 only

#MEAT DATA
#Meat production estimations. See meat_data.r for details.
#Key in meatprod_key.txt
meatprod = t(array(scan("InputFiles_CBW/meatprod.txt"),c(nyrs,n_meats))) #Meat emissions estimations for 2002 and 2007 only
#Key in meatemissions_key.txt
meatemissions = t(array(scan("InputFiles_CBW/meatemissions.txt"),c(8,n_meats)))
# key is in meatdata_key.txt
meatdata = t(array(scan("InputFiles_CBW/meatdata.txt"), c(9,9)))
Mtrade = t(array(scan("InputFiles_CBW/Mtrade.txt"),c(12,9)))  
# first 2 columns: net percent of domestic production that is traded. negative 
# values indicate that exports are larger than imports
# (positive value = imports greater than exports)
# 3rd and 4th col: # of domestic production (not including
# beginning stocks) exported, 2002 and 2007
#Fill in zeros for missing data years:
Mtrade[,10:11]=Mtrade[,3:4]
Mtrade[,4:5]=Mtrade[,1:2]
Mtrade[,1:3]=0
Mtrade[,6:9]=0
Mtrade[,12]=0

#ETOH PRODUCTION
#Ethanol production in each data year
#key in etohprod_key.txt
etohprod = t(array(scan("InputFiles_CBW/etohprod.txt"),c(nyrs,1)))

#FOOD WASTE
foodwaste = t(array(scan("InputFiles_CBW/foodwaste.txt"),c(12,9))) #estimations for 2002 and 2007 only
#col 1: 02 industrial loss (for meat % of edible weight lost at industrial level
#col 2: 07 industrial loss
#col 3: 02 consumer loss
#col 4: 07 consumer loss
#rows 1:9, animal products: beef, dairy, pork,
#sheep/lamb, horse (0), layers(eggs), broilers,
#turkey, goats
#rows 10:24, corn grain, corn silage, wheat, oats,
#barley, sorghum grain, sorghum silage, potatoes, rye,
#alfalfa hay, other hay, soybeans, cropland pasture,
#noncropland pasture, rice 
#TEMPORARY FILL-IN
foodwaste[,10:11]=foodwaste[,3:4]
foodwaste[,4:5]=foodwaste[,1:2]
foodwaste[,1:3]=0
foodwaste[,6:9]=0
foodwaste[,12]=0

