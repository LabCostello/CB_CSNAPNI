# CreateInputs.R
## This file generates input files needed to run NANI and NAPI by loading and formatting data from various sources.
## All the source files are in the "Raw Data" folder.

if(print_tags == 1){
  print("ModelSubs/CreateInputs_CBW.R")
}

# Start the clock!
ptm <- proc.time()

# * = still needs to be updated for 2017

source("CreateInputsSubs_CBW/constants.r") # Add new counties (>203), add cropscape weighted variables

#####################################################################################
##County area data (from NANI Accounting Tool V3.1.0)
source("CreateInputsSubs_CBW/cntyarea_data.r") # Increase from 202 counties to more

#####################################################################################
##Corn disappearance proportions (from USDA ERS DATA)
source("CreateInputsSubs_CBW/cornuse_data.r")

#####################################################################################
##County population data (from NANI Accounting Tool V3.1.0, original source: US Census)
source("CreateInputsSubs_CBW/population_data.r") # *using 2012 for 2017 currently

#####################################################################################
##Census of agriculture data (crop harvested areas and production, animal average populations) 
#(from NASS QuickStats database, original source: USDA Ag Census)
source("CreateInputsSubs_CBW/NASSpull/NANI_NAPI_NASS_pull.r")
source("CreateInputsSubs_CBW/cropprod_data.r") #Check adding clipped area considering ratios for counties in CBW (done)
source("CreateInputsSubs_CBW/animpop_data.r") #Check adding clipped area considering ratios for counties in CBW (done)
source("CreateInputsSubs_CBW/harvestedarea_data.r") #Check adding clipped area considering ratios for counties in CBW (done)
if (grass_scenario ==1){source("CreateInputsSubs_CBW/grass_scenario.r")}
source("CreateInputsSubs_CBW/cropscape_cnty_da.r") # Check why there is a tiny difference between cropareacty[,17,5] and cropareaws[,17,5]

#####################################################################################
##Crop export data (from USDA NASS and GATS databases)
source("CreateInputsSubs_CBW/export_data.r") # Check if make sense to change that (specially for the netfood and feed, because here it considers that all produced in the region is consumed in the region, but that is not necessary)

#####################################################################################
##Crop allocation to animals (methods from Costello 2015)
source("CreateInputsSubs_CBW/AFIA_feed_allocation.r")
source("CreateInputsSubs_CBW/cropalloc_model.r") #Check adding clipped area considering ratios for counties in CBW

#####################################################################################
##P diet supplementation to animals (NEW methods)
source("CreateInputsSubs_CBW/P_diet_supp.r") #Check adding clipped area considering ratios for counties in CBW

#####################################################################################
##NANI totals from the VB model (from NANI Accounting Tool V3.1.0)
source("CreateInputsSubs_CBW/NANItotals_vbmodel.r") # *used data from 2012 for 2017, (2002 for 2012 and 2017 for atm N dep)

#####################################################################################
##NAPI totals from the VB model (from NAPI Accounting Tool V3.1.0)
source("CreateInputsSubs_CBW/NAPItotals_vbmodel.r") # *used data from 2012 for 2017 (CHECK)

#####################################################################################
##Meat production and emissions data (from original commodity-specific NANI for 2002)
source("CreateInputsSubs_CBW/meat_data.r") #Check adding clipped area considering ratios for counties in CBW
#add Mtrade data

#####################################################################################
##Ethanol production data (from USDA ERS)
source("CreateInputsSubs_CBW/etoh_data.r") #Check adding clipped area considering ratios for counties in CBW

#####################################################################################
##Crop fertilization rate data (from USDA ERS and NASS, and MN extension recommendations)
source("CreateInputsSubs_CBW/fertrates_data.r") #Check adding clipped area considering ratios for counties in CBW considering ratios for counties in CBW

#####################################################################################
##Food waste data (from original commodity-specific NANI for 2002)
#source("foodwaste_data.r") # *
#still needs to be scripted for years other than 02 and 07

# Stop the clock
runtime = proc.time() - ptm
runtime_msg = paste("input files created in", runtime, "seconds", sep = " ")
print(runtime_msg[3])
