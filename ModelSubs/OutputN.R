output_lrs_coefficients <- read_excel("RawData/SourceData.xlsx", sheet = "DeliveryFactors")

#Filter the ones that is just in CBW
output_lrs_coefficients <- output_lrs_coefficients[output_lrs_coefficients$LandRiverSegment %in% CBW_lrs_shp$LndRvrSeg,]

LU_for_lrs_outputcoef <- unique(output_lrs_coefficients$LoadSource) 

list_LU <- LU_for_lrs_outputcoef[c(2:14)]

output_lrs_coef_filt <- output_lrs_coefficients[output_lrs_coefficients$LoadSource %in% list_LU,]

out_lrs_N <- data.frame("LRS" = output_lrs_coef_filt[,1],
                        "LU" = output_lrs_coef_filt[,2],
                        "NOut" = output_lrs_coef_filt[,3]*output_lrs_coef_filt[,6]*output_lrs_coef_filt[,9])

out_lrs_N$CombinedColumn <- paste(out_lrs_N$LandRiverSegment,out_lrs_N$LandToWater_TN_Factor,sep = "_")

not_duplicated_rows <- out_lrs_N[!duplicated(out_lrs_N$CombinedColumn), ] # There are some LRS that has 2 coefficients and others that just one for all land uses

# Changing name of Load Source
not_duplicated_rows$LoadSource[not_duplicated_rows$LoadSource=="Full Season Soybeans"] <- "Grains&Silage"
not_duplicated_rows$LoadSource[not_duplicated_rows$LoadSource=="Leguminous Hay"] <- "Hay&Pasture"

Nout_lrs <- not_duplicated_rows %>% group_by(LandRiverSegment) %>% summarize(mean(LandToWater_TN_Factor))

Nout_lrs <- Nout_lrs[order(match(Nout_lrs$LandRiverSegment,CBW_lrs_shp$LndRvrSeg)),]

RvrNexport <- NANIBtotsum[,6]*Nout_lrs$`mean(LandToWater_TN_Factor)`
RvrNexport2 <- (TNsum[,6]-rowSums(CNprod[,,6]))*Nout_lrs$`mean(LandToWater_TN_Factor)`

RvrNexportarea <- RvrNexport/area[,1]
RvrNexportarea <- RvrNexportarea[RvrNexportarea>0]

# Commodity portion ####
#Synthetic fertilizer
(CfertNinorgtotlrs)

# Manure fertilizer
kgNmanuretoeachcrop+kgmanurenotavailabletoeachcrop

((CfertNinorgtotlrs+kgNmanuretoeachcrop+kgmanurenotavailabletoeachcrop)[,,6])

CfertNtotlrs[,,6]
# Calculate how much is uptaken by plant, do the subtraction of the total N applied to the plant - uptake
NInputs_crops_lrs <- (((CfertNinorgtotlrs+kgNmanuretoeachcrop+kgmanurenotavailabletoeachcrop)[,,6])-CfertNtotlrs[,,6])
#NInputs_crops_lrs[,c(10,11,13,16)] <- CfixNwswE[,c(10,11,13,16),6]-CNprod[,c(10,11,13,16),6] # For the N fixing crops
DF <- not_duplicated_rows

class_crop_df <- c(rep("Grains&Silage",9),rep("Hay&Pasture",2),"Grains&Silage",rep("Hay&Pasture",2),rep("Grains&Silage",7))

# Making a for loop on each line for each column of NLoads and based on class_crop_df define if it will yse DF$Grains&Silage or DF$Hay&Pasture
LRS_vector <- unique(DF$LandRiverSegment)
LRS_vector <- LRS_vector[order(match(LRS_vector,CBW_lrs_shp$LndRvrSeg))]

NLoads_lrs <- matrix(NA,nrow = nrow(NInputs_crops_lrs),ncol = ncol(NInputs_crops_lrs))

# for(i in 1:nrow(NInputs_crops_lrs)){
#   for(j in 1:ncol(NInputs_crops_lrs)){
#     if(class_crop_df[j]=="Grains&Silage"){
#       NLoads_lrs[i,j] <- NInputs_crops_lrs[i,j]*DF$LandToWater_TN_Factor[DF$LandRiverSegment==LRS_vector[i] & DF$LoadSource=="Grains&Silage"]
#     } else if(class_crop_df[j]=="Hay&Pasture"){
#       # Try to get the Hay&Pasture factor
#       hay_factor <- DF$LandToWater_TN_Factor[DF$LandRiverSegment==LRS_vector[i] & DF$LoadSource=="Hay&Pasture"]
#       
#       # If no matching row found for Hay&Pasture, use Grains&Silage factor
#       if(length(hay_factor) == 0) {
#         hay_factor <- DF$LandToWater_TN_Factor[DF$LandRiverSegment==LRS_vector[i] & DF$LoadSource=="Grains&Silage"]
#       }
#       
#       NLoads_lrs[i,j] <- NInputs_crops_lrs[i,j] * hay_factor
#     }
#   }
# }

# Modified loop with BMP factors
for(i in 1:nrow(NInputs_crops_lrs)){
  for(j in 1:ncol(NInputs_crops_lrs)){
    
    # Calculate base N load
    base_load <- 0
    
    if(class_crop_df[j]=="Grains&Silage"){
      base_load <- NInputs_crops_lrs[i,j] * DF$LandToWater_TN_Factor[DF$LandRiverSegment==LRS_vector[i] & DF$LoadSource=="Grains&Silage"]
    } else if(class_crop_df[j]=="Hay&Pasture"){
      # Try to get the Hay&Pasture factor
      hay_factor <- DF$LandToWater_TN_Factor[DF$LandRiverSegment==LRS_vector[i] & DF$LoadSource=="Hay&Pasture"]
      
      # If no matching row found for Hay&Pasture, use Grains&Silage factor
      if(length(hay_factor) == 0) {
        hay_factor <- DF$LandToWater_TN_Factor[DF$LandRiverSegment==LRS_vector[i] & DF$LoadSource=="Grains&Silage"]
      }
      
      base_load <- NInputs_crops_lrs[i,j] * hay_factor
    }
    
    # Apply BMP factors only for corn (j=1) and only if grass or winter rye scenarios are active
    if(j %in% c(1,2)) {  # Corn is the first crop (j=1)
      # Apply only grass BMP factor
      NLoads_lrs[i,j] <- 0.6*base_load + (0.4*base_load * grass_bmp_factor)
      # Apply only wr BMP factor
      NLoads_lrs[i,j] <- NLoads_lrs[i,j] * 0.3 * wr_bmp_factor
    }
    else {
      # For other crops, no BMP factor applied
      NLoads_lrs[i,j] <- base_load
    }
  }
}

# Sum all the N lost to streams over the region
NLoads_lrs_sum <- colSums(NLoads_lrs)
# Do the division of the amount of crop N output by the amount of crop produced in the region kg N output/kg crop produced
NLoads_lrs_sum_kg <- NLoads_lrs_sum/colSums(CkgwE)[,6]
NLoads_lrs_sum_kg[is.na(NLoads_lrs_sum_kg)] <- 0
NLoads_lrs_sum_g <- NLoads_lrs_sum_kg*1000 # in g N output/kg crop produced

# Based on the diet of the animal calculate the N output
NLoads_anim_kgdom <- NLoads_anim_kgimp <- matrix(0,nrow = nrow(feedpermeatdom),ncol = ncol(feedpermeatdom))

for(i in 1:9){
  NLoads_anim_kgdom[i,] <- feedpermeatdom[i,,6]*NLoads_lrs_sum_kg
  NLoads_anim_kgimp[i,] <-  (feedpermeatimp[i,,6]*unitfertNCnational[,6]*0.25)
}
# Sum the N output from domestic and imported feed
rowSums(NLoads_anim_kgdom+NLoads_anim_kgimp)

# Calculate the N output from the manure management portion (Considering all this manure would volatilize would volatilize)
NInputs_mannrcv_kg <- data.frame(LRS=CBW_lrs_shp$LndRvrSeg,ST=CBW_lrs_shp$ST,Nonrecoveman=kgmanureNlrs[,,6]-kgmanureNrec450[,,6])
NInputs_mannrcv_kg <- NInputs_mannrcv_kg %>%
  mutate(Emitted_to_tidal = case_when(
    ST == "DE" ~ 0.0527,
    ST == "MD" ~ 0.1117,
    ST == "NY" ~ 0.0243,
    ST == "PA" ~ 0.067,
    ST == "VA" ~ 0.0893,
    ST == "WV" ~ 0.0496,
    ST == "DC" ~ 0,  # Add value for DC
    TRUE ~ NA_real_  # Default for any unmatched states
  ))

nonrecoveman_cols <- grep("^Nonrecoveman", names(NInputs_mannrcv_kg), value = TRUE)
NLoads_mannrcv_kg<- NInputs_mannrcv_kg[nonrecoveman_cols] * NInputs_mannrcv_kg$Emitted_to_tidal
tot_NLoads_mannrcv_kg <- data.frame(beef=sum(colSums(NLoads_mannrcv_kg)[c(1,10,11,13,15)]),
                                    dairy=sum(colSums(NLoads_mannrcv_kg)[c(2,12,14,16)]),
                                    pork=sum(colSums(NLoads_mannrcv_kg)[c(3,4)]),
                                    sheep=0,
                                    horse=0,
                                    layers=colSums(NLoads_mannrcv_kg)[5],
                                    broilers=sum(colSums(NLoads_mannrcv_kg)[c(7,8)]),
                                    turkeys=sum(colSums(NLoads_mannrcv_kg)[c(6,9)]),
                                    goats=0)

NLoads_mannrcv_kgpermeat <- tot_NLoads_mannrcv_kg/totmeat[6,]
NLoads_mannrcv_kgpermeat[is.na(NLoads_mannrcv_kgpermeat)] <- 0

# Sum the portion of N
Nloads_anim_kgpermeatcbw <- (rowSums(NLoads_anim_kgdom+NLoads_anim_kgimp)+NLoads_mannrcv_kgpermeat)

Nloads_anim_kgperprotcbw <- (rowSums(NLoads_anim_kgdom+NLoads_anim_kgimp)+NLoads_mannrcv_kgpermeat)/ (meatdata[,z] / 1000)
Nloads_anim_kgperprotcbw[is.na(Nloads_anim_kgperprotcbw)] <- 0
