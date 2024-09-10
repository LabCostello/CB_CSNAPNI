#fertrates_data.r
#This script pulls the crop fertilization rates from a summary spreadsheet, cropfertrates.xlsx, and converts the values from lbs/acre to kg/km

if(print_tags == 1){
  print("CreateInputsSubs_CBW/fertrates_data.R")
}

read_file = 'RawData/cropfertrates.xlsx' #these areas are planted areas 
import_yrs6 = c("1997","2002","2007","2012","2017") #this file contains data from years, 1997-2017
read_sheet1 = 'Nfert'
read_sheet2 = 'Pfert'

data1 = as.matrix(read_excel(read_file, sheet = read_sheet1)) #nfert
data2 = as.matrix(read_excel(read_file, sheet = read_sheet2)) #pfert

data_range1 = data1[2:17,4:8] #exclude some cells
data_range2 = data2[2:17,4:8] #exclude some cells

Nfert_lbsperacre=Nfert_lbsperacre1=data_range1[1:length(data_range1[,1]),]
Pfert_lbsperacre=Pfert_lbsperacre1=data_range2[1:length(data_range2[,1]),]

if (fertassump==2 & exists("fertilizer_nass")==FALSE) { # NASS Survey values
  Nfert_lbsperacre2=cbind(data_range1[1:length(data_range1[,1]),],rep(0,16))
  
  library(rnassqs)
  library(dbplyr)
  
  options(scipen = 999)
  
  nassqs_auth(key = "714CEBBE-DB96-3065-9829-443B40CEAB84")
  #c("BARLEY","CORN","OATS","SOYBEANS","WHEAT","POTATOES")
  # Set the API parameters
  params <- list(
    state_alpha = c("PA","VA","MD","WV","DE","NY"),
    domaincat_desc = "FERTILIZER: (NITROGEN)"
  )
  
  fertilizer_nass <- nassqs(params)
  
  # Corn (Consider %area planted as well)
  corn_fert_nass <- (fertilizer_nass[fertilizer_nass$commodity_desc=="CORN"&fertilizer_nass$unit_desc=="LB / ACRE / YEAR, AVG",] %>% arrange(year))[,c(10,30,31,38)]
  Nfert_lbsperacre2[1,] <- Nfert_lbsperacre2[2,] <- c(mean(corn_fert_nass[10:11,4]), #1997
                                                      mean(corn_fert_nass[14:17,4]), #2002
                                                      mean(corn_fert_nass[18:19,4]), #2007
                                                      mean(corn_fert_nass[20:22,4]), #2012
                                                      mean(corn_fert_nass[25:28,4]), #2017
                                                      mean(corn_fert_nass[29:30,4])) #2022
  
  # Wheat
  wheat_fert_nass <- (fertilizer_nass[fertilizer_nass$commodity_desc=="WHEAT"&fertilizer_nass$unit_desc=="LB / ACRE / YEAR, AVG",] %>% arrange(year))[,c(10,30,31,38)]
  Nfert_lbsperacre2[3,] <- rep(wheat_fert_nass[,4],6)
  
  # Oats
  oats_fert_nass <- (fertilizer_nass[fertilizer_nass$commodity_desc=="OATS"&fertilizer_nass$unit_desc=="LB / ACRE / YEAR, AVG",] %>% arrange(year))[,c(10,30,31,38)]
  Nfert_lbsperacre2[4,] <- c(mean(oats_fert_nass[1:2,4]), #1997
                             mean(oats_fert_nass[1:2,4]), #2002
                             mean(oats_fert_nass[1:2,4]), #2007
                             mean(oats_fert_nass[3:4,4]), #2012
                             mean(oats_fert_nass[3:4,4]), #2017
                             mean(oats_fert_nass[5:6,4])) #2022
  
  # Barley
  barley_fert_nass <- (fertilizer_nass[fertilizer_nass$commodity_desc=="BARLEY"&fertilizer_nass$unit_desc=="LB / ACRE / YEAR, AVG",] %>% arrange(year))[,c(10,30,31,38)]
  Nfert_lbsperacre2[5,] <- c(barley_fert_nass[1,4],         #1997
                             barley_fert_nass[1,4],         #2002
                             barley_fert_nass[1,4],         #2007
                             mean(barley_fert_nass[2:3,4]), #2012
                             mean(barley_fert_nass[4:5,4]), #2017
                             mean(barley_fert_nass[6:7,4])) #2022
  
  # Soybeans
  soybeans_fert_nass <- (fertilizer_nass[fertilizer_nass$commodity_desc=="SOYBEANS"&fertilizer_nass$unit_desc=="LB / ACRE / YEAR, AVG",] %>% arrange(year))[,c(10,30,31,38)]
  Nfert_lbsperacre2[12,] <- c(mean(soybeans_fert_nass[6:7,4]),  #1997
                             mean(soybeans_fert_nass[9:10,4]),  #2002
                             soybeans_fert_nass[11,4],          #2007
                             mean(soybeans_fert_nass[12,4]), #2012
                             soybeans_fert_nass[14,4],          #2017
                             mean(soybeans_fert_nass[15,4])) #2022
  
  # Potatoes
  potatoes_fert_nass <- (fertilizer_nass[fertilizer_nass$commodity_desc=="POTATOES"&fertilizer_nass$unit_desc=="LB / ACRE / YEAR, AVG",] %>% arrange(year))[,c(10,30,31,38)]
  Nfert_lbsperacre2[8,] <- c(mean(potatoes_fert_nass[13:14,4]), #1997
                             potatoes_fert_nass[15,4],    #2002
                             potatoes_fert_nass[15,4],    #2007
                             potatoes_fert_nass[15,4],    #2012
                             potatoes_fert_nass[15,4],    #2017
                             potatoes_fert_nass[15,4])    #2022
  
  # Consolidating results
  Nfert_lbsperacre=Nfert_lbsperacre2
  Pfert_lbsperacre=Pfert_lbsperacre2=data_range2[1:length(data_range2[,1]),]
  Nfert_lbsperacre[c(6,7,9,10,11,15,16),6] <- Nfert_lbsperacre[c(6,7,9,10,11,15,16),5]
}
if (fertassump==2) {
Nfert_lbsperacre=Nfert_lbsperacre2
Nfert_lbsperacre[c(6,7,9,10,11,15,16),6] <- Nfert_lbsperacre[c(6,7,9,10,11,15,16),5]
}
if (fertassump==3) {
  # Yield for the following crops: corn grain, silage, wheat, oats, barley, sorghum grain and silage, rye, alfalfa, other hay
  corng_y <- c(mean(NASS_County[,2]/NASS_County[,1],na.rm=TRUE),
               mean(NASS_County[,53]/NASS_County[,52],na.rm=TRUE),
               mean(((NASS_County[,104]/NASS_County[,103])[!is.infinite(NASS_County[,104]/NASS_County[,103])]),na.rm=TRUE),
               mean(NASS_County[,155]/NASS_County[,154],na.rm=TRUE),
               mean(NASS_County[,206]/NASS_County[,205],na.rm=TRUE))
  
  corns_y <- c(mean(NASS_County[,4]/NASS_County[,3],na.rm=TRUE),
               mean(NASS_County[,55]/NASS_County[,54],na.rm=TRUE),
               mean(((NASS_County[,106]/NASS_County[,105])[!is.infinite(NASS_County[,106]/NASS_County[,105])]),na.rm=TRUE),
               mean(NASS_County[,157]/NASS_County[,156],na.rm=TRUE),
               mean((NASS_County[,208]/NASS_County[,207])[!is.infinite(NASS_County[,208]/NASS_County[,207])],na.rm=TRUE))
  
  wheat_y <- c(mean(NASS_County[,6]/NASS_County[,5],na.rm=TRUE),
               mean(((NASS_County[,57]/NASS_County[,56])[!is.infinite(NASS_County[,57]/NASS_County[,56])]),na.rm=TRUE),
               mean(((NASS_County[,108]/NASS_County[,107])[!is.infinite(NASS_County[,108]/NASS_County[,107])]),na.rm=TRUE),
               mean(NASS_County[,159]/NASS_County[,158],na.rm=TRUE),
               mean(((NASS_County[,210]/NASS_County[,209])[!is.infinite(NASS_County[,210]/NASS_County[,209])]),na.rm=TRUE))
  
  oats_y <- c(mean(NASS_County[,8]/NASS_County[,7],na.rm=TRUE),
              mean(((NASS_County[,59]/NASS_County[,58])[!is.infinite(NASS_County[,59]/NASS_County[,58])]),na.rm=TRUE),
              mean(((NASS_County[,110]/NASS_County[,109])[!is.infinite(NASS_County[,110]/NASS_County[,109])]),na.rm=TRUE),
              mean(((NASS_County[,161]/NASS_County[,160])[!is.infinite(NASS_County[,161]/NASS_County[,160])]),na.rm=TRUE),
              mean(((NASS_County[,212]/NASS_County[,211])[!is.infinite(NASS_County[,212]/NASS_County[,211])]),na.rm=TRUE))
  
  barley_y <- c(mean(NASS_County[,10]/NASS_County[,9],na.rm=TRUE),
                mean(((NASS_County[,61]/NASS_County[,60])[!is.infinite(NASS_County[,61]/NASS_County[,60])]),na.rm=TRUE),
                mean(((NASS_County[,112]/NASS_County[,111])[!is.infinite(NASS_County[,112]/NASS_County[,111])]),na.rm=TRUE),
                mean(((NASS_County[,163]/NASS_County[,162])[!is.infinite(NASS_County[,163]/NASS_County[,162])]),na.rm=TRUE),
                mean(((NASS_County[,214]/NASS_County[,213])[!is.infinite(NASS_County[,214]/NASS_County[,213])]),na.rm=TRUE))
  
  sorghumg_y <- c(mean(NASS_County[,12]/NASS_County[,11],na.rm=TRUE),
                  mean(((NASS_County[,63]/NASS_County[,62])[!is.infinite(NASS_County[,63]/NASS_County[,62])]),na.rm=TRUE),
                  mean(((NASS_County[,114]/NASS_County[,113])[!is.infinite(NASS_County[,114]/NASS_County[,113])]),na.rm=TRUE),
                  mean(((NASS_County[,165]/NASS_County[,164])[!is.infinite(NASS_County[,165]/NASS_County[,164])]),na.rm=TRUE),
                  mean(((NASS_County[,216]/NASS_County[,215])[!is.infinite(NASS_County[,216]/NASS_County[,215])]),na.rm=TRUE))
  
  sorghums_y <- c(mean(NASS_County[,14]/NASS_County[,13],na.rm=TRUE),
                  mean(((NASS_County[,65]/NASS_County[,64])[!is.infinite(NASS_County[,65]/NASS_County[,64])]),na.rm=TRUE),
                  mean(((NASS_County[,116]/NASS_County[,115])[!is.infinite(NASS_County[,116]/NASS_County[,115])]),na.rm=TRUE),
                  mean(((NASS_County[,167]/NASS_County[,166])[!is.infinite(NASS_County[,167]/NASS_County[,166])]),na.rm=TRUE),
                  mean(((NASS_County[,218]/NASS_County[,217])[!is.infinite(NASS_County[,218]/NASS_County[,217])]),na.rm=TRUE))
  
  rye_y <- c(mean(((NASS_County[,18]/NASS_County[,17])[!is.infinite(NASS_County[,18]/NASS_County[,17])]),na.rm=TRUE),
             mean(((NASS_County[,69]/NASS_County[,68])[!is.infinite(NASS_County[,69]/NASS_County[,68])]),na.rm=TRUE),
             mean(((NASS_County[,120]/NASS_County[,119])[!is.infinite(NASS_County[,120]/NASS_County[,119])]),na.rm=TRUE),
             mean(((NASS_County[,171]/NASS_County[,170])[!is.infinite(NASS_County[,171]/NASS_County[,170])]),na.rm=TRUE),
             mean(((NASS_County[,222]/NASS_County[,221])[!is.infinite(NASS_County[,222]/NASS_County[,221])]),na.rm=TRUE))
  
  
  
  
  # List of Pound of Nitrogen/Phosphorus recommended per crop (PSU Ag. Guide) - corn grain, silage, wheat, oats, barley, sorghum grain and silage, rye
  psu_ag_guide_pN <- c(1, 7, 1, 0.8, 0.8, 0.75, 7, 1)
  
  psu_ag_guide_pP <- c(1, 7, 1, 0.8, 0.8, 0.75, 7, 1)
  
  N_recommendation_PSU <- data.frame(Corng = corng_y*psu_ag_guide_pN[1],
                                     Corns = corns_y*psu_ag_guide_pN[2],
                                     Wheat = wheat_y*psu_ag_guide_pN[3],
                                     Oats = oats_y*psu_ag_guide_pN[4],
                                     Barley = barley_y*psu_ag_guide_pN[5],
                                     Sorghumg = sorghumg_y*psu_ag_guide_pN[6],
                                     Sorghums = sorghums_y*psu_ag_guide_pN[7],
                                     Rye = rye_y*psu_ag_guide_pN[8],
                                     row.names = c(1997,2002,2007,2012,2017))
  
  N_recommendation_PSU <- as.matrix(t(N_recommendation_PSU))
  
  P_recommendation_PSU <- data.frame(Corng = corng_y*psu_ag_guide_pP[1],
                                     Corns = corns_y*psu_ag_guide_pP[2],
                                     Wheat = wheat_y*psu_ag_guide_pP[3],
                                     Oats = oats_y*psu_ag_guide_pP[4],
                                     Barley = barley_y*psu_ag_guide_pP[5],
                                     Sorghumg = sorghumg_y*psu_ag_guide_pP[6],
                                     Sorghums = sorghums_y*psu_ag_guide_pP[7],
                                     Rye = rye_y*psu_ag_guide_pP[8],
                                     row.names = c(1997,2002,2007,2012,2017))
  
  P_recommendation_PSU <- as.matrix(t(P_recommendation_PSU))
  
  # It was changed the amount of fertilizer per crop for some of the crops
  Nfert_lbsperacre3=data_range1[1:length(data_range1[,1]),]
  Nfert_lbsperacre3[c(1:7,9),] <- N_recommendation_PSU[1:8,]
  Nfert_lbsperacre <- Nfert_lbsperacre3
  
  Pfert_lbsperacre=data_range2[1:length(data_range2[,1]),]
  Pfert_lbsperacre[c(1:7,9),] <- P_recommendation_PSU[1:8,]
  Pfert_lbsperacre3 <- Pfert_lbsperacre
}


Pfert_lbsperacre <- cbind(Pfert_lbsperacre,rep(0,16)) # Fix in the future

# It was changed the amount of fertilizer per crop for some of the crops
Nfert=as.numeric(Nfert_lbsperacre) / array(lbsperkg,c(16,length(year_labels))) / array(km2peracre,c(16,length(year_labels))) #kg/km2

Pfert=as.numeric(Pfert_lbsperacre)/array(lbsperkg,c(16,length(year_labels)))/array(km2peracre,c(16,length(year_labels))) #kg/km2

# Grass scenario
Nfert_lbsperacre <- rbind(Nfert_lbsperacre, c(0,0,0,0,0,0))
Nfert <- rbind(Nfert, c(0,0,0,0,0,0))
Pfert_lbsperacre <- rbind(Pfert_lbsperacre, c(0,0,0,0,0,0))
Pfert <- rbind(Pfert,  c(0,0,0,0,0,0))
if(grass_fert_scenario==1){
  Nfert_lbsperacre <- rbind(Nfert_lbsperacre, c(0,0,0,0,0,0))
  Nfert <- rbind(Nfert, c(11200,11200,11200,11200,11200))
  Pfert_lbsperacre <- rbind(Pfert_lbsperacre, c(0,0,0,0,0,0))
  Pfert <- rbind(Pfert, c(5860,5860,5860,5860,5860))
}

# Winter Rye scenario
if(wr_use==2 & wr_scenario){
  Nfert_lbsperacre <- rbind(Nfert_lbsperacre, rep(40.0326300,6))
  Nfert <- rbind(Nfert, rep(4483.584,6))
  Pfert_lbsperacre <- rbind(Pfert_lbsperacre, rep(8.7273547,6))
  Pfert <- rbind(Pfert, rep(977.4484,6))
}else{
  Nfert_lbsperacre <- rbind(Nfert_lbsperacre, c(0,0,0,0,0,0))
  Nfert <- rbind(Nfert, c(0,0,0,0,0,0))
  Pfert_lbsperacre <- rbind(Pfert_lbsperacre, c(0,0,0,0,0,0))
  Pfert <- rbind(Pfert,  c(0,0,0,0,0,0))
}

#write files
write_name1 = paste('InputFiles_CBW/Nfert.txt')
write_name2 = paste('InputFiles_CBW/Pfert.txt')
write.table(Nfert, file = write_name1, sep = " ", row.names = FALSE, col.names = FALSE)
write.table(Pfert, file = write_name2, sep = " ", row.names = FALSE, col.names = FALSE)

#write keys
write_name1 = paste("InputFileKeys/Nfert_key.txt")
Nfert_key = array(" ", c(16+1,length(import_yrs6)+1))
Nfert_key = c("kgN/km2", import_yrs6) #row headings
write.table(Nfert_key, file = write_name1, sep = " ", row.names = FALSE, col.names = FALSE)
write_name2 = paste("InputFileKeys/Pfert_key.txt")
Pfert_key = array(" ", c(16+1,length(import_yrs6)+1))
Pfert_key = c("kgP2O5/km2", import_yrs6) #row headings
write.table(Pfert_key, file = write_name2, sep = " ", row.names = FALSE, col.names = FALSE)

