## biogas.R

if(print_tags == 1){
  print("ModelSubs/biogas.R")
}

read_file = 'RawData/biogas.xlsx' #these areas are planted areas 

biogasdata = read_excel(read_file)

biogasinfo = as.matrix(biogasdata[2:27,2:11])


# Amount of feedstock available ####
# Manure
manureperanimyr <- animdatadyn[,10]/as.numeric(c(biogasinfo[5,4],biogasinfo[1,4],biogasinfo[11,4],biogasinfo[11,4],biogasinfo[15,4],
              biogasinfo[16,4],biogasinfo[15,4],biogasinfo[14,4],biogasinfo[15,4],biogasinfo[5,4],
              biogasinfo[2,4],biogasinfo[2,4],biogasinfo[3,4],biogasinfo[3,4],biogasinfo[3,4],
              biogasinfo[3,4],biogasinfo[19,4],biogasinfo[21,4],biogasinfo[19,4]))

manurecty <- array(0, c(197,19,length(year_labels)))
manurews <- array(0, c(1925,19,length(year_labels)))
a <- array(0,dim = c(1925,19))

for (n in 1:data_yrs) {
  for (i in 1:19) {manurecty[,i,n] <- animpopcnty[,i,n]*manureperanimyr[i]} 
}

for (n in 1:data_yrs) {
  for (i in 1:19) {manurews[,i,n] <- animpopws[,i,n]*manureperanimyr[i]} 
}

for(i in 1:n_ws_tbx){
  if(as.numeric(substr(as.character(FIPSws[i]), 1, 2)) == 10){ # Delaware
    a[i,] = manurefactor[8,] # kg manure N rec for 1902 DA
  }
  else if(as.numeric(substr(as.character(FIPSws[i]), 1, 2)) == 24){ # Maryland
    a[i,] = manurefactor[20,] # kg manure N rec for 1902 DA
  }
  else if(as.numeric(substr(as.character(FIPSws[i]), 1, 2)) == 36){ # New York
    a[i,] = manurefactor[32,] # kg manure N rec  for 1902 DA
  }
  else if(as.numeric(substr(as.character(FIPSws[i]), 1, 2)) == 42){ # Pennsylvania
    a[i,] = manurefactor[38,] # kg manure N rec for 1902 DA
  }
  else if(as.numeric(substr(as.character(FIPSws[i]), 1, 2)) == 51){ # Virginia
    a[i,] = manurefactor[46,] # kg manure N rec for 1902 DA
  }
  else if(as.numeric(substr(as.character(FIPSws[i]), 1, 2)) == 54){ # West Virginia
    a[i,] = manurefactor[48,] # kg manure N rec for 1902 DA
  }
}

# Grass
grasscty <- cropprodcnty[,17,]
grassws <- cropprodws[,17,]

# Rye (colSums(CNprod[,18,])-colSums(feedN4meat[,18,]), general idea)
wrcty <- array(0, c(197,length(year_labels)))
wrws <- array(0, c(1925,length(year_labels)))

for (n in 1:5) {
  wrws[,n] <- CkgwE[,18,n] - rowSums(animpopws[,1:2,n]*animNfromC[1:2,18,n]/NperC[18]) # How much is produced - how much the animal eat
  wrcty[,n] <- cropprodcnty[,18,n] - rowSums(noanimdyncty[,1:2,n]*animNfromC[1:2,18,n]/NperC[18]) # How much is produced - how much the animal eat
}

# Estimating biogas available #####
VSperanim <- as.numeric(c(biogasinfo[5,3],biogasinfo[1,3],biogasinfo[11,3],biogasinfo[11,3],biogasinfo[15,3],
                              biogasinfo[16,3],biogasinfo[15,3],biogasinfo[14,3],biogasinfo[15,3],biogasinfo[5,3],
                              biogasinfo[2,3],biogasinfo[2,3],biogasinfo[3,3],biogasinfo[3,3],biogasinfo[3,3],
                              biogasinfo[3,3],biogasinfo[19,3],biogasinfo[21,3],biogasinfo[19,3]))

CH4perTSanim <- as.numeric(c(biogasinfo[5,8],biogasinfo[1,8],biogasinfo[11,8],biogasinfo[11,8],biogasinfo[15,8],
                          biogasinfo[16,8],biogasinfo[15,8],biogasinfo[14,8],biogasinfo[15,8],biogasinfo[5,8],
                          biogasinfo[2,8],biogasinfo[2,8],biogasinfo[3,8],biogasinfo[3,8],biogasinfo[3,8],
                          biogasinfo[3,8],biogasinfo[19,8],biogasinfo[21,8],biogasinfo[19,8]))

CH4peranim <- TSperanim*CH4perTSanim/1000

# Manure biogas
manurebiogascty <- array(0, c(197,19,length(year_labels)))
manurebiogasws <- array(0, c(1925,19,length(year_labels)))

for (i in 1:19) {
  for (n in 1:data_yrs) {manurebiogascty[,i,n] <- manurecty[,i,n]*CH4peranim[i]}
    for (n in 1:data_yrs) {manurebiogasws[,i,n] <- manurews[,i,n]*CH4peranim[i]}
}
# Grass biogas
grassbiogascty <- array(0, c(197,length(year_labels)))
grassbiogasws <- array(0, c(1925,length(year_labels)))

for (n in 1:length(import_yrs)){
grassbiogascty[,n] <- grasscty[,n] * as.numeric(biogasinfo[23,3]) * as.numeric(biogasinfo[23,8]) / 1000
grassbiogasws[,n] <- grassws[,n] * as.numeric(biogasinfo[23,3]) * as.numeric(biogasinfo[23,8]) / 1000
}

# Rye biogas
wrbiogascty <- array(0, c(197,length(year_labels)))
wrbiogasws <- array(0, c(1925,length(year_labels)))
if (wr_biogas == 1 & wr_scenario ==1) {
  for (n in 1:length(import_yrs)){
  wrbiogascty[,n] <- wrcty[,n] * as.numeric(biogasinfo[24,3]) * as.numeric(biogasinfo[24,8]) / 1000
  wrbiogasws[,n] <- wrws[,n] * as.numeric(biogasinfo[24,3]) * as.numeric(biogasinfo[24,8]) / 1000
  }
}
# Total biogas
CH4cnty <- array(0, c(197,length(year_labels)))
CH4ws <- array(0, c(1925,length(year_labels)))

for (n in 1:length(import_yrs)){
  CH4cnty[,n] <- rowSums(manurebiogascty[,,n]) + grassbiogascty[,n] + wrbiogascty[,n]
  CH4ws[,n] <- rowSums(manurebiogasws[,,n]) + grassbiogasws[,n] + wrbiogasws[,n]
}

# Calculate energy ####
# Multiply by 10kwh/m3
kwcnty <- CH4cnty*10
kwws <- CH4ws*10

# Calculate digestate


# Reference ####
# AD-ScreeningTool_v2.3
sum(grassbiogascty[,length(year_labels)]*10)
#CHP benefits
# Burning Scenario (how much energy we could get - )
# Billion-ton
# 411MWh (41x10^6m3) 
# 370 TWh only cow poop (sum(manurebiogascty[,2,5])*10/1000000)) - 9% electricity in the US 3TWh
# 4.8TWh
# To be released in the future ####
# GM_Ratio <- 0.25 # 0 to 2.5
# HRT <- 35 # 15 to 50
# OL <- 8 # 5 to 10
# # Biogas yield (L/kgVS)
# biogas_yield_experiment <- (-44.4061) + (75.62 * GM_Ratio) + (2.8923 * HRT) + (16.5418 * OL) +
#   (-15.7594 * GM_Ratio^2) + (0.0004678 * HRT^2) + (-0.52582 * OL^2) + (0.37158 * GM_Ratio * HRT) + (-3.2546 * GM_Ratio * OL) + (0.0777 * HRT * OL)
# 
# # Methane Yield (mL/gVS):
# methane_yield_experiment <- (-40.3794016121415) + (42.9747340004101 * GM_Ratio) + (2.64952058226338 * HRT) + (10.8063962468867 * OL) + 
#   (-8.49557738021234 * GM_Ratio^2) + (-0.00501912749810205 * HRT^2) + (-0.352307370248999 * OL^2) + (0.161920331221587 * GM_Ratio * HRT) +
#   (-1.76853028491111 * GM_Ratio * OL) + (-0.0124632072874282 * HRT * OL)
# 
# # % Total Solids to Solid digestate (% of starting solids) -  Moisture content of solid digestate: ~74.5%:
# percet_soliddigestate <- (-0.957361611866969) + (0.443715343915344 * GM_Ratio) + (0.00469137188208617 * HRT) + (0.345758650793651 * OL) + 
#   (-0.0504592592592592 * GM_Ratio^2) + (0.0000615918367346938 * HRT^2) + (-0.021142 * OL^2) + (-0.00370793650793651 * GM_Ratio * HRT) +
#   (-0.00088888888888884 * GM_Ratio * OL) + (-0.00126342857142857 * HRT * OL)
# 
# # % of starting nitrogen going to solid digestate:
percent_Nsoliddigestate <- (-0.793140885613505) + (0.263667901234568 * GM_Ratio) + (0.0104860657596372 * HRT + 0.271661428571429 * OL) +
   (-0.0611358024691358 * GM_Ratio^2) + (-0.0000226122448979593 * HRT^2) + (-0.016752 * OL^2) + (-0.00350222222222222 * GM_Ratio * HRT) +
   (0.02296 * GM_Ratio * OL) + (-0.00142342857142857 * HRT * OL)
