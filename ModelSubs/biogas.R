## biogas.R

if(print_tags == 1){
  print("ModelSubs/biogas.R")
}

read_file = 'RawData/biogas.xlsx' #these areas are planted areas 

biogasdata = read_excel(read_file)

biogasinfo = as.matrix(biogasdata[2:27,2:12])


# Amount of feedstock available ####
# Manure
manureperanim <- as.numeric(c(biogasinfo[5,1],biogasinfo[1,1],biogasinfo[11,1],biogasinfo[11,1],biogasinfo[15,1],
                              biogasinfo[16,1],biogasinfo[15,1],biogasinfo[14,1],biogasinfo[15,1],biogasinfo[5,1],
                              biogasinfo[2,1],biogasinfo[2,1],biogasinfo[3,1],biogasinfo[3,1],biogasinfo[3,1],
                              biogasinfo[3,1],biogasinfo[19,1],biogasinfo[21,1],biogasinfo[19,1]))

manureperanimyr <- manureperanim*365

manurecty <- array(0, c(197,19,5))
manurews <- array(0, c(1925,19,5))

for (n in 1:data_yrs) {
  for (i in 1:19) {manurecty[,i,n] <- animpopcnty[,i,n]*manureperanimyr[i]} 
}

for (n in 1:data_yrs) {
  for (i in 1:19) {manurews[,i,n] <- animpopws[,i,n]*manureperanimyr[i]} 
}


# Grass
grasscty <- cropprodcnty[,17,]
grassws <- cropprodws[,17,]

# Rye
cccty <- array(0, c(197,5))
ccws <- array(0, c(1925,5))

# Estimating biogas available #####
TSperanim <- as.numeric(c(biogasinfo[5,4],biogasinfo[1,4],biogasinfo[11,4],biogasinfo[11,4],biogasinfo[15,4],
                              biogasinfo[16,4],biogasinfo[15,4],biogasinfo[14,4],biogasinfo[15,4],biogasinfo[5,4],
                              biogasinfo[2,4],biogasinfo[2,4],biogasinfo[3,4],biogasinfo[3,4],biogasinfo[3,4],
                              biogasinfo[3,4],biogasinfo[19,4],biogasinfo[21,4],biogasinfo[19,4]))

CH4perTSanim <- as.numeric(c(biogasinfo[5,9],biogasinfo[1,9],biogasinfo[11,9],biogasinfo[11,9],biogasinfo[15,9],
                          biogasinfo[16,9],biogasinfo[15,9],biogasinfo[14,9],biogasinfo[15,9],biogasinfo[5,9],
                          biogasinfo[2,9],biogasinfo[2,9],biogasinfo[3,9],biogasinfo[3,9],biogasinfo[3,9],
                          biogasinfo[3,9],biogasinfo[19,9],biogasinfo[21,9],biogasinfo[19,9]))

CH4peranim <- TSperanim*CH4perTSanim/1000

# Manure biogas
manurebiogascty <- array(0, c(197,19,5))
manurebiogasws <- array(0, c(1925,19,5))

for (i in 1:19) {
  for (n in 1:data_yrs) {manurebiogascty[,i,n] <- manurecty[,i,n]*CH4peranim[i]}
    for (n in 1:data_yrs) {manurebiogasws[,i,n] <- manurews[,i,n]*CH4peranim[i]}
}
# Grass biogas
grassbiogascty <- array(0, c(197,5))
grassbiogasws <- array(0, c(1925,5))

for (n in 1:length(import_yrs)){
grassbiogascty[,n] <- grasscty[,n] * as.numeric(biogasinfo[23,4]) * as.numeric(biogasinfo[23,9]) / 1000
grassbiogasws[,n] <- grassws[,n] * as.numeric(biogasinfo[23,4]) * as.numeric(biogasinfo[23,9]) / 1000
}

# Rye biogas
ccbiogascty <- array(0, c(197,5))
ccbiogasws <- array(0, c(1925,5))

for (n in 1:length(import_yrs)){
ccbiogascty[,n] <- cccty[,n] * as.numeric(biogasinfo[24,4]) * as.numeric(biogasinfo[24,9]) / 1000
ccbiogasws[,n] <- ccws[,n] * as.numeric(biogasinfo[24,4]) * as.numeric(biogasinfo[24,9]) / 1000
}

# Total biogas
CH4cnty <- array(0, c(197,5))
CH4ws <- array(0, c(1925,5))

for (n in 1:length(import_yrs)){
  CH4cnty[,n] <- rowSums(manurebiogascty[,,n]) + grassbiogascty[,n] + ccbiogascty[,n]
  CH4ws[,n] <- rowSums(manurebiogasws[,,n]) + grassbiogasws[,n] + ccbiogasws[,n]
}

# Calculate energy ####
# Multiply by 10kwh/m3
kwcnty <- CH4cnty*10
kwws <- CH4ws*10

# Calculate digestate


# Reference ####
# AD-ScreeningTool_v2.3
sum(grassbiogascty[,5]*10)
#CHP benefits
# Burning Scenario (how much energy we could get - )
# Billion-ton
# 411MWh (41x10^6m3) 
# 370 TWh only cow poop (sum(manurebiogascty[,2,5])*10/1000000)) - 9% electricity in the US 3TWh

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
# percent_Nsoliddigestate <- (-0.793140885613505) + (0.263667901234568 * GM_Ratio) + (0.0104860657596372 * HRT + 0.271661428571429 * OL) +
#   (-0.0611358024691358 * GM_Ratio^2) + (-0.0000226122448979593 * HRT^2) + (-0.016752 * OL^2) + (-0.00350222222222222 * GM_Ratio * HRT) +
#   (0.02296 * GM_Ratio * OL) + (-0.00142342857142857 * HRT * OL)
