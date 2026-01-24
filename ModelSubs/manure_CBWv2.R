# manure.R
# calculation of recoverable manure
# used values from Kellogg et al. 2000 Table 3, combined confinement and
# recoverability factors
# multiply population by manure per animal by the state specific
# confinement-recoverability factor to estimate the amount of recoverable
# manure (assumed to all be from confinement facilities)

# 11.28.12 Adding code to apply confinement & recovery factors to manure
# using state-specific data

if(print_tags == 1){
  print("ModelSubs/manure_CBW.R")
}

#allocate matrix space
kgmanureNrec = array(0,c(n_cnty,n_anims,nyrs))        
kgmanureNrec450 = array(0,c(n_ws_tbx,n_anims,nyrs))  
kgmanurePrec = array(0,c(n_cnty,n_anims,nyrs))        
kgmanurePrec450 = array(0,c(n_ws_tbx,n_anims,nyrs))
kgmanureNcnty = array(0,c(n_cnty,n_anims,nyrs))  
kgmanurePcnty = array(0,c(n_cnty,n_anims,nyrs))  
kgmanureNlrs = array(0,c(n_ws_tbx,n_anims,nyrs))  
kgmanurePlrs = array(0,c(n_ws_tbx,n_anims,nyrs)) 

# County arrays
dimnames(kgmanureNrec) <- dimnames(kgmanurePrec) <- dimnames(kgmanureNcnty) <- dimnames(kgmanurePcnty) <- create_cbw_dimnames("counties", "animals", "year")

# LRS arrays
dimnames(kgmanureNrec450) <- dimnames(kgmanurePrec450) <- dimnames(kgmanureNlrs) <- dimnames(kgmanurePlrs) <- create_cbw_dimnames("lrs", "animals", "year")

manurefactor2 <- array(0,c(n_anims,nyrs)) # Taken from Kellogs 2014. Values for 2022 are values taken from 2017
manurefactor2[1,] <- c(60,64,68,72,76,76)
manurefactor2[2,] <- c(mean(c(55,56,54)),mean(c(58,60,59)),mean(c(62,63,64)),mean(c(65,67,70)),mean(c(68,71,75)),mean(c(68,71,75)))
manurefactor2[3,] <- c(mean(c(80,84)),mean(c(84,87)),mean(c(88,90)),mean(c(92,93)),mean(c(96,97)),mean(c(96,97)))
manurefactor2[4,] <- c(mean(c(73,81)),mean(c(76,85)),mean(c(79,89)),mean(c(82,93)),mean(c(95,97)),mean(c(95,97))) #hogs for slaughter
manurefactor2[5,] <- c(85,87,90,92,95,95) # chicken layers
manurefactor2[6,] <- c(76,80,84,89,93,93) # breeding turkeys
manurefactor2[7,] <- c(85,87,90,92,95,95) # chicken pullets
manurefactor2[8,] <- c(75,81,87,93,98,98) # chicken broilers
manurefactor2[9,] <- c(76,80,84,89,93,93) # slaughter turkeys
manurefactor2[10,] <- manurefactor2[1,] # beef breeding herd
manurefactor2[11,] <- manurefactor2[1,] # beef calves
manurefactor2[12,] <- manurefactor2[2,] # dairy calves
manurefactor2[13,] <- manurefactor2[1,] # beef heifers
manurefactor2[14,] <- manurefactor2[2,] # dairy heifers
manurefactor2[15,] <- manurefactor2[1,] # beef stockers
manurefactor2[16,] <- manurefactor2[2,] # dairy stockers
manurefactor2[17,] <- c(64,69,74,78,83,83) # sheep
manurefactor2[18,] <- manurefactor2[17,] # horses
manurefactor2[19,] <- manurefactor2[17,] # goats

manurefactor2 <- manurefactor2/100

Nrecfactor <- c(0.4,0.4,0.25,0.25,0.69,0.53,0.5,0.6,0.53,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.3,0.3,0.3) # losses include volatilization, denitrification and spillage
Precfactor <- c(0.9,0.95,0.9,0.9,0.85,0.95,0.95,0.95,0.95,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9) # losses include spillage

for(n in 1:nyrs){
  for(i in 1:n_cnty){
    kgmanureNcnty[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10]))
    kgmanurePcnty[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11]))
    
    kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor2[,n] * Nrecfactor
    kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor2[,n] * Precfactor
  }
}

# Calculating recoverable N&P in manure per land river segment
for(n in 1:nyrs){
  for(i in 1:n_ws_tbx){
    kgmanureNlrs[i,,n] = (noanimdyn[i,,n] * t(animdatadyn[,10]))
    kgmanurePlrs[i,,n] = (noanimdyn[i,,n] * t(animdatadyn[,11]))
    
    kgmanureNrec450[i,,n] = noanimdyn[i,,n] * t(animdatadyn[,10]) * manurefactor2[,n] * Nrecfactor # kg manure N rec for 1925 LRS
    kgmanurePrec450[i,,n] = noanimdyn[i,,n] * t(animdatadyn[,11]) * manurefactor2[,n] * Precfactor # kg manure P rec for 1925 LRS
  }
}

# Calculating recoverable N available as fertilizer

# Manure is probably broadcasted, which means not all the N available will get to the plant. If we apply a coefficient that considers the new N available, how much different it will be?
# Considering that we have broadcasting in two times of the year Spring (April) and Fall (October) we will have different availability factors
# Manure is composed of inorganic and organic fractions, and it is just the inorganic fraction that will be volatilized, but is also not 100% of the organic fraction that will be available for the plant
# Considering this
# Inorganic fraction # https://blog-crop-news.extension.umn.edu/2024/09/nitrogen-availability-of-different.html (For now)

kgmanureNrec450inorganic <- array(0,c(1925,19,6))

#ammonium_concentration <- c(0.52,0.57,0.75,0.75,0.26,0.26,0.26,0.26,0.26,0.52,0.52,0.57,0.52,0.57,0.52,0.57,0,0,0) # Inorganic fraction # https://blog-crop-news.extension.umn.edu/2024/09/nitrogen-availability-of-different.html (For now)
ammonium_concentration <- c(0.31,0.5,0.65,0.65,0.16,0.2,0.16,0.15,0.2,0.18,0.18,0.2,0.18,0.2,0.18,0.2,0,0,0) # Processed based on Table 1.2-10 in Agronomy Guide

for (n in 1:6) {
  for (i in 1:1925) {
    kgmanureNrec450inorganic[i,,n] <- kgmanureNrec450[i,,n]*ammonium_concentration
  }
}

# Organic fraction of manure
kgmanureNrec450organic <- array(0,c(1925,19,6))

for (n in 1:6) {
  for (i in 1:1925) {
    kgmanureNrec450organic[i,,n] <- kgmanureNrec450[i,,n]*(1-ammonium_concentration)
  }
}

# Some manure is moving in between LRS.

# Dividing over the year
kgmanureNrec450springorg <- kgmanureNrec450organic/2
kgmanureNrec450springinorg <- kgmanureNrec450inorganic/2

kgmanureNrec450fallorg <- kgmanureNrec450organic/2
kgmanureNrec450fallinorg <- kgmanureNrec450inorganic/2

# Part of inorganic will be available
kgmanureNrec450fallinorgplantavailable  <- array(0,c(1925,19,6)) # Agronomy guide says that if you don't have cover crop assume that it won't be available
kgmanureNrec450springinorgplantavailable <- array(0,c(1925,19,6))

for (n in 1:6) {
  for (i in 1:1925) {
    kgmanureNrec450springinorgplantavailable[i,,n] <- kgmanureNrec450springinorg[i,,n]*c(0.1,0.1,0.1,0.1,0.2,0.2,0.2,0.2,0.2,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1) # Table 1.2-12 >7 days to incorporation (no-till)
  }
}

kgmanureNrec450fallorgplantavailable <- array(0,c(1925,19,6))
kgmanureNrec450springorgplantavailable <-array(0,c(1925,19,6))

for (n in 1:6) {
  for (i in 1:1925) {
    kgmanureNrec450fallorgplantavailable[i,,n] <- kgmanureNrec450fallorg[i,,n]*c(0.25,0.25,0.3,0.3,0.3,0.25,0.3,0.3,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25) # Table 1.2-12.agronomy guide for reference (winter crop)
    kgmanureNrec450springorgplantavailable[i,,n] <- kgmanureNrec450springorg[i,,n]*c(0.35,0.35,0.5,0.5,0.5,0.35,0.5,0.5,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35) # Table 1.2-12.agronomy guide for reference (summer)
  }
}

# Summing inorganic and organic fractions available for plants
kgmanureNrecplantLRS <- array(0,c(1925,19,6))

for (n in 1:6) {
  kgmanureNrecplantLRS[,,n] <- kgmanureNrec450springorgplantavailable[,,n]+kgmanureNrec450fallorgplantavailable[,,n]+kgmanureNrec450springinorgplantavailable[,,n]
}

# Total manure available per LRS over the 6 years
animal_manure <- 1:16 #c(1:5,7,8,10:16) before
kgmanureNlrsavailableplantavailable <- apply(kgmanureNrecplantLRS[,animal_manure,],c(1,3),sum)
kgmanureNlrsrecovnonavailableplant <- apply(kgmanureNrec450[,animal_manure,]-kgmanureNrecplantLRS[,animal_manure,],c(1,3),sum)
# References: Kellogs (2014) and Agronomy Guide (2019)