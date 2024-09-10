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

for(n in 1:nyrs){
  for(i in 1:n_cnty){
    kgmanureNcnty[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10]))
    kgmanurePcnty[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11]))
    if(FPs[i] == 1){ # Alabama
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[1,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[1,]
    }else if(FPs[i] == 4){ # Arizona
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[3,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[3,]
    }else if(FPs[i] == 5){ # Arkansas
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[4,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[4,]
    }else if(FPs[i] == 6){ # California
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[5,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[5,]
    }else if(FPs[i] == 8){ # Colorado
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[6,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[6,]
    }else if(FPs[i] == 9){ # Connecticut
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[7,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[7,]
    }else if(FPs[i] == 10){ # Delaware
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[8,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[8,]
    }else if(FPs[i] == 12){ # Florida
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[9,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[9,]
    }else if(FPs[i] == 13){ # Georgia
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[10,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[10,]
    }else if(FPs[i] == 16){ # Idaho
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[12,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[12,]
    }else if(FPs[i] == 17){ # Illinois
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[13,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[13,]
    }else if(FPs[i] == 18){ # Indiana
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[14,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[14,]
    }else if(FPs[i] == 19){ # Iowa
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[15,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[15,]
    }else if(FPs[i] == 20){ # Kansas
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[16,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[16,]
    }else if(FPs[i] == 21){ # Kentucky
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[17,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[17,]
    }else if(FPs[i] == 22){ # Louisiana
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[18,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[18,]
    }else if(FPs[i] == 23){ # Maine
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[19,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[19,]
    }else if(FPs[i] == 24){ # Maryland
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[20,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[20,]
    }else if(FPs[i] == 25){ # Massachusetts
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[21,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[21,]
    }else if(FPs[i] == 26){ # Michigan
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[22,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[22,]
    }else if(FPs[i] == 27){ # Minnesota
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[23,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[23,]
    }else if(FPs[i] == 28){ # Mississippi
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[24,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[24,]
    }else if(FPs[i] == 29){ # Missouri
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[25,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[25,]
    }else if(FPs[i] == 30){ # Montana
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[26,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[26,]
    }else if(FPs[i] == 31){ # Nebraska
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[27,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[27,]
    }else if(FPs[i] == 32){ # Nevada
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[28,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[28,]
    }else if(FPs[i] == 33){ # New Hampshire
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[29,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[29,]
    }else if(FPs[i] == 34){ # New Jersey
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[30,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[30,]
    }else if(FPs[i] == 35){ # New Mexico
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[31,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[31,]
    }else if(FPs[i] == 36){ # New York
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[32,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[32,]
    }else if(FPs[i] == 37){ # North Carolina
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[33,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[33,]
    }else if(FPs[i] == 38){ # North Dakota
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[34,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[34,]
    }else if(FPs[i] == 39){ # Ohio
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[35,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[35,]
    }else if(FPs[i] == 40){ # Oklahoma
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[36,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[36,]
    }else if(FPs[i] == 41){ # Oregon
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[37,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[37,]
    }else if(FPs[i] == 42){ # Pennsylvania
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[38,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[38,]
    }else if(FPs[i] == 44){ # Rhode Island
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[39,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[39,]
    }else if(FPs[i] == 45){ # South Carolina
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[40,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[40,]
    }else if(FPs[i] == 46){ # South Dakota
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[41,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[41,]
    }else if(FPs[i] == 47){ # Tennessee
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[42,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[42,]
    }else if(FPs[i] == 48){ # Texas
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[43,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[43,]
    }else if(FPs[i] == 49){ # Utah
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[44,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[44,]
    }else if(FPs[i] == 50){ # Vermont
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[45,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[45,]
    }else if(FPs[i] == 51){ # Virginia
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[46,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[46,]
    }else if(FPs[i] == 53){ # Washington
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[47,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[47,]
    }else if(FPs[i] == 54){ # West Virginia
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[48,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[48,]
    }else if(FPs[i] == 55){ # Wisconsin
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[49,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[49,]
    }else if(FPs[i] == 56){ # Wyoming
      kgmanureNrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,10])) * manurefactor[50,]
      kgmanurePrec[i,,n] = (noanimdyncty[i,,n] * t(animdatadyn[,11])) * manurefactor[50,]
    }
  }
}
# Calculating recoverable N&P in manure per land river segment
for(n in 1:nyrs){
  for(i in 1:n_ws_tbx){
    if(as.numeric(substr(as.character(FIPSws[i]), 1, 2)) == 10){ # Delaware
      kgmanureNrec450[i,,n] = noanimdyn[i,,n]*t(animdatadyn[,10])*manurefactor[8,] # kg manure N rec for 1902 DA
      kgmanurePrec450[i,,n] = noanimdyn[i,,n]*t(animdatadyn[,11])*manurefactor[8,] # kg manure P rec for 1902 DA
    }
    else if(as.numeric(substr(as.character(FIPSws[i]), 1, 2)) == 24){ # Maryland
      kgmanureNrec450[i,,n] = noanimdyn[i,,n]*t(animdatadyn[,10])*manurefactor[20,] # kg manure N rec for 1902 DA
      kgmanurePrec450[i,,n] = noanimdyn[i,,n]*t(animdatadyn[,11])*manurefactor[20,] # kg manure P rec for 1902 DA
    }
    else if(as.numeric(substr(as.character(FIPSws[i]), 1, 2)) == 36){ # New York
      kgmanureNrec450[i,,n] = noanimdyn[i,,n]*t(animdatadyn[,10])*manurefactor[32,] # kg manure N rec for 1902 DA
      kgmanurePrec450[i,,n] = noanimdyn[i,,n]*t(animdatadyn[,11])*manurefactor[32,] # kg manure P rec for 1902 DA
    }
    else if(as.numeric(substr(as.character(FIPSws[i]), 1, 2)) == 42){ # Pennsylvania
      kgmanureNrec450[i,,n] = noanimdyn[i,,n]*t(animdatadyn[,10])*manurefactor[38,] # kg manure N rec for 1902 DA
      kgmanurePrec450[i,,n] = noanimdyn[i,,n]*t(animdatadyn[,11])*manurefactor[38,] # kg manure P rec for 1902 DA
    }
    else if(as.numeric(substr(as.character(FIPSws[i]), 1, 2)) == 51){ # Virginia
      kgmanureNrec450[i,,n] = noanimdyn[i,,n]*t(animdatadyn[,10])*manurefactor[46,] # kg manure N rec for 1902 DA
      kgmanurePrec450[i,,n] = noanimdyn[i,,n]*t(animdatadyn[,11])*manurefactor[46,] # kg manure P rec for 1902 DA
    }
    else if(as.numeric(substr(as.character(FIPSws[i]), 1, 2)) == 54){ # West Virginia
      kgmanureNrec450[i,,n] = noanimdyn[i,,n]*t(animdatadyn[,10])*manurefactor[48,] # kg manure N rec for 1902 DA
      kgmanurePrec450[i,,n] = noanimdyn[i,,n]*t(animdatadyn[,11])*manurefactor[48,] # kg manure P rec for 1902 DA
    }
  }
}

