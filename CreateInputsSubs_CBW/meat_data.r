#meat_data.r
#This script takes the meat production and emissions data from the 2015 MATLAB NANI files and adapts it for 1997-2017.
#Meat production: This uses the first two columns of meatprod_old.txt to get meat/animal values for 2002 and 2007
#These values are averaged and then used with animal populations to calculate meat production for all the other years
#Meat emissions: This is just cols 3-10 of meatprod_old.txt. Using the same values for every year
#because there's not much difference between 02 and 07 data, but there's no basis for comparison for N2O emissions,
#so that should probably be checked

if(print_tags == 1){
  print("CreateInputsSubs_CBW/meat_data.R")
}

#meat production
import_yrs = c("97","02","07","12","17")  #last two digits of data years to import, only needed for length in this file
meatprod_old=t(array(scan("RawData/meatprod_old.txt"), c(10,n_meats))) # OLD AND NOT REPRESENT THE REALITY FOR CBW, BUT FOR THE COUNTRY (DON'T USE THIS VALUE)
old = 2 #two columns to use from the old file
meatprodnew = array(0,c(n_meats,length(import_yrs)))
kgmeatperanim = array(0,c(n_meats,old))
kgmeatperanim_avg = array(0,c(n_meats,1))
animpoptotal_sort = array(0,c(n_meats,length(import_yrs)))
for(n in 1:length(import_yrs)){
  animpoptotal_sort[1,n]=animpoptotal[1,n]+animpoptotal[10,n]+animpoptotal[11,n]+animpoptotal[13,n]+animpoptotal[15,n] #beef
  animpoptotal_sort[2,n]=animpoptotal[2,n]+animpoptotal[12,n]+animpoptotal[14,n]+animpoptotal[16,n] #dairy
  animpoptotal_sort[3,n]=animpoptotal[3,n]+animpoptotal[4,n] #pigs/hogs
  animpoptotal_sort[4,n]=animpoptotal[17,n] #sheep
  animpoptotal_sort[5,n]=animpoptotal[18,n] #horse
  animpoptotal_sort[6,n]=animpoptotal[5,n] #layers
  animpoptotal_sort[7,n]=animpoptotal[7,n]+animpoptotal[8,n] #broilers
  animpoptotal_sort[8,n]=animpoptotal[6,n]+animpoptotal[9,n] #turkey
  animpoptotal_sort[9,n]=animpoptotal[19,n] #goats
}
for(col in 1:old){
  kgmeatperanim[,col]=meatprod_old[,col]/animpoptotal_sort[,col+which(import_yrs=="02")] #need this to calc yearly (02 and 07 kg meat per animal, these two columns should be the same/very similar)
}
for(a in 1:9){
  kgmeatperanim_avg[a]=mean(kgmeatperanim[a,]) #average kg meat per animal
}

#PLUGGED
#kgmeatperanim_avg = matrix(data=c(animdatadyn[1,14],animdatadyn[2,14],animdatadyn[4,14],animdatadyn[17,14],animdatadyn[18,14],animdatadyn[5,14],animdatadyn[8,14],animdatadyn[9,14],animdatadyn[19,14]), ncol = 1) # The meatprod_old is old txt file with numbers that are too big for the size of the region, and unbalancing the model
kgmeatperanim_avg = matrix(data=c(144.2,10068.67,124.55,15.6,0,12.9,10.2,21.1,23.9),ncol = 1) # Temporary - copied from the CSNAPNI, because the original is based on a country number

for(n in 1:length(import_yrs)){
  meatprodnew[,n] = animpoptotal_sort[,n]*kgmeatperanim_avg
}

#### PLUGGED #####
meatprodnew[2,] <- 10068.67*animpoptotal[2,]
##################

#meat emissions
meatemissions = meatprod_old[,3:length(meatprod_old[1,])]
meatemissions <- array(0,c(9,8)) # just to not keep wrong values, change it later

#write meatprod file
write_name = paste('InputFiles_CBW/meatprod.txt')
write.table(meatprodnew, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

#write meatprod key
write_name = paste("InputFileKeys/meatprod_key.txt")
meatprod_key = array(" ", c(n_meats+1,length(import_yrs)+1))
meatprod_key[1,] = c(" ", import_yrs) #column headings
meatprod_key[,1]=c("(kg)","beef","dairy","swine","sheep","horse","layer chicken","broiler chicken","turkey","goats") #row headings
write.table(meatprod_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

#write meatemissions file
write_name = paste('InputFiles_CBW/meatemissions.txt')
write.table(meatemissions, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

#write meatemissions key
write_name = paste("InputFileKeys/meatemissions_key.txt")
meatemissions_key = array(" ", c(n_meats+1,length(meatemissions[1,])+1))
meatemissions_key[1,] = c(" ","[1]entericCH4(TgCO2eq,100yrGWP/kg meat)2002","[2]manureCH4(TgCO2eq,100yrGWP/kg meat)2002","[3]manureN2O(TgCO2eq,100yrGWP/kg meat)2002","[4]entericCH4(TgCO2eq,100yrGWP/kg meat)2007","[5]entericCH4(TgCO2eq,20yrGWP/kg meat)2002","[6]manureCH4(TgCO2eq,20yrGWP/kg meat)2002","[7]manureN2O(TgCO2eq,20yrGWP/kg meat)2002","[8]entericCH4(TgCO2eq,100yrGWP/kg meat)2007") #column headings
meatemissions_key[,1]=c(" ","beef","dairy","swine","sheep","horse","layer chicken","broiler chicken","turkey","goats") #row headings
write.table(meatemissions_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)