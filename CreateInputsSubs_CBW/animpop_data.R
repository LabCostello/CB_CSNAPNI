# animpop_data.r
## This script pulls county animal population data (3111 counties from the continental US) for 1997-2017
#from NANI_NAPI_NASS_pull.r
#uses "anim_avg_inv_array"

if(print_tags == 1){
  print("CreateInputsSubs_CBW/animpop_data.R")
}


# Specify number and names of crop categories
n_animtyp = 19 #number of crops to import
animtyp=array("",c(n_animtyp))
animtyp[1] = 'fattened cattle'
animtyp[2] = 'milk cows'
animtyp[3] = 'hogs for breeding'
animtyp[4] = 'hogs for slaughter'
animtyp[5] = 'chicken layers'
animtyp[6] = 'breeding turkeys'
animtyp[7] = 'chicken pullets'
animtyp[8] = 'chicken broilers'
animtyp[9] = 'slaughter turkeys'
animtyp[10] = 'beef breeding herd'
animtyp[11] = 'beef calves'
animtyp[12] = 'dairy calves'
animtyp[13] = 'beef heifers'
animtyp[14] = 'dairy heifers'
animtyp[15] = 'beef stockers'
animtyp[16] = 'dairy stockers'
animtyp[17] = 'sheep'
animtyp[18] = 'horses'
animtyp[19] = 'goats'

# allocate space to matrices
animpopcnty_old <- array(0,c(n_cnty,length(animtyp),length(import_yrs)))
animpopcnty = array(0,c(n_cnty,length(animtyp),length(import_yrs)))
animpopcnty = anim_avg_inv_array
animpopws = array(0,c(n_ws_tbx,length(animtyp),length(import_yrs)))
for(n in 1:length(import_yrs)){
  # watershed crop production
  animpopws[,,n] = t(cnty_ws)%*%animpopcnty[,,n]
  
  dummy <- merge(cbind(percent_crop_in_cbw,percent_corn_in_cbw$percentage.corn),cbind(FIPS,animpopcnty[,,n]))
  animws <- data.frame("FIPS"=dummy$FIPS,
                       "LNDRVRSEG"=dummy$LNDRVRSEG,
                       "OBJECTID"=dummy$OBJECTID,
                       "fattened cattle"=dummy$percentage*dummy$V2,
                       "milk cows"=dummy$`percent_corn_in_cbw$percentage.corn`*dummy$V3,
                       "hogs for breeding"=dummy$percentage*dummy$V4,
                       "hogs for slaughter"=dummy$percentage*dummy$V5,
                       "chicken layers"=dummy$percentage*dummy$V6,
                       "breeding turkeys"=dummy$percentage*dummy$V7,
                       "chicken pullets"=dummy$percentage*dummy$V8,
                       "chicken broilers"=dummy$percentage*dummy$V9,
                       "slaughter turkeys"=dummy$percentage*dummy$V10,
                       "beef breeding herd"=dummy$percentage*dummy$V11,
                       "beef calves"=dummy$percentage*dummy$V12,
                       "dairy calves"=dummy$percentage*dummy$V13,
                       "beef heifers"=dummy$percentage*dummy$V14,
                       "dairy heifers"=dummy$percentage*dummy$V15,
                       "beef stockers"=dummy$percentage*dummy$V16,
                       "dairy stockers"=dummy$percentage*dummy$V17,
                       "sheep"=dummy$percentage*dummy$V18,
                       "horses"=dummy$percentage*dummy$V19,
                       "goats"=dummy$percentage*dummy$V20)
  
  animpopcnty_old[,,n] <- animpopcnty[,,n]
  
  animpopcnty[,,n] <- as.matrix(subset(aggregate(animws[,4:22], by=list(animws$FIPS), FUN = sum),select=-1)) # Readjusting county information to only info inside CBW
  
  animpopws[,,n] <- as.matrix(subset(animws,select=-c(1:3)))
  
  #write data files
  ##animal population
  write_name = paste("InputFiles_CBW/noanimdyn",run_yrs[n],".txt",sep = "")
  write.table(animpopws[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  write_name = paste("InputFiles_CBW/noanimdyncty",run_yrs[n],".txt",sep = "")
  write.table(animpopcnty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
}
#write key
##animal population
write_name = "InputFileKeys/noanimdyn_key.txt"
animpopws_key = array(" ", c(n_ws_tbx+1,length(animtyp)+1))
animpopws_key[1,]=c(" ", animtyp) #column headings
animpopws_key[,1]=c("ws_num", 1:n_ws_tbx) #row headings
write.table(animpopws_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
write_name = "InputFileKeys/noanimdyncty_key.txt"
animpopcnty_key = array(" ", c(n_cnty+1,length(animtyp)+1))
animpopcnty_key[1,]=c(" ", animtyp) #column headings
animpopcnty_key[,1]=c("ws_num", 1:n_cnty) #row headings
write.table(animpopcnty_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
animpoptotal = drop(colSums(animpopcnty)) #total animal populations in each year