#AFIA_feed_allocation
  #Description
  
  if(print_tags == 1){
    print("CreateInputsSubs_CBW/AFIA_feed_allocation.R")
  }

read_file = 'RawData/Diet-AFIA.csv'

#CROPS
#Load crop data for 
#crop allocation fractions between animals and humans ([4] prop_human, [5] prop_anim)
#crop N and P contents ([1] prop_dm, [2] N/dm, [3] P/dm)
#crop forage/grain classifications ([10] grain?)
#citations in cropdata_master.xlsx
#key in cropdata_key.txt
cropdata = t(array(scan("InputFiles_CBW/cropdata.txt"), c(18,20)))

dmprop=cropdata[,1]
Nperdm=cropdata[,2]

##ANIMALS
#get livestock N and P requirements and diet propgrain and propforage
animNreq = animdatadyn[,8]
animPreq = animdatadyn[,9]

# This table comes from AFIA and considers only feed that contain protein
AFIA_data_table = (read.csv(read_file))

colnames(AFIA_data_table) <- AFIA_data_table[1,]
AFIA_data_table <- AFIA_data_table[-1,]

AFIA_data_table[,3:25] <- sapply(AFIA_data_table[,3:25],as.numeric)


# Total amount of feed per animal and per type of feed in short tons aggregated by animal
sum_AFIA_df <- aggregate(AFIA_data_table[,3:25], by= list(AFIA_data_table$Species),FUN=sum)

sum_AFIA_df[,2:24] <- sum_AFIA_df[,2:24] * 0.9071847 # converting from short tons to tons

# Separating Corn Gluten Feed/Meal, using csnapni % between CGF and CGM in kg (not in dry matter or nitrogen content)
CGF_percent_diet <- 0.8461538
CGM_percent_diet <- 0.1538462
sum_AFIA_df$`Corn Gluten Meal` <- c(sum_AFIA_df[1,4]*CGM_percent_diet,
                                    0,
                                    sum_AFIA_df[3,4]*CGM_percent_diet,
                                    sum_AFIA_df[4,4]*CGM_percent_diet,
                                    sum_AFIA_df[5,4]*CGM_percent_diet,
                                    0,0,0)

sum_AFIA_df$`Corn Gluten Feed` <- sum_AFIA_df$`Corn Gluten Feed/Meal`*CGF_percent_diet

sum_AFIA_df <- sum_AFIA_df[,-4]

# Converting to Nutrient content (Nutrient content comes from CSNAPNI, Feedipedia)
c_dm <-    c(dmprop[1], #corn grain
            dmprop[20],  #corn dgs
            0.887,       #hominy feed 
            dmprop[3],   #wheat
            dmprop[4],   #oats
            0.923,       #cottonseed
            0.88,        #soybean meal
            0.89,        #canola meal
            0.91,        #cottonseed meal  
            0.87,        #wheat middlings and wheat bran 
            0.90,        #rice mill feed
            0.891,       #soy hulls
            0,           #soy oil
            dmprop[12],  #soybean seeds
            0.958,       #meat and bone meal
            0.92,        #feather meal
            0.958,       #meat meal
            0.938,       #bloodmeal
            0,           #miscellaneous byproduct feeds
            dmprop[2],   #corn silage
            dmprop[10],  #alfalfa hay
            dmprop[11],  #other hay
            dmprop[19],  #corn gluten meal
            dmprop[18])  #corn gluten feed

c_ncindm <- c(Nperdm[1],   #corn grain
              Nperdm[20],  #corn dgs
              0.01904,      #hominy feed 
              Nperdm[3],   #wheat
              Nperdm[4],   #oats
              0.03488,      #cottonseed
              0.0883,       #soybean meal
              0.061,        #canola meal
              0.07568,      #cottonseed meal  
              0.02864,      #wheat middlings and wheat bran 
              0.0185,       #rice mill feed
              0.021,        #soy hulls
              0,            #soy oil
              Nperdm[12],  #soybean seeds
              0.0878,       #meat and bone meal
              0.1368,       #feather meal
              0.0878,   #meat meal
              0.1506,  #bloodmeal
              0, #miscellaneous byproduct feeds
              Nperdm[2], #corn silage
              Nperdm[10],#alfalfa hay
              Nperdm[11], #other hay
              Nperdm[19],  #corn gluten meal
              Nperdm[18]) #corn gluten feed

AFIA_df <- sum_AFIA_df

for (i in 1:24) {
  AFIA_df[,i+1] <- AFIA_df[,i+1]*c_dm[i]*c_ncindm[i]}

AFIA_df_percent <- AFIA_df[,2:25]/rowSums(AFIA_df[,2:25])

animNreq_AFIA <- c(animNreq[1],animNreq[8],animNreq[2],animNreq[4],animNreq[18],animNreq[5],animNreq[17],animNreq[9])
diet <- AFIA_df_percent
for (i in 1:8) {diet[i,] <- AFIA_df_percent[i,]*animNreq_AFIA[i]}

rownames(diet) <- sum_AFIA_df[,1]

diet_crops_model <- data.frame("corn.grain"=diet$Corn,
                               "corn.silage"=diet$`Corn Silage`,
                               "wheat"=diet$Wheat+diet$`Wheat Middlings and Wheat Bran`,
                               "oats"=diet$Oats,
                               "barley"=0,
                               'sorghum for grain'=0,
                               'sorghum for silage'=0,
                               'potatoes'=0,
                               'rye'=0,
                               'alfalfa hay'=diet$`Alfalfa Hay`,
                               'other hay'=diet$`Other Hay`,
                               'soybeans'=diet$`Soy Hulls`+diet$`Soybean Meal`+diet$`Soybean Seeds`,
                               'cropland pasture'=0,
                               'noncropland pasture'=0,
                               'rice'=diet$`Rice Mill Feed`,
                               'peanuts'=0,
                               'grass'=0,
                               'CGF'=diet$`Corn Gluten Feed`,
                               'CGM'=diet$`Corn Gluten Meal`,
                               'DGS'=diet$`Corn DDGs`)

diet_crops_model <- as.matrix(unlist(diet_crops_model))

diet_animal_byproducts <- diet[,c(3,15,16,17,18)]
diet_plant_byproducts <- diet[,c(6,8,9)]


