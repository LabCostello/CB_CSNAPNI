#AFIA_feed_allocation
  #Description
  
  if(print_tags == 1){
    print("CreateInputsSubs_CBW/AFIA_feed_allocation.R")
  }

read_file = 'RawData/Diet-AFIA.xlsx'
AFIA_data_table = (read_excel(read_file))

sum_AFIA_df <- aggregate(AFIA_data_table[,3:59], by= list(AFIA_data_table$Species),FUN=sum)
  
