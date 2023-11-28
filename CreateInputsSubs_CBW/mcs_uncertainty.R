# mcs_uncertainty
if(print_tags == 1){
  print("CreateInputsSubs_CBW/mcs_uncertainty.r")
}

# Set the seed for reproducibility
#set.seed(123)

# Define parameters for fertilizer usage (mean and standard deviation)
mean_fertilizer_usage <- 150 # Replace with actual mean value
sd_fertilizer_usage <- 30   # Replace with actual standard deviation

# Define parameters for corn acreage (mean and standard deviation)
mean_corn_acreage <- 90000000 # Replace with actual mean value
sd_corn_acreage <- 10000000   # Replace with actual standard deviation

# Number of Monte Carlo simulations
num_simulations <- 10000

# Create an empty vector to store simulation results
results <- numeric(num_simulations)

# Monte Carlo simulation
for (i in 1:num_simulations) {
  # Simulate corn acreage and fertilizer usage for each iteration
  corn_acreage <- rnorm(1, mean_corn_acreage, sd_corn_acreage)
  fertilizer_usage <- rnorm(1, mean_fertilizer_usage, sd_fertilizer_usage)
  
  # Calculate total fertilizer used for this iteration
  total_fertilizer_used <- corn_acreage * fertilizer_usage
  
  # Store the result
  results[i] <- total_fertilizer_used
}

# Calculate summary statistics
mean_fertilizer_used <- mean(results)
sd_fertilizer_used <- sd(results)

# Display simulation results
cat("Mean Fertilizer Usage: ", mean_fertilizer_used, "\n")
cat("Standard Deviation: ", sd_fertilizer_used, "\n")

# You can also create a histogram or other visualizations of the results
hist(results, main = "Monte Carlo Simulation Results for Fertilizer Usage")

# runif
#rnorm
#rexp
#rpois
#rbinom
#rgamma
#rweibull

qqplot(rexp(length(cropareacty[,1,5])),cropareacty[,1,5])
abline(0, 1, col = "red")

# NANI
NANIBtot[,1,n] = totNANIws[,1,n]            # atm dep
NANIBtot[,2,n] = rowSums(CfixNwswE[,,n])    # N fix
NANIBtot[,3,n] = sum_commod_spec_fertN[,n]  # ag fertilizer
NANIBtot[,4,n] = FFtotN[,n]                 # food/feed
NANIBtot[,5,n] = totNANIws[,4,n]            # non-ag fertilizer
NANIBtot[,6,n] = totNANIws[,13,n]           # nonfood ag fertilizer
NANIBtotsum[,n] = rowSums(NANIBtot[,,n])
NANIBperkm[,n] = NANIBtotsum[,n] / areaws

c(0.001401730, 0.003039052, 0.003060091, 0.003076567, 0.003307952, 0.003375667)

#Fixation
num_simulations <- 10000
results <- numeric(num_simulations)
for (r in 1:num_simulations) {
  b <-rtri(1, min=0.02742057,max = 0.0309943, mode =  0.0282)
  c <-rtri(1, min=0.001401730,max = 0.003307952, mode =  0.0031)# c(0.001401730, 0.003039052, 0.003060091, 0.003076567, 0.003307952, 0.003375667)
  d <-rtri(1, min=0.06472390,max = 0.06604937, mode =  0.066) #c(0.06472390, 0.06604937, 0.06593764, 0.06593685, 0.05333781,0.065701746)
  e <-rtri(1, min=0.02247174,max = 0.02733232 , mode =  0.027) #c(0.02535043, 0.02707040, 0.02706942, 0.02733232, 0.02247174)
  
  b = CkgwswE_orig[,10,5] * b
  c <- CkgwswE_orig[,11,5] * c
  d <- CkgwswE_orig[,12,5] * d
  e <- CkgwswE_orig[,13,5] * e
  
  results[r] <- sum(b,c,d,e)
}  
hist(results) 

#Fertilizer

for(n in 1:nyrs){
  CfertNtot[1:(n_crops-3),n] = Nfert[,n] * croparea[1:(n_crops-3),n] # total fertilizer applied per crop based on an average fert app rate
  
  for(i in (n_crops-2):n_crops){CfertNtot[i,n]=Nfert[1,n] * croparea[i,n]}
  
  for(i in 1:n_crops){
    if(CfertNtot[i,n] > 0){
      unitfertNC[i,n] = CfertNtot[i,n] / (sum(CkgwE[,i,n])) # kg fert / kg crop. based on production without subtracting "loss/waste"
    }else{unitfertNC[i,n] = 0}
  }
}

for(i in 1:n_crops){CfertNwswE[,i,n] = CkgwswE_orig[,i,n] * unitfertNC[i,n]} # 4.23.13, pre-loss production values