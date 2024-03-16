# mcs_uncertainty
if(print_tags == 1){
  print("CreateInputsSubs_CBW/mcs_uncertainty.r")
}
library(EnvStats)
# Set the seed for reproducibility
set.seed(123)
library("fitdistrplus")

# Fertilizer Real deal ####
# Startin from the point:
#1. that sum(CfertNtot[,5]) is basicaly sum(NANIBtot[,3,5]), which is the N input coming from fertilizer
#2. that CfertNtot is basicaly Nfert * area
# We have to randomize Nfert, and area will keep the same all the time because it is census data.

# Corn grain
cornga <- rtri(10000,69.7,205.1,173.9)*croparea[1,5]/lbsperkg/km2peracre

cornga <- na.omit(NASS_County[,206]/NASS_County[,205])
cornga <- cornga[cornga>0]*croparea[1,5]/lbsperkg/km2peracre
  
shapiro.test(cornga) #shapirotest (has to be more than 0.05)
descdist(cornga)

cornga <- rnorm(10000,mean(cornga),sd(cornga))

hist(cornga)
# Corn silage
cornsa <- NASS_County[,208]/NASS_County[,207]
cornsa <- cornsa[is.finite(cornsa)]
cornsa <- cornsa[cornsa>0]

cornsa <- cornsa * 7*croparea[2,5]/lbsperkg/km2peracre

shapiro.test(cornsa)
descdist(cornsa)

cornsa <- rnorm(10000, mean(cornsa), sd(cornsa))

# Wheat
wheata <- NASS_County[,210]/NASS_County[,209]
wheata <- wheata[is.finite(wheata)]
wheata <- wheata[wheata>0]*croparea[3,5]/lbsperkg/km2peracre

shapiro.test(wheata)
descdist(wheata) # beta distribution

wheata <- runif(10000,min(wheata),max(wheata))

# Oats
oata <- NASS_County[,212]/NASS_County[,211]
oata <- oata[is.finite(oata)]
oata <- oata[oata>0]*0.8*croparea[4,5]/lbsperkg/km2peracre

oata <- runif(10000,min(oata),max(oata))

# Barley
barleya <- NASS_County[,214]/NASS_County[,213]
barleya <- barleya[is.finite(barleya)]
barleya <- barleya[barleya>0]*0.8*croparea[5,5]/lbsperkg/km2peracre

barleya <- runif(10000,min(barleya),max(barleya))

# Sorghum for grain
sorghumga <- NASS_County[,216]/NASS_County[,215]
sorghumga <- sorghumga[is.finite(sorghumga)]
sorghumga <- sorghumga[sorghumga>0]*0.75*croparea[6,5]/lbsperkg/km2peracre

sorghumga <- runif(10000,min(sorghumga),max(sorghumga))

# Sorghum for silage
sorghumsa <- NASS_County[,218]/NASS_County[,217]
sorghumsa <- sorghumsa[is.finite(sorghumsa)]
sorghumsa <- sorghumsa[sorghumsa>0]*7*croparea[7,5]/lbsperkg/km2peracre

sorghumsa <- runif(10000,min(sorghumsa),max(sorghumsa))

#Potatoes
potatoesa <- rep(965655.38,10000)

# Rye
ryea <- NASS_County[,222]/NASS_County[,221]
ryea <- ryea[is.finite(ryea)]
ryea <- ryea[ryea>0]*croparea[9,5]/lbsperkg/km2peracre

ryea <- runif(10000,min(ryea),max(ryea))

# Alfalfa hay
alfla <- rep(3291196.98,10000)

# Other hay
otha <- rep(12506438.00,10000)

# Soybeans
soya <- rep(3393396.01,10000)

# peanuts
peana <- rep(22096.95,10000)
  
# cgf
feeda <- rep(238187.20,10000)

# cgm
meala <- rep(43306.76,10000)

# Other fertilizers
otfa <- rep(24625952,10000)

# Confidence interval
allcropsfertnorm <- cornga + cornsa + wheata + oata + barleya + sorghumga + sorghumsa + potatoesa + ryea + alfla + otha + soya + peana + feeda + meala

mean_val <- mean(allcropsfertnorm)

t.test(allcropsfertnorm)

error <- qnorm(c(0.05, 0.95), mean = mean(allcropsfertnorm), sd = sd(allcropsfertnorm))



p <- ggplot(data.frame(x=allcropsfertnorm), aes(x)) +
  geom_density(fill = "skyblue", color = "black") +
  geom_vline(xintercept = mean_val, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = error[1], color = "blue", linetype = "dotted", linewidth = 1) +
  geom_vline(xintercept = error[2], color = "blue", linetype = "dotted", linewidth = 1) +
  labs(title = "Normal Distribution with Mean and 95% Confidence Interval",
       x = "Values", y = "Density")

NANIferta <- allcropsfertnorm + otfa
errora <- qnorm(c(0.05, 0.95), mean = mean(NANIferta), sd = sd(NANIferta))

pa <- ggplot(data.frame(x=NANIferta), aes(x)) +
  geom_density(fill = "skyblue", color = "black") +
  geom_vline(xintercept = mean(NANIferta), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = errora[1], color = "blue", linetype = "dotted", linewidth = 1) +
  geom_vline(xintercept = errora[2], color = "blue", linetype = "dotted", linewidth = 1) +
  labs(title = "Normal Distribution with Mean and 95% Confidence Interval",
       x = "Values", y = "Density")+
  geom_label(x= mean(NANIferta)+5000000,y=1e-09,size = 3.5, label = "1.64 e8")+
  geom_label(x=errora[1]+5000000,y=1e-09,size = 3.5, label = "1.41 e8")+
  geom_label(x= errora[2]+5000000,y=1e-09,size = 3.5, label = "1.87 e8")

# Temporario
NANItota <- NANIferta+rep((sum(totNANIws[,1,5])+sum(CfixNwswE[,,5])+  sum(FFtotN[,5])),10000)
errora <- qnorm(c(0.025, 0.975), mean = mean(NANItota), sd = sd(NANItota))

pa <- ggplot(data.frame(x=NANItota), aes(x)) +
  geom_density(fill = "skyblue", color = "black") +
  geom_vline(xintercept = mean(NANItota), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = errora[1], color = "blue", linetype = "dotted", linewidth = 1) +
  geom_vline(xintercept = errora[2], color = "blue", linetype = "dotted", linewidth = 1) +
  labs(title = "Normal Distribution with Mean and 95% Confidence Interval",
       x = "Values", y = "Density")+
  geom_label(x= mean(NANItota)+5000000,y=1e-09,size = 3.5, label = "6.10 e8")+
  geom_label(x=errora[1]+5000000,y=1e-09,size = 3.5, label = "5.83 e8")+
  geom_label(x= errora[2]+5000000,y=1e-09,size = 3.5, label = "6.38 e8")

# Fixation for Real ####
# Alfalfa (75)
a <- NASS_County[,224]/NASS_County[,223]
a <- a[is.finite(a)]
a <- a[a>0]*75.59

# Soybean (5.3)

# Other Hay (175.73)


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

