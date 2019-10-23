#Baseline Model using Tracey Mangin's PT Surplus Production Model (Cisneros-Mata; 2019)

library(tidyverse)
library(TMB)

#Define the surplus production function

pt <- function(B_t, K, phi, g, H){
  B_t <- B_t + 
    (((phi + 1) / phi) * g * B_t * (1 - ((B_t / K) ^ phi)))
    - H
  return(B_t)
}

#Under no management we are assuming that the harvest (H term of pt model) would be the given by F_oa*B_t
#Where: 
    #- F_oa = fishing mortality at open access (defined as 0.3*Bmsy from Gaines et al 2018)
    #- B_t = biomass at time t

#Define the amount of harvest equation (the H term of the pt model):

F_oa <- function(g, phi){
  g * ((phi + 1) / phi) * (1 - ((0.3 ^ phi) / phi + 1))
}

#Define parameters

#Note these values of r are made up - not actually reflective of slow vs fast growing species; need to find this still
r_slow<- 0.1 #intrinsic growth rate for slow-growing species()
r_med <- 0.5 #intrinsic growth rate for medium-growing species ()
r_fast <- 0.8 #intrinsic growth rate for fast-growing species ()

K <- 10000 #carrying capacity (metric tons)
phi <- 0.188 #shape parameter
g <- ((r_slow * phi) / (phi + 1))  #growth parameter

B0_open <- 0.1 * K #Assume initial biomass is being fished at open access

B_t <- B0_open #at time 1 B_t = B0

# Define timescale (t)
tyears <- 100

#Find F_oa
F_slow <- F_oa(g = g, phi = phi)

#Reset B_t
B_t <- B0_open

#Looped pt model:
for(t in 2:tyears){
  B_t[t] <- pt(B_t = B_t[t-1], K = K, phi = phi, g = g, H = F_slow*B_t[t-1])
}

plot(B_t)

# save results in a data frame

# create a time vector
time <- c(1:100)

# create a data frame with time vector
results_slow <- as.data.frame(time)

# add model loop result vector to data frame
results_slow$B_t <- B_t
