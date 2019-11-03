# Testing model behavior: Range shifts

## This version tests the impact of the magnitude of climate change on range shifts. Experiments will vary ks
## but all other inputs in the model will remain constant (assume NO change in growth rates)

# Set up a small trial number of experiments  
n_experiment = 10 

experiment <- data.frame(
  k_s = seq(-0.01, -0.001, 0.001)) #Range shift per year
#Range shift based on a low of no movement in 100 yrs and a high of 100% in 100 yrs or 1% per year

# Model:
test_ks <- function(b, f, r, p, k, k_s, years){
  # b = 1
  # f = 1.5
  # g = 0.2
  # g_s = -0.01
  # phi = 0.188
  # years = 50   Note: these values were to help setup the initial model and ensure it ran properly - we don't need these anymore
  
  results <- data.frame(
    b = rep(NA, years), c = rep(NA, years), 
    year = 1:years, k = rep(NA, years),
    k_s = rep(NA, years)) #Setup the results dataframe where the outputs will be writen for #years our experiment is running
  
  #Set the initial result for the outputs in year 1
  results$b[1] = b
  results$c[1] = f * b
  results$k[1] = k
  results$k_s[1] = k_s
  
  #Loop the model over the specified number of years
  for (t in 2:years) {
    
    results$c[t] = results$b[t-1] * f #define catch - won't need this if using the climate ready management below
    results$b[t] = results$b[t-1] + (r / p) * results$b[t-1] * (1 - ((results$b[t-1]/results$k[t-1]) ^ p)) - results$c[t] #operating model-PT
    results$k_s[t] = k_s*(t-1)
    results$k[t] = results$k[1] * (1 + (k_s*(t-1))) #climate model - how does k change with climate change in each year
    
  } 
  
  return(results)
}


# Run model over experiments:
results_ks = list() 

for (i in 1:n_experiment){
  results_ks[[i]] <- test_ks(b = 6000, f = 0.1, r = 0.2, p = 0.2, k_s = experiment$k_s[i],
                             years = 50, k = 10000)
}


#Plot the change in biomass over time for all 10 experiments:
for(i in 1:n_experiment){
  plot(results_ks[[i]]$b)
}

#Save a copy of plots to look at later:
# pdf(file = "/Users/saraorofino/Documents/GitHub/SomefinFishE/Models/behavior_checks/ks/b_plots.pdf",
#     width = 4,
#     height = 4)
# 
# for(i in 1:n_experiment){
#   plot(results_ks[[i]]$b)
# }
# 
# dev.off()

# Have a vector of starting k_s values to compare to the graphs
library(tidyverse)
ks_intial <- as.data.frame(experiment$k_s) %>% 
  rename(k_s = "experiment$k_s")
