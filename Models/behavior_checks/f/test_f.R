# Testing model behavior: Fishing Mortality Rate

## This version tests the effect of varying fishing mortality rate. Experiments will vary f
## but all other inputs in the model will remain constant 

# Set up a small trial number of experiments
n_experiment = 10 

experiment <- data.frame(
  f = seq(0.01, 0.91, 0.1)) #low end 1% of fish, high end 90% of fish present will be caught

# Model:
test_f <- function(b, f, r, r_s, p, k, years){
  # b = 1
  # f = 1.5
  # g = 0.2
  # g_s = -0.01
  # phi = 0.188
  # years = 50   Note: these values were to help setup the initial model and ensure it ran properly - we don't need these anymore
  
  results <- data.frame(
    b = rep(NA, years), c = rep(NA, years), 
    year = 1:years, r = rep(NA, years)) #Setup the results dataframe where the outputs will be writen for #years our experiment is running
  
  #Set the initial result for the outputs in year 1
  results$b[1] = b
  results$c[1] = f * b
  results$r[1] = r
  
  #Loop the model over the specified number of years
  for (t in 2:years) {
    
    results$c[t] = results$b[t-1] * f #define catch - won't need this if using the climate ready management below
    results$b[t] = results$b[t-1] + (results$r[t-1] / p) * results$b[t-1] * (1 - ((results$b[t-1]/k) ^ p)) - results$c[t] #operating model-PT
    results$r[t] = results$r[1] * (1 + (r_s*(t-1))) #climate model - how does r change with climate change in each year
    
  } 
  
  return(results)
}


# Run model over experiments:
results_f = list() 

for (i in 1:n_experiment){
  results_f[[i]] <- test_f(b = 6000, f = experiment$f[i], r = 0.2, r_s = 0, p = 0.2,
                                years = 50, k = 10000)
}


#Plot the change in biomass over time for all 10 experiments:
for(i in 1:n_experiment){
  plot(results_f[[i]]$b)
}

#Save a copy of plots to look at later:
# library(here)
# pdf(file = "/Users/saraorofino/Documents/GitHub/SomefinFishE/Models/behavior_checks/f/b_plots.pdf",
#     width = 4,
#     height = 4)
# 
# for(i in 1:n_experiment){
#   plot(results_f[[i]]$b)
# }
# 
# dev.off()

# Have a vector of starting f values to compare to the graphs
library(tidyverse)
f_intial <- as.data.frame(experiment$f) %>% 
  rename(f = "experiment$f")
