# Testing model behavior: Change in r

## This version tests the magnitude of climate change on r. Experiments will vary r_s (change in growth per degree C)
## but all other inputs in the model will remain constant 

# Set up a small trial number of experiments
n_experiment = 10 

experiment <- data.frame(
  r_s = seq(-0.015, 0.020, 0.002)) #range of values from the table

# Model:
test_rs <- function(b, f, r, r_s, p, k, years){
  # b = 1
  # f = 1.5
  # g = 0.2
  # g_s = -0.01
  # phi = 0.188
  # years = 50   Note: these values were to help setup the initial model and ensure it ran properly - we don't need these anymore
  
  results <- data.frame(
    b = rep(NA, years), c = rep(NA, years), 
    year = 1:years, r = rep(NA, years)) #Setup the results dataframe 
  
  
  #Set the initial result for the outputs in year 1
  results$b[1] = b
  results$c[1] = f * b
  results$r[1] = r
  
  #Loop the model over the specified number of years
  for (t in 2:years) {
    
    results$c[t] = results$b[t-1] * f #define catch 
    results$b[t] = results$b[t-1] + (results$r[t-1] / p) * results$b[t-1] * 
      (1 - ((results$b[t-1]/k) ^ p)) - results$c[t] #operating model-PT
    results$r[t] = results$r[1] * (1 + (r_s*(t-1))) #climate model
    
  } 
  
  return(results)
}


# Run model over experiments:
results_rs = list() 

for (i in 1:n_experiment){
  results_rs[[i]] <- test_rs(b = 6000, f = 0.1, r = 0.2, r_s = experiment$r_s[i], p = 0.2,
                                years = 50, k = 10000)
}


#Plot the change in biomass over time for all 10 experiments:
for(i in 1:n_experiment){
  plot(results_rs[[i]]$b)
}

#Save a copy of plots to look at later:
# pdf(file = "/Users/saraorofino/Documents/GitHub/SomefinFishE/Models/behavior_checks/rs/b_plots.pdf",
#     width = 4,
#     height = 4)
# 
# for(i in 1:n_experiment){
#   plot(results_rs[[i]]$b)
# }
# 
# dev.off()

# Have a vector of starting r_s values to compare to the graphs
library(tidyverse)
rs_intial <- as.data.frame(experiment$r_s) %>% 
  rename(r_s = "experiment$r_s")
