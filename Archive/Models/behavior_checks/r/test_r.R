# Testing model behavior: Intrinsic Growth Rate

## This version tests the effect of varying intrinsic growth rate. Experiments will vary r
## but all other inputs in the model will remain constant 

# Set up a small trial number of experiments  
n_experiment = 10 

experiment <- data.frame(
  r = seq(0.015, 1.15, 0.1))

# Model:
test_r <- function(b, f, r, r_s, p, k, years){
  
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
results_r = list() 

for (i in 1:n_experiment){
  results_r[[i]] <- test_r(b = 6000, f = 0.1, r = experiment$r[i], r_s = 0, p = 0.2,
                                years = 50, k = 10000)
}


#Plot the change in biomass over time for all 10 experiments:
for(i in 1:n_experiment){
  plot(results_r[[i]]$b)
}

#Save a copy of plots to look at later:
# pdf(file = "/Users/saraorofino/Documents/GitHub/SomefinFishE/Models/behavior_checks/r/b_plots.pdf",
#     width = 4,
#     height = 4)
# 
# for(i in 1:n_experiment){
#   plot(results_r[[i]]$b)
# }
# 
# dev.off()

# Have a vector of starting r values to compare to the graphs
library(tidyverse)
r_intial <- as.data.frame(experiment$r) %>% 
  rename(r = "experiment$r")