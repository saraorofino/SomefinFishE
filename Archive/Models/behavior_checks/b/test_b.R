# Testing model behavior: Initial biomass

## This version tests the effect of varying intial biomass. Experiments will vary initial biomass
## but all other inputs in the model will remain constant 

# Set up a small trial number of experiments - vary biomass 
n_experiment = 10 

experiment <- data.frame(
  b = seq(1000, 10000, 1000)) #low end is 0.1K and high is 0.6K (from table)

# Model:
test_b <- function(b, f, r, r_s, p, k, years){
  
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
results_b = list() 

for (i in 1:n_experiment){
  results_b[[i]] <- test_b(b = experiment$b[i], f = 0.1, r = 0.2, r_s = 0, p = 0.2,
                              years = 50, k = 10000)
}


#Plot the change in biomass over time for all 10 experiments:
for(i in 1:n_experiment){
  plot(results_b[[i]]$b)
}

# #Save a copy of plots to look at later:
# library(here)
# pdf(file = "/Users/saraorofino/Documents/GitHub/SomefinFishE/Models/behavior_checks/b/b_plots.pdf",
#     width = 4,
#     height = 4)
# 
# for(i in 1:n_experiment){
#   plot(results_b[[i]]$b)
# }
# 
# dev.off()

# Have a vector of starting b values to compare to the graphs
library(tidyverse)
b_intial <- as.data.frame(experiment$b) %>% 
  rename(b = "experiment$b")