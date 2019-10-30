# Testing model behavior: Initial biomass

## This version tests the effect of varying intial biomass. Experiments will vary initial biomass
## but all other inputs in the model will remain constant 

# Set up a small trial number of experiments - vary biomass 
n_experiment = 10 

experiment <- data.frame(
  b = runif(n_experiment, min = 0.1, max = 0.60)) #low end is 0.1K and high is 0.6K (from table)

# Model:
test_b <- function(b, f, r, r_s, p, k, years){
  # b = 1
  # f = 1.5
  # g = 0.2
  # g_s = -0.01
  # phi = 0.188
  # years = 50   Note: these values were to help setup the initial model and ensure it ran properly - we don't need these anymore
  
  results <- data.frame(
    b = rep(NA, years), c = rep(NA, years), 
    year = 1:years, r = rep(NA, years)) #Setup the results dataframe where the outputs will be writen for however many years our experiment is running
  
  #Set the initial result for the outputs in year 1
  results$b[1] = b
  results$c[1] = f * b
  results$r[1] = r
  
  #Loop the model over the specified number of years
  for (t in 2:years) {
    
    results$b[t] = results$b[t-1] + (results$r[t-1] / p) * results$b[t-1] * (1 - ((results$b[t-1]/k) ^ p)) - results$c[t-1] #operating model - PT
    results$r[t] = results$r[t-1] * (1 + r_s) #climate model - how does r change with climate change in each year
    results$c[t] = results$b[t-1] * f #define catch - won't need this if using the climate ready management below

  } 
  
  return(results)
}


# Run model over experiments:
results_b = list() 

for (i in 1:n_experiment){
  results_b[[i]] <- test_b(b = experiment$b[i], f = 0.1, r = 0.2, r_s = 0.01, p = 0.2,
                              years = 50, k = 1)
}


#Plot the change in biomass over time for all 10 experiments:
for(i in 1:n_experiment){
  plot(results_b[[i]]$b)
}

#Save a copy of plots to look at later:
pdf(file = "G:/Data Analysis/SomefinFishE/Models/behavior_checks/b/b_plots.pdf",
    width = 4,
    height = 4)

for(i in 1:n_experiment){
  plot(results_b[[i]]$b)
}

dev.off()
