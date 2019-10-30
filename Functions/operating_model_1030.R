# Operating model - this is our basic core model
## Initial version created 10/28 with help from Dan

##############################
# Step 1- Create df of experiments to run through the model
## Example for setup

n_experiment = 10 #Number of experiments we are running

experiment <- data.frame(
  r = runif(n_experiment, min = 0.05, max = 1.2),
  r_s = runif(n_experiment, min = -0.01, max = 0.01),
  k = runif(n_experiment, min = 0.001, max = 1.00)) # Here we are specifying the range for all the inputs we are varying in this model run
##############################
# Step 2 - define the simulation model

sim_fishery <- function(b, f, r, r_s, p, k, years){
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
    
  #   if (manager == "climate ready"){
  #     results$c[t] = results$g[t] * f * 
  #       (results$b[t] * exp(rnorm(1, 0, e))) #example of adding climate management with log normal observation error 
  #   }
  #   
  #   if (manager != "climate ready"){
  #     results$c[t] = g * f * (results$b[t] * exp(rnorm(1, 0, e)))
  #   }
   } 
  
  return(results)
}

results = sim_fishery(b = 1, f = 0.1, r = 0.2, p = 0.2,
                      years = 300, r_s = -0.01, k = 100) #Test the model to be sure it works
plot(results$b) #View results to make sure they make sense

####################
# Step 3 - Run the model for the experiments 

results = list() #we want the results to be outputted as a list

#Loop the simualation model over the experiments
for (i in 1:n_experiment){
  results[[i]] <- sim_fishery(b = 1, f = 0.05, r = experiment$r[i], r_s = experiment$r_s[i], p = 0.2,
                              years = 50, k = 1)
}
#Note if you're getting a data has more rows type error you forgot to run the results = list() command first 

plot(results[[2]]$b) #plot results for b from the 2nd experiment
