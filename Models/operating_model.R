#Operating model with help from Dan:

# Create df of experiments
## Example for setup
n_experiment = 10

experiment <- data.frame(
  g = runif(n_experiment, min = 0.05, max = 1.2),
  g_s = runif(n_experiment, min = -0.01, max = 0.01)
)

# Define the simulation model 
sam_fishery <- function(b, f, g, g_s, phi, years, manager, e){
  # b = 1
  # f = 1.5
  # g = 0.2
  # g_s = -0.01
  # phi = 0.188
  # years = 50
  results <- data.frame(
    b = rep(NA, years), c = rep(NA, years), year = 1:years,
    g = rep(NA, years)
    
  )
  results$b[1] = b
  results$c[1] = g * f * b
  results$g[1] = g
  
  for (t in 2:years) {
  results$b[t] = results$b[t-1] + (((phi + 1) / phi) * results$g[t-1] * results$b[t-1] * (1 - (results$b[t-1] ^ phi)/(phi + 1))) - results$c[t-1]
  results$g[t] = results$g[t-1] * (1 + g_s)
  if (manager == "climate ready"){
    results$c[t] = results$g[t] * f * 
      (results$b[t] * exp(rnorm(1, 0, e))) #example of adding climate management
  }
  if (manager != "climate ready"){
    results$c[t] = g * f * (results$b[t] * exp(rnorm(1, 0, e)))
  }
  }
  return(results)
}

results = sam_fishery(b = 1, f = 1, g = 0.2, phi = 0.188,
                      years = 50, g_s = -0.01, manager = "climate ready",
                      e = 0.1)
plot(results$b)

#Run the model for the 10 experiments


results = list()


for (i in 1:n_experiment){
  results[[i]] <- sam_fishery(b = 1, f = 1, g = experiment$g[i], g_s = experiment$g_s[i], phi = 0.188,
              years = 50, manager = "climate ready", e = 0.1)
}

plot(results[[2]]$b) #plot results for b from the 2nd experiment
