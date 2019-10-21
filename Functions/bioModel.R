#BioModel - Pella Tomlinson Surplus Production Model
##Parameterized based on Free et al 2019

#Gives biomass in metric tons:

bioModel <- function(B_t, r, K, p, H){
  B_t <- B_t + (
    ((r/p) * B_t) *
      (1-((B_t/K) ^ p)
       )
  ) - H
  return(B_t)
}
