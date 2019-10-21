#BioModel - Pella Tomlinson Surplus Production Model
##Parameterized based on Free et al 2019

#Gives biomass in metric tons:

bioModel_1 <- function(B_t, r, K, p, H){
  B_t <- B_t + (
    ((r/p) * B_t) *
      (1-((B_t/K) ^ p)
       )
  ) - H
  return(B_t)
}


##########################
#Parameterized based on Costello et al 2016
#Where b = B_t/BMSY and f = F_t/FMSY


bioModel_2 <- function(b, f, g, phi){
  b_next = b + (((phi + 1) / phi) * g * b * (1 - (b ^ phi)/(phi + 1))) - (g * f * b)
  return(b_next)
}
