#Maximum Sustainable Yield
##Equations based on parameterization from Free et al 2019 

#Equation for MSY:
msy_1 <- function(r, K, p){
  msy = (r * K) / ((p + 1) ^ ((p + 1)/p))
  return(msy)
}


#Equation for biomass at MSY:
bmsy_1 <- function(K, p){
  bmsy = K / ((p + 1) ^ ((1+p) / p))
}

##########################
#Equations based on parameterization from Costello et al 2016

msy_2 <- function(g, K, phi){
  msy = (g * K) / ((phi + 1) ^ (1/phi))
  return(msy)
}

bmsy_2 <- function(K, phi){
  bmsy = K / ((phi + 1) ^ (1/phi))
  return(bmsy)
}

