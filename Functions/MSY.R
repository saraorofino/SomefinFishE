#Maximum Sustainable Yield
##Equations based on parameterization from Free et al 2019 

#Equation for MSY:
msy <- function(r, K, p){
  msy = (r * K) / ((p + 1) ^ ((p + 1)/p))
  return(msy)
}


#Equation for biomass at MSY:
bmsy <- function(K, p){
  bmsy = K / ((p + 1) ^ ((1+p) / p))
  return(bmsy)
}
