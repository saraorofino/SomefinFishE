#Combined functions script to run through model
# All parameterized according to Mangin, Gaines, and Costello models

#################
#Biomodel - Note f = F/Fmsy and b = B/Bmsy
bioModel <- function(b, f, g, phi){
  b_next = b + (((phi + 1) / phi) * g * b * (1 - (b ^ phi)/(phi + 1))) - (g * f * b)
  return(b_next)
}

#################
#Growth rate
g <- function(r, phi){
  (r * phi) / (phi + 1)
}

################
#MSY 
msy <- function(g, K, phi){
  msy = (g * K) / ((phi + 1) ^ (1/phi))
  return(msy)
}

################
#Bmsy
bmsy <- function(K, phi){
  bmsy = K / ((phi + 1) ^ (1/phi))
  return(bmsy)
}

################
#Fishing mortality at open access 
F_oa <- function(g, phi){
  ((phi + 1) / phi) * g * (
    (0.3^phi) / (1 + phi)
  )
} 

###############
