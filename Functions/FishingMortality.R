#Fishing Mortality Rate
##Based on the equation from Gaines et al 2018 but hand derived using parameterization from Free et al 2019 


#Open Access fishing mortality is the rate would yield 0.3*BMSY
F_oa1 <- function(r,p){
  (r/p) * (1-
             ((0.3 ^ p)/
                ((p+1) ^ (1+p)))
  )
}


####Parameterized from Gaines et al 2018 
F_oa2 <- function(g, phi){
  ((phi + 1) / phi) * g * (
    (0.3^phi) / (1 + phi)
  )
} 


#In steady state (from Costello et al 2016):
f_t <- function(b, phi){
  ((phi + 1) / phi) * (1 - ((b ^ phi) / (phi + 1)))
}
