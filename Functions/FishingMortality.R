#Fishing Mortality Rate
##Based on the equation from Gaines et al 2018 but hand derived using parameterization from Free et al 2019 


#Open Access fishing mortality is the rate would yield 0.3*BMSY

F_oa <- function(r,p){
  (r/p) * (1-
             ((0.3 ^ p)/
                ((p+1) ^ (1+p)))
  )
}
