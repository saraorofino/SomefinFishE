#Calculate growth rate (g) based on intrinsic growth rate (r)
## Equation based on Mangin et al 2018

g <- function(r, phi){
   (r * phi) / (phi + 1)
}