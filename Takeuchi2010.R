# This code is to reproduce the plots from Takeuchi (2010)
#
# So far the following plots are produced:
#
# upperBoundFH.png   - the Frechet-Hoeffding upper bound,
#                      this corresponds to the left panel of
#                      figure 1 in T10. However it is a 2D
#                      contour plot rather than 3D as in T10.
# lowerBoundFH.png   - the Frechet-Hoeffding lower bound,
#                      this corresponds to the right panel of
#                      figure 1 in T10. However it is a 2D
#                      contour plot rather than 3D as in T10.
#
#



# Import required packages:
library(scatterplot3d)

# move to tables directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
setwd('Figures')

# First the Frechet-Hoeffding upper and lower bounds

## For 'matrix' objects
## The Frechet--Hoeffding bounds W and M
n.grid <- 1e3
u <- seq(0, 1, length.out = n.grid)
grid <- expand.grid("u[1]" = u, "u[2]" = u)
W <- function(u) pmax(0, rowSums(u)-1) # lower bound W
M <- function(u) apply(u, 1, min) # upper bound M
x.W <- cbind(grid, "W(u[1],u[2])" = W(grid)) # evaluate W on 'grid'
x.M <- cbind(grid, "M(u[1],u[2])" = M(grid)) # evaluate M on 'grid'

##########
# Do you want to save the 2D contour plots? 
# This takes some time to produce, so I advise only setting to True
# when totally necessary.
saveFrechHoeff = T
if (saveFrechHoeff == T){
  png(paste0('lowerBoundFH.png'))
  contourplot2(x.W) # contour plot of W
  dev.off()
  png(paste0('upperBoundFH.png'))
  contourplot2(x.M) # contour plot of M
  dev.off()
}
##########
# Do you want to plot the 2D contour plots? 
# This takes some time to produce, so I advise only setting to True
# when totally necessary.
plotFrechHoeff = F
if (plotFrechHoeff == T){
  contourplot2(x.W) # contour plot of W
  contourplot2(x.M) # contour plot of M
}


#########
# plot the 3D surface of the Frechet-Hoeffding upper and lower bounds
## For 'matrix' objects
## The Frechet--Hoeffding bounds W and M
n.grid <- 26
u <- seq(0, 1, length.out = n.grid)
grid <- expand.grid("u[1]" = u, "u[2]" = u)
W <- function(u) pmax(0, rowSums(u)-1) # lower bound W
M <- function(u) apply(u, 1, min) # upper bound M
x.W <- cbind(grid, "W(u[1],u[2])" = W(grid)) # evaluate W on 'grid'
x.M <- cbind(grid, "M(u[1],u[2])" = M(grid)) # evaluate M on 'grid'
# wireframe2(x.W)
# wireframe2(x.M)

if (saveFrechHoeff==T) { 
  png(paste0('lowerBoundFH_3Dsurface.png'))
  wireframe2(x.W, shade = F, drape = T, col.regions='blue',col='blue',colorkey=F) # plot of W
  dev.off()
  png(paste0('upperBoundFH_3Dsurface.png'))
  wireframe2(x.M, shade = F, drape = T, col.regions='blue',col='blue',colorkey=F) # plot of W
  dev.off()
}
if (plotFrechHoeff==T) { 
  png(paste0('lowerBoundFH_3Dsurface.png'))
  wireframe2(x.W, shade = F, drape = T, col.regions='blue',col='blue',colorkey=F) # plot of W
  dev.off()
  png(paste0('upperBoundFH_3Dsurface.png'))
  wireframe2(x.M, shade = F, drape = T, col.regions='blue',col='blue',colorkey=F) # plot of W
  dev.off()
}

## For 'Copula' objects
cop <- frankCopula(-4)
wireframe2(cop, pCopula) # the copula
wireframe2(cop, pCopula, shade = TRUE) # ditto, "shaded"
wireframe2(cop, pCopula, shade = TRUE, col = "gray60") # ditto, "shaded"+grid
wireframe2(cop, pCopula, drape = TRUE, xlab = quote(x[1])) # adjusting an axis label
wireframe2(cop, dCopula, delta=0.01) # the density
wireframe2(cop, dCopula) # => the density is set to 0 on the margins


## For 'mvdc' objects
mvNN <- mvdc(gumbelCopula(3), c("norm", "norm"),
             list(list(mean = 0, sd = 1), list(mean = 1)))
wireframe2(mvNN, dMvdc, xlim=c(-2, 2), ylim=c(-1, 3))


setwd('..')