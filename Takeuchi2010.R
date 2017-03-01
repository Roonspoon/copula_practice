# This code is to reproduce the plots from Takeuchi (2010)

# move to tables directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
setwd('../tables')


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



png(paste0('lowerBoundFH.png'))
contourplot2(x.W) # contour plot of W
dev.off()
png(paste0('upperBoundFH.png'))
contourplot2(x.M) # contour plot of M
dev.off()




