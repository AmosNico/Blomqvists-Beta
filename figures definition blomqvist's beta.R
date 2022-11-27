# An example from a bivariate normal distribution to illustrate how the lines 
# x=med_x and y = med_y separate the plane

library("MASS")  # library for the multivariate normal distribution
set.seed(1)
n <- 500          # Number of data points
mu <- c(0, 0)     # Means of the variables
sigma <- matrix(c(2, 2, 1, 4), ncol = 2) # covariance matrix

data <- mvrnorm(n, mu, sigma)

medx = median(data[,1])
medy = median(data[,2])

plot(data, ylab="Y", xlab="X", 
     col = ifelse(data[,1]<medx,
                  ifelse(data[,2]>medy, "blue", "red"),
                  ifelse(data[,2]>medy, "black", "green")))
abline(v=medx)
abline(h=medy)
title("The 4 quadrants for a sample of the bivariate normal distribution")



# An example to show that it is difficult to define the number of datapoints in 
# each quadrant if some of the points have the same x-or y-coordinate

x <- c(-2, -2, 0, 0, 1, 2)
y <- c(1, -2, 2, -1, 2, 0)

medx = median(x)
medy = median(y)

plot(NULL, xlim=c(-2,2), ylim=c(-2,2), ylab="Y", xlab="X")
abline(v=medx, lwd = 2)
abline(h=medy, lwd = 2)
points(x,y,col = ifelse(x==medx,"red","black"),pch=16,cex = 1.5)
title(expression(bold("Dataset with two points on the line x = M"["X"])))
