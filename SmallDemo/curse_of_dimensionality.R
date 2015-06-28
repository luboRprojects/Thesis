# Curse of dimensionality:
# Show that the area under curve is changing as the dimensinality
# increases. 
# X ~ N(0,1)
# from x={-1,1} we have the mass = 0.6826895
pnorm(1) - (1-pnorm(1))

# In multivariate case:
library(mvtnorm)

mean <- c(0, 0) 
lower <- c(-1,-1) # vector of lower bound
upper <- c(1,1)
corr <- diag(2) # create diagonal matrix (variance = 1)
corr[lower.tri(corr)] <- 0 # uncorrelated variables
corr[upper.tri(corr)] <- 0

# From x1={-1,1} and x2={-1,1}
# Area under the curve is = 0.4660649
pmvnorm(lower, upper, mean, corr)

# This can be computed by Monte Carlo simulation:
n.samples = 1E4 # generate 10 000 observation
data <- data.frame(rmvnorm(n=n.samples, mean, sigma=corr)) 

# Class 1 consists of observation whose absolute values
# of x1 and x2 are <1

data$class <- ifelse( abs(data$X1)>=1 | abs(data$X2)>=1, 1, 0)

# Proportion of class 1 is ~0.465
table(data$class)/ nrow(data)

library(ggplot2)
ggplot(data, aes(x=X1, y=X2) ) + 
 geom_point(aes(colour=as.factor(class)), alpha=0.3) +
 theme_bw()

#--------
euclid.fun <- function(x){ sqrt( (x[1]-mean[1])^2 + (x[2]-mean[1])^2 )}
data <- data.frame(rmvnorm(n=10000, mean, sigma=corr))
data$dist <- apply(data, 1, euclid.fun)
data$class <- ifelse(data$dist<=1, 0, 1)
table(data$class)/ nrow(data)

#--------
data <- data.frame(rmvnorm(n=10000, mean, sigma=corr))
data$class <- ifelse( abs(data$X1)>=1 | abs(data$X2)>=1, 1, 0)
table(data$class)/ nrow(data) #0.4660649
