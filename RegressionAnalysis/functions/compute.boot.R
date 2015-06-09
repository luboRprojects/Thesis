compute.boot <- function(data, year=2010, R=500){
library(dplyr)
library(boot)

bootfit <- function(data, indices){
 fit.boot <- glm(formula, data=data[indices, ], family = binomial)
 return(coef(fit.boot))
}

data.temp <- data %>% filter(year==year)

n.samples <- R
results <- boot(data=data.temp, statistic = bootfit, R = n.samples)
boot.est <- data.frame(results$t)
colnames(boot.est) <- names(results$t0)
 colnames(boot.est)[1] <- "Intercept"
boot.est$id <- 1:n.samples

boot.est
}