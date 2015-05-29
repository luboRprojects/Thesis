#================ LOGISTIC REGRESSION ANALYSIS ================
#-------- Read data --------
data.in <- read.table("C:/Users/user/Disk Google/Disertace/Analysis/Survival/Data/final_data.txt",
 sep=";", header=TRUE)

#---- Adjust data ----
data <- data.in 
data$def.comp <- ifelse(data$def==0, 1, 0)
str(data.in)

data.scaled <- data.frame(cbind(data[ ,c(1:4, 9)], apply(data[, 5:8], 2, scale)))
#-------- Libraries --------
library(dplyr)
library(lme4)
library(boot)
library(reshape2)
library(ggplot2)

#========== Cross-sectional analysis =========
data %>% group_by(year, def) %>% summarise(count = n() )

formula <- "cbind(data$def,data$def.comp) ~ 
acid.test + debt.ratio + asset.turn + returns"

fit.09 <- glm(formula, data=data, subset=year==2009, family = binomial)
fit.10 <- glm(formula, data=data, subset=year==2010, family = binomial)
fit.11 <- glm(formula, data=data, subset=year==2011, family = binomial)
fit.12 <- glm(formula, data=data, subset=year==2012, family = binomial)

coef.glm <- data.frame(rbind(coef(fit.09), coef(fit.10), coef(fit.11), coef(fit.12) ))
rownames(coef.glm) <- paste0("year", 2000+9:12) 
coef.glm

#---------- Bootstrap coefficients ----

bootfit <- function(data, indices){
 fit.boot <- glm(formula, data=data[indices, ], subset=year==2010, family = binomial)
 return(coef(fit.boot))
}

n.samples <- 1000
results <- boot(data=data, statistic = bootfit, R = n.samples)
boot.est <- data.frame(results$t)
colnames(boot.est) <- names(results$t0)
boot.est$id <- 1:n.samples

#----------------
source("C:/Users/user/Disk Google/Disertace/Analysis/Survival/Data/compute_hpd_data.R")

data.hpd(boot.est[ ,1:5])$data %>% cbind(id=boot.est[ ,6]) %>% melt(id.var="id") %>%
ggplot(aes(x=value) ) + 
geom_histogram(col="darkblue", fill="grey") + 
facet_grid(.~variable, scales="free") + theme_bw() + scale_x_continuous("Value of bootstrap estimate") +
ggtitle("Histogram of 1000 bootstrapped estimates on 2010 dataset.\n ")

ggsave("boot_est_glm_2010.pdf")



