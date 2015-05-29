#================ LOGISTIC REGRESSION ANALYSIS ================
#-------- Read data --------
data.in <- read.table("C:/Users/user/Disk Google/Disertace/Analysis/Survival/Data/final_data.txt",
 sep=";", header=TRUE)

#---- Adjust data ----
data <- data.in 
data$def.comp <- ifelse(data$def==0, 1, 0)
str(data.in)

#-------- Libraries --------
library(dplyr)
library(lme4)

#========== Cross-sectional analysis =========
data %>% group_by(year, def) %>% summarise(count = n() )
da

formula <- "cbind(data$def,data$def.comp) ~ 
acid.test + debt.ratio + asset.turn + returns"

fit.09 <- glm(formula, data=data, subset=year==2009, family = binomial)
fit.10 <- glm(formula, data=data, subset=year==2010, family = binomial)
fit.11 <- glm(formula, data=data, subset=year==2011, family = binomial)
fit.12 <- glm(formula, data=data, subset=year==2012, family = binomial)
fit.13 <- glm(formula, data=data, subset=year==2013, family = binomial)




