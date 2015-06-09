#================ LOGISTIC REGRESSION ANALYSIS ================
#-------- Read data --------
data.in <- read.table("C:/Users/user/Disk Google/Disertace/Analysis/Survival/Data/final_data.txt",
 sep=";", header=TRUE)

#---- Adjust data ----
data <- data.in 
data$def.comp <- ifelse(data$def==0, 1, 0)
data$acid.test <- ifelse(data$acid.test>4, 4, data$acid.test)
str(data.in)

#-------- Define Data --------
data.scaled <- data.frame(cbind(data[ ,c(1:4, 9)], apply(data[, 5:8], 2, scale)))%>% 
 mutate(year=1+year-min(year))
data.glmm <- data %>% mutate(year=1+year-min(year))
str(data.glmm)

#-------- Libraries --------
library(dplyr)
library(lme4)
library(boot)

#-------- Fit the Model --------
formula.glmm <- "cbind(data$def,data$def.comp) ~ 
acid.test + debt.ratio + asset.turn + returns + (1+year|ICO)"

glmm1 <- glmer(formula.glmm, data.glmm, family="binomial")
 summary(glmm1)

glmm2 <- glmer(formula.glmm, data.scaled, family="binomial")
 summary(glmm2)


#-------- Make Predictions --------
predicted <- predict(glmm1, type="response")
res.glmm <- data.frame(ICO=data.glmm$ICO, year=data.glmm$year, predicted, def=data.glmm$def)

library(lattice)
library(ggplot2)
select.icos <- c(543551, 554812)

res.glmm %>% filter(ICO %in% select.icos ) %>% mutate(year=year+min(data$year)-1) %>% 
 ggplot(aes(x=year, y=predicted) ) + geom_line(size=1.5, colour="darkblue") + 
 facet_grid(.~ICO) +  theme_bw()

#=============