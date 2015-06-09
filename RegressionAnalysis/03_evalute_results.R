# ROCR analysis
library(ROCR)
library(ggplot2)
library(MASS)

# Data ----
data.11 <- data %>% filter(year==2011)
data.12 <- data %>% filter(year==2012)

# GLM ---------------
model.11 <- fit.11 # from 01_glmm.R
pred.glm.11.11 <- predict(model.11, data.11, type="response")
pred.glm.11.12 <- predict(model.11, data.12, type="response")

# LDA ---------------
lda.11 <- lda(def ~ acid.test + debt.ratio + asset.turn + returns, data=data.11)
lda.12 <- lda(def ~ acid.test + debt.ratio + asset.turn + returns, data=data.12)
pred.lda.11.11 <- predict(lda.11, data.11)$posterior[, 2]
pred.lda.11.12 <- predict(lda.11, data.12)$posterior[, 2]

# QDA ---------------
qda.11 <- qda(def ~ acid.test + debt.ratio + asset.turn + returns, data=data.11)
qda.12 <- qda(def ~ acid.test + debt.ratio + asset.turn + returns, data=data.12)
pred.qda.11.11 <- predict(qda.11, data.11)$posterior[, 2]
pred.qda.11.12 <- predict(qda.11, data.12)$posterior[, 2]

# Find the threshold ---------------
cost.matrix <- data.frame(event=c("00", "01", "10", "11"), cost=c(5,-20,-5,15) )

source("C:/Users/user/Disk Google/Disertace/Analysis/GLMM/functions/compute.cost.R")
cost.res.glm <- compute.cost(def=data.11$def, predicted=pred.glm.11.11, prec=0.001)
cost.res.lda <- compute.cost(def=data.11$def, predicted=pred.lda.11.11, prec=0.001)
cost.res.qda <- compute.cost(def=data.11$def, predicted=pred.qda.11.11, prec=0.001)

#========= GLM ==========
foo.11 <- data.frame(
 yhat = ifelse(pred.glm.11.11 >= cost.res$solution$thresholds[1], 1, 0),
 def = data.11$def)

foo.12 <- data.frame(
 yhat = ifelse(pred.glm.11.12 >= cost.res$solution$thresholds[1], 1, 0),
 def = data.12$def)

#========= LDA ==========
foo.11 <- data.frame(
 yhat = ifelse(pred.lda.11.11 >= cost.res$solution$thresholds[1], 1, 0),
 def = data.11$def)

foo.12 <- data.frame(
 yhat = ifelse(pred.lda.11.12 >= cost.res$solution$thresholds[1], 1, 0),
 def = data.12$def)

#========= QDA ==========
foo.11 <- data.frame(
 yhat = ifelse(pred.qda.11.11 >= cost.res$solution$thresholds[1], 1, 0),
 def = data.11$def)

foo.12 <- data.frame(
 yhat = ifelse(pred.qda.11.12 >= cost.res$solution$thresholds[1], 1, 0),
 def = data.12$def)

#========= Summarise results ====
# table(foo.11$def)
#table(foo.12$def)

table(foo.11)
table(foo.12)

# Final Comparison ---------------
predictions.11 <- data.frame(data.12[, 1:3], predicted.glm, predicted.lda)

pred.glm <- prediction(prediction=data.prediction$predicted.glm, labels=data.prediction$def)
pred.lda <- prediction(prediction=data.prediction$predicted.lda, labels=data.prediction$def)

# ROC plot --
perf.glm <- performance(pred.glm,"tpr","fpr") 
 plot(perf.glm)
perf.lda <- performance(pred.lda,"tpr","fpr") 
 plot(perf.lda)


summary(predicted.glm)

data.12$def

# DEV ---------------
table(predict(lda1, data.12)$class, data.12$def)
# From 21 defaulted were 9 companies assigned as 0



