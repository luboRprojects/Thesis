#================ LOGISTIC REGRESSION ANALYSIS ================
#-------- Read data --------
data.in <- read.table("C:/Users/user/Disk Google/Disertace/Analysis/Survival/Data/final_data.txt",
 sep=";", header=TRUE)

#---- Adjust data ----
data <- data.in 
data$def.comp <- ifelse(data$def==0, 1, 0)
str(data.in)

# Manual over-ride --
data$acid.test <- ifelse(data$acid.test>4, 4, data$acid.test)

data.scaled <- data.frame(cbind(data[ ,c(1:4, 9)], apply(data[, 5:8], 2, scale)))

# summary(data[, 5:8])
# apply(data[, 5:8], 2, sd) 
# apply(data.scaled[, 5:8], 2, sd)
# data <- data.scaled()
#-------- Libraries --------
library(dplyr)
library(lme4)
library(boot)
library(reshape2)
library(ggplot2)
library(GGally)
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
colnames(coef.glm)[1] <- "Intercept"
coef.glm


data %>% filter(year==2012) %>% 
select(acid.test, debt.ratio, asset.turn, returns) %>%
ggpairs( alpha=0.7, colour="1") + theme_bw()
# pdf("efa_data.pdf")

# Demonstrate non-linear interpretation:
demonstration.sample <- data.frame(cbind(
ICO = c(1,2,3),
acid.test = c(0,0,0),
debt.ratio = c(0,0,0),
asset.turn = c(0,0,0),
returns = c(0.01,0.11,0.21))
)

predict(fit.12, demonstration.sample, type="response")

#---------- Bootstrap coefficients ----
source("C:/Users/user/Disk Google/Disertace/Analysis/GLMM/functions/compute.boot.R")

b08 <- compute.boot(data, year=2008, R=1000)
b09 <- compute.boot(data, year=2009, R=1000)
b10 <- compute.boot(data, year=2010, R=1000)
b11 <- compute.boot(data, year=2011, R=1000)
b12 <- compute.boot(data, year=2012, R=1000)

#---Snapshot Analysis of 2010 Sampling Distributions -----
source("C:/Users/user/Disk Google/Disertace/Analysis/Survival/Data/compute_hpd_data.R")

boot.est <- b12
data.hpd(boot.est[ ,1:5], alpha=0.01)$data %>% cbind(id=boot.est[ ,6]) %>%
 melt(id.var="id") %>%
ggplot(aes(x=value) ) + 
geom_histogram(col="darkblue", fill="grey") + 
facet_grid(.~variable, scales="free") + theme_bw() + scale_x_continuous("Value of bootstrap estimate") +
ggtitle("Histogram of 1000 bootstrapped estimates on 2012 dataset.\n ")

#ggsave("boot_est_glm_2012.pdf")

#------------------------
# TODO ----

plot1 <- boot.est %>% ggplot(aes(x=acid.test, y=debt.ratio)) + 
 geom_point(colour="darkblue", alpha=0.5) + 
 scale_y_continuous("Debt Ratio") + scale_x_continuous("Acid Test") + 
theme_bw()

plot2 <- boot.est %>% ggplot(aes(x=Intercept, y=debt.ratio)) + 
 geom_point(colour="darkblue", alpha=0.5) + 
 scale_y_continuous("Debt Ratio") + scale_x_continuous("Intercept") + theme_bw()

library(gridExtra)

pdf("aa.pdf", width=12, height=5)
 grid.arrange(plot1, plot2, ncol=2)
dev.off()
#------------------------
# --- Predictions ---
yhat09 <- predict(fit.09, type="response", data=data, subset=year==2009)
def <- data %>% filter(year==2009) %>% select(def)
foo09 <- data.frame(yhat09, def)

foo09 %>% group_by(def) %>% summarise(min=min(yhat09), mean=mean(yhat09), max=max(yhat09) ) 

# ggplot(foo09, aes(x=yhat09) ) + geom_histogram() + facet_grid(.~def, scales="free")

#--- General Framework
source("C:/Users/user/Disk Google/Disertace/Analysis/GLMM/functions/boot.pred.R")

p08 <- boot.pred(data, coef=b08, year=2008)
p09 <- boot.pred(data, coef=b09, year=2009)
p10 <- boot.pred(data, coef=b10, year=2010)
p11 <- boot.pred(data, coef=b11, year=2011)
p12 <- boot.pred(data, coef=b12, year=2012)

library(fanplot) #?fan

data %>% group_by(ICO) %>% summarise(count=n()) %>% filter(count==5)
show.ico <- 543551

#--- General FanPlot ---
source("C:/Users/user/Disk Google/Disertace/Analysis/GLMM/functions/make_fan.R")

make.fan(25217194)

#--- General Compare 2 FanPlots ---
source("C:/Users/user/Disk Google/Disertace/Analysis/GLMM/functions/make_fans.R")

icos <- c(543551, 554812)
make.fans(icos)

cairo_pdf("fanplot_2companies_comparison.pdf", width=11, height=6)
 make.fans(icos)
dev.off()

#--- Compute Statistics
icos <- c(543551, 554812)
ico1 <- 543551
source("C:/Users/user/Disk Google/Disertace/Analysis/GLMM/functions/pred_quantiles.R")

pred_quantiles(ico1)

# Used in Thesis
library(xtable)
quantiles <- pred_quantiles(ico1)
xtable(quantiles)


#========= For Open Bugs =============
export12 <- data %>% dplyr::filter(year==2012) %>% select(-class, -def.comp)
write.table(x=export12, file="export12.txt", row.names=FALSE)























#===================================
#======	DEV	========
#===================================
summary(fit.10)

compare <- data.frame(rbind(
 apply(b10, 2, function(x){quantile(x, probs=0.5)}),
 apply(b10, 2, mean),
 apply(b10, 2, sd),
 round(dnorm(abs(apply(b10, 2, mean)/apply(b10, 2, sd)))),3 )
rownames(compare) <- c("med.est", "mean", "se", "ruff.pval")
compare

coef.glm

d08 <- data %>% filter(year==2008)



#--- Analyse Boot ---
results$t0
summary(results$t)

boot.est %>% select(-id) %>% ggpairs(alpha=0.3) + theme_bw()



#--- Prediction - manually

check.n <- 3
coef09 <- coef(fit.09)
d09 <- data %>% filter(year==2009) %>% select (acid.test, debt.ratio, asset.turn, returns) %>% head(check.n)
covar <- cbind(intercept=1, d09)

coef09 %*% t(covar)
predict(fit.09, data=data, subset=year==2009)[1:check.n]
predict(fit.09, type="response", data=data, subset=year==2009)[1:check.n]

my.logit <- function(p){(1 /(1+exp(-p)))}
my.logit(coef09 %*% t(covar))


