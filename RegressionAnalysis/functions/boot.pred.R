boot.pred <- function(data = data, coef = b09 , year = 2009){
 data.temp <- data
  colnames(data.temp)[which(colnames(data)=="year")] <- "year.select"
 icos.temp <- data.temp %>% filter(year.select==year) %>% select(ICO)
 data.temp <- data.temp %>% filter(year.select==year) %>% select (acid.test, debt.ratio, asset.turn, returns)
 covar.temp <- cbind(intercept=1, data.temp)

 coef.pred <- coef %>% select(-id)

est.p <- data.frame(matrix(0, nrow=nrow(icos.temp), ncol=nrow(coef.pred)) )

for(i in 1:nrow(icos.temp) ){
 est.p[i, ] <- as.matrix(covar.temp[i,]) %*% as.matrix(t(coef.pred))
}

my.logit <- function(p){(1 /(1+exp(-p)))}
foo <- data.frame(cbind(ICO=icos.temp, year=year, prob=my.logit(est.p) ))
foo

}