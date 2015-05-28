data.hpd <- function(dframe, alpha=0.05) {
 library(TeachingDemos)
 bounds <- t(apply(dframe, 2, function(x){ emp.hpd(na.omit(x), conf=1-alpha)}))

 data.win <- dframe
for (i in 1:ncol(data.win)){
 data.win[, i] <- ifelse( (data.win[ ,i] < bounds[i,1]), bounds[i,1], data.win[ ,i])
 data.win[, i] <- ifelse( (data.win[ ,i] > bounds[i,2]), bounds[i,2], data.win[ ,i])
}
 foo <- list(limits=bounds, data=data.win)
}