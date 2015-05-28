# ========= Demonstrate HPD regions ==========
source("compute_hpd_data.R")
# head(data, 10)
#d01 <- data.hpd(data, alpha=0.01)
# head(d01$data, 10)
# d01$limit
#d05 <- data.hpd(data, alpha=0.05)
# head(d05$data, 10)
# d05$limit
d10 <- data.hpd(data, alpha=0.1)
 head(d10$data, 10)
 d10$limit


data <- data.frame(acid, debt.ratio, returns)

ret.data <- data.frame(returns = data$returns)
ret.data$type <- ifelse(ret.data$returns < d10$limit[4,1] | ret.data$returns > d10$limit[4,2], "Out", "Normal")

library(ggplot2)
l0 <- ggplot(ret.data, aes(x=returns) )
l0 + scale_x_continuous(limits=emp.hpd(na.omit(returns))+c(-0.5,0.5))+ 
geom_histogram(binwidth=0.01, aes(fill=type))+ 
theme_bw()