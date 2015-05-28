# Compute Multiples
library(dplyr)
setwd("C:/Users/user/Disk Google/Disertace/Analysis/Survival/Data")
source("01_import_albertina.R")

data01 <- read.table("multiples_data_surv.txt", sep=";", header=TRUE)
data02 <- import_albertina("data_glmm_surv.csv")
data03 <-data02$data
 data03$year <- substr(data03$DAT_OD, 1, 4)

data <- merge(x=data01, y=data03, by.x=c("ICO","year","AKTIVACELK"), by.y=c("ICO","year","AKTIVACELK"))
#data[which(duplicated(data)), ]
data04.1 <- data %>% select(-DAT_OD, -DAT_DO, -TZPZ, -TZPVVAS)
data04 <- data04.1[-which(duplicated(data04.1)), ]

# Variables 
# Assume that NA means 0.

data04$Z		<- ifelse(is.na(data04$Z), 0, data04$Z)
data04$OBEZNAA	<- ifelse(is.na(data04$OBEZNAA), 0, data04$OBEZNAA)
data04$VLASTNIJM	<- ifelse(is.na(data04$VLASTNIJM), 0, data04$VLASTNIJM)
data04$HVML 	<- ifelse(is.na(data04$HVML), 0, data04$HVML)
data04$KZ 		<- ifelse(is.na(data04$KZ), 0, data04$KZ)
data04$KBU 		<- ifelse(is.na(data04$KBU), 0, data04$KBU)
data04$ZCPIMM	<- ifelse(is.na(data04$ZCPIMM), 0, data04$ZCPIMM)


#===== Compute multiples - mine =====
acid.test  <- with(data04, (OBEZNAA-Z)/(KZ+KBU) )
debt.ratio <- with(data04, (AKTIVACELK-VLASTNIJM)/AKTIVACELK)
asset.turn <- with(data04, (sales) / (AKTIVACELK) )
returns    <-  with(data04, (PROVHOSPV)/(AKTIVACELK) )

#======= Outliers and Missing Values ======
source("compute_hpd_data.R")
data.multiples <- data.frame(acid.test, debt.ratio, asset.turn, returns)
data.ident <- data04[ ,c("ICO", "year")]
data.win <- data.hpd(data.multiples)$data
 # round(data.hpd(data.multiples)$limits, 3)

data_clean <- data.frame(data.ident, data.win)

defaults <- data03_defaults[, c("ICO","year","def", "class")]

str(data03_defaults)

data.final <- merge(x=defaults, y=data_clean, by.x=c("ICO", "year"), by.y=c("ICO", "year") )

data.final %>% group_by(year,def) %>% summarise(count=n() )


summary(data.final)
# write.table(x=data.final, file="final_data.txt", sep=";", row.names=FALSE)





#========== OLD WAY ================
# acid.test
acid.test  <- ifelse(acid.test>5 | (data04$KZ+data04$KBU <0), 5, acid.test) #boxplot(acid.test)
data04[which(is.infinite(acid.test)), ]
#===== Compute multiples - Altman =====
x1 <- with(data04, (OBEZNAA-KZ+KBU)/(AKTIVACELK))
x2 <- with(data04, HVML/AKTIVACELK)
x3 <- with(data04, ebit/AKTIVACELK)
x4 <- with(data04, (sales) / (AKTIVACELK) )

#=====================================================
# Check for correspoding values ---
#---
str(data01) # from survival analysis 2620
str(data03) # 2871 from new Albertina query
str(data) # after join 2624
str(data04) # 2620

first <- data01 %>% group_by(ICO) %>% summarise(count = n())
last <- data04 %>% group_by(ICO) %>% summarise(count = n())

nrow(first);nrow(last)

check <- data.frame(cbind(first, last))
check$check <- with(check, count - count.1)


#=====================
#----------------

summary(data04)
table()

show <- t(head(sort(apply(data04, 1, function(x){sum(as.numeric(is.na(x)))}), 
 decreasing=TRUE ), 20))

data04[as.numeric(colnames(show)), ]

head(data04[which(is.na(data04$ebit)), ])



data04 %>% filter(ICO==24747629)
data01 %>% filter(ICO==25560590)

data01$ICO

#---Manually remove:
remove_ico <- c(24747629, )


