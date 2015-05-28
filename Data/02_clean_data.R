clean_survival <- function(file1){
library(dplyr)
library(lubridate)
# setwd("C:/Users/user/Disk Google/Disertace/Analysis/Survival/Data")
# source("import_albertina.R")

# setwd("C:/Users/user/Disk Google/Disertace/ToDo/Dupont")
# data.in <- read.table("dupont_data.csv", sep=";", header=TRUE)
#=====================

data.in <- file1 #import_albertina("data_surv2.csv") # data.in <- data01
danove.sazby <- read.table("danove_sazby.txt", sep="\t", header=TRUE)
#data.in <- data.in %>% select(-X)

#=========	Cleaning ===============
#---- Date.problems ----
data1.1 <- data.in  %>% filter(POC_MESICU==12) %>% select(-POC_MESICU)

data1.1$date <- ymd(data1.1$DAT_OD)
 data1.1 <- data1.1 %>% select(-DAT_OD)
data1.1$year <- year(data1.1$date)
data1.2 <- merge(x=data1.1, y=danove.sazby, by.x="year", by.y="rok") 
data1.3 <- data1.2 %>% filter(month(date)<3) # Tolerance of one month delay

#======== This has to be here because of the export to "ebit.data" -> "multiples_data_surv.txt" ===
#---- Remove missing values according to Assets ---
data1.4 <- data1.3 %>% filter(!is.na(AKTIVACELK)) %>% data.frame()
#------  Exclude companies with no sales ---
data1.4$sales  <- with(data1.4, ifelse(is.na(TZPZ), 0, TZPZ) + 
 ifelse(is.na(TZPVVAS), 0, TZPVVAS) )
data1.4 <- data1.4[-which(data1.4$sales==0), ]
#==========

#----
data2.1 <- data1.4
# data2.2 <- data2.1 %>% select(-NA) # table(is.na(data2.1$VAPZ))
# data2.3 <- data2.2 %>% select(-HVPREDZD) # table(is.na(data2.2$HVPREDZD))
data2.1$pretax.income <- with(data2.1, ifelse(HOSPVZUO<0, HOSPVZUO, HOSPVZUO*(1-dan.sazba)) )
data2.3 <- data2.1

#-- Financial Expanses
set.seed(123)
# interest rate as a random variable ~N(0.05, 0.01)
data2.3$financial.expanses <- with(data2.3, ifelse(is.na(NU), rnorm(1, 0.05, 0.01)*(AKTIVACELK-VLASTNIJM), NU ) )

data2.3$ebit <- with(data2.3, pretax.income + data2.3$financial.expanses )
data2.3$ebitda <- data2.3$ebit + ifelse(is.na(data2.3$OHANIM), 0, data2.3$OHANIM) 

data2.3$trzby <- with(data2.3, ifelse(is.na(TZPVVAS), TZPZ, TZPVVAS) )
data2.3$obchodni.firma <- with(data2.3, ifelse(TZPZ>TZPVVAS | is.na(TZPVVAS), 1, 0))
 data2.3$obchodni.firma <- with(data2.3, ifelse(is.na(obchodni.firma), 0, obchodni.firma))

data2.3 <- data2.3[-which(duplicated(data2.3)), ]
return(data2.3)
 }
#=================

# write.table(x=data2.3, file="data_survival.txt", sep=";", row.names=FALSE)

#=================