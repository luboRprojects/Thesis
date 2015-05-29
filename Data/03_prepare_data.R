#------	Load data	----------
prepare_data <- function(file2){

data0 <- file2 #data0 <- data02
data1.1 <- data0

#---- Buggy duplications identification ---
#data0.2 <- anyDuplicated(data0.1)
#data0.2 <- ifelse(dupl.indicator==TRUE, 1, data0.1)
#data0.1[-which(duplicated(data0.1)), ]
#data0.1 <- ifelse(length(which(duplicated(data0[ ,c("year", "ICO")])))==0, data0, data0[-which(duplicated(data0[ ,c("year", "ICO")])), ])
#ifelse(length(duplicated(data0[ ,c("year", "ICO")]))>0, data0[-which(duplicated(data0[ ,c("year", "ICO")])), ], data0)
dupl.indicator <- as.logical(anyDuplicated(data1.1))
cat("Are there any duplicated rows?  ", dupl.indicator)

#------	Variables	----------
#-- Net Worth 
data1.1$net.worth <- data1.1$VLASTNIJM

#-- EBITDA approximated by ordinary income + depreciation
data1.1$ebitda <- data1.1$PROVHOSPV+data1.1$OHANIM

#-- Total Debt
data1.1$total.debt <- data1.1$AKTIVACELK - data1.1$VLASTNIJM
data1.1

#------	Remove Company With Inappropriate Records	----------
# 1) companies with available data
data2.1 <- data1.1 %>% 
 select(ICO, year, ebitda, financial.expanses, net.worth, total.debt) %>% 
 group_by(ICO, year) %>% arrange(year)

data2.2 <- data2.1[complete.cases(data2.1), ]

# temp0 indicates first observation of the company
data2.2$temp0 <- ifelse(c(1, diff(data2.2$ICO))>0, 0, 1)
data2.2$conti <- ifelse(c(1, diff(data2.2$year))==1, 1, 0)
data2.2$use <- ifelse(
 (data2.2$temp0==0) | (data2.2$temp0==0 & data2.2$conti==1),
 1,data2.2$conti)

#head(data2.2, 20)

# We need to check that companies report all
# consecutive data
# 101 indicates missing value (0=missing)
companies <- data2.2 %>% 
 group_by(ICO) %>% 
 summarise(
  id = paste0(as.character(use), collapse=""),
  remove = grepl(x=id, pattern="101")
 ) %>%
 filter(remove==FALSE) %>% 
 filter(nchar(id)>2) %>% data.frame()


# companies with available data
data2 <- data2.2[which(data2.2$ICO %in% companies$ICO), ]
#head(data2, 20)

#----
data3.1 <- data2
# Some companies have use=0. This would be a problem if the 
# problematic year would be anywhere but on the bottom* of the
# analysed period. Let's check whether it's the case:

use0 <- as.numeric(unlist(data3.1[which(data3.1$use==0), "ICO"]))

data3.1 %>%
 filter(ICO %in% use0)%>%
 group_by(ICO) %>%
 mutate(
  use.inv = ifelse(use==1, 0, 1),
  year.problem = use.inv * year) %>%
 summarise(
  last = max(year),
  problem = max(year.problem),
  last.observation = last==problem
 )

# If the last column in contains TRUE value then 
# removing observation will solve the problem.
# If there is FALSE -> lookup the company and impute/remove

data3.2 <- data3.1
data3.2$id <- rownames(data3.1)

remove.id <- as.numeric((data3.2[which(data3.2$use==0), c("ICO", "year", "id")])$id)
data3.3 <- data3.2[-remove.id, ] %>% select(-id)

data3 <- data3.3 %>% select(-temp0, -conti, -use)
#------	DISTRESS ASSIGNMENT	----------
# 1) Rule 1 = EBITDA < Financial Expanses in two consecutive years

data4.1 <- data3 %>% group_by(ICO, year)

data4.1$r1t1 <- as.numeric(data4.1$ebitda < data4.1$financial.expanses)
data4.1$r1t2 <- c(data4.1$r1t1[-1], 0)
data4.1$first <- as.numeric(!duplicated(data4.1$ICO))
data4.1$last <- c(data4.1$first[-1], 1)
data4.1$r1t3 <- ifelse( (data4.1$r1t1+data4.1$r1t2 == 2), 1, 0)
data4.1$rule1 <- ifelse( (data4.1$r1t1+data4.1$r1t2 == 2) & (data4.1$last==1), 0, data4.1$r1t3)

data4 <- data4.1 %>% select(-r1t1, -r1t2, -r1t3)

# data4.1 <- data3
# data4.1$negative.val <- with(data4.1, ifelse(ebitda<financial.expanses , 1, 0) )
# data4.1$lagged.aux <- lag(data4.1$negative.val)
#  data4.1$lagged.aux[1] <- 0

# data4.1$rule2.aux <- as.numeric(with(data4.1, negative.val==1 & lagged.aux==1 & c(ICO[1], diff(ICO))==0, 1, 0))
# data4.1$rule1 <- c(lead(data4.1$rule2.aux)) # shift one month up
# data4.1$rule1[nrow(data4.1)] <- data4.1$negative.val[nrow(data4.1)] # last observation

# data.frame(data4.1) %>% select(ICO, year, ebitda, financial.expanses, rule1)

# data4.2 <- data4.1
# data4.2$id <- as.numeric(rownames(data4.1))
#  correct <- data4.2[which(!duplicated(data4.2$ICO) ), ]$id -1
# data4.2$rule1[correct] <- with(data4.2[correct, ], ifelse(ebitda<financial.expanses, 1, 0) )

# data4 <- data4.2 %>% select(-negative.val, -lagged.aux, -rule2.aux, -id)

# 2) Rule 2 = net worth < total debt
data5.1 <- data4
data5.1$rule2 <- with(data5.1, ifelse(net.worth<total.debt, 1, 0))
data5 <- data5.1
# 3) Rule 3 = net worth decreased
data6.1 <- data5 
data6.1$rule3.temp <- 1
data6.1$rule3.temp[which(!duplicated(data6.1$ICO))] <- 0
data6.1$rule3 <- ifelse(c(0, diff(data6.1$net.worth)) * data6.1$rule3.temp >=0, 0, 1)

# Combine rules
data6.2 <- data6.1 %>% select(-rule3.temp)
data6.2$def <- with(data6.2, ifelse(rule1 + rule2 + rule3 ==3, 1, 0))

#=======================================
data6 <- data6.2 %>% 
 select(-ebitda, -financial.expanses, -net.worth, -total.debt)
# write.table(x=data6, file="surv_def.txt", sep=";", row.names=FALSE)
# data6 %>% select(ICO, year) %>% write.table(file="icos.txt", sep=";", row.names=FALSE)

#---- Class==1 : companies with higher than mode ebitda/
median.all <- data6.2 %>% group_by(ICO) %>% summarise(med = quantile(ebitda/net.worth, probs=0.5)) %>% 
 select(med) %>% summarise(median = quantile(med, probs=0.5)) %>% as.numeric()

add.class <- data6.2 %>% group_by(ICO) %>% 
 summarise(
  med = quantile(ebitda/net.worth, probs=0.5),
  class = ifelse(med > median.all, 1, 0)
 )
 data7.1 <- merge(data6, add.class, by.x="ICO", by.y="ICO") %>% select(-med)
 data7.1
}

# data7.1 <- merge(data6, add.class, by.x="ICO", by.y="ICO") %>% select(-med)
# data7.1 %>% write.table(file="data_cox.txt", sep=";", row.names=FALSE)
