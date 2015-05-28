setwd("C:/Users/user/Disk Google/Disertace/Analysis/Survival/Data")

#=== Read data from Albertina ===
source("01_import_albertina.R")
data01_list <- import_albertina("data_surv2.csv")
data01 <- data01_list$data

#=== Compute Varibles ===
source("02_clean_data.R")
data02 <- clean_survival(data01)

#=== Check for consistency, cleaning, rule creation ===
source("03_prepare_data.R")
data03_defaults <- prepare_data(data02)

#=== Winsorise data with HPD estimation ===
source("data_hpd.R")
#--- Summaries ---
data03 %>% group_by(year, def) %>% summarise(count=n() )
table(c(data03$rule1 + data03$rule2 + data03$rule3 ), data03$def)

ebit.data <- data02 %>% select(year, ICO, AKTIVACELK, ebit, ebitda, financial.expanses, sales)
# write.table(x=ebit.data, file="multiples_data_surv.txt", sep=";", row.names=FALSE) 


