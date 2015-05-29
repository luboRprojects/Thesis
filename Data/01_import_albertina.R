import_albertina <- function(file){
 data.in <- read.table(file, sep=";", skip=1, header=TRUE)
 data.in <- data.in[ ,-ncol(data.in)] # Last column contains NA values only
 codeSheet <- read.table(file, sep=";", header=TRUE)[1, ]
 colnames(data.in) <- colnames(codeSheet) <- unlist(strsplit(readLines(file, 1),";"))
 data.alb <-list(data=data.in, codeSheet=t(codeSheet) )
}

