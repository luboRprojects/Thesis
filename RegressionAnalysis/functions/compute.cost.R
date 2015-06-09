compute.cost <- function(def, predicted, prec=0.005){
 thresholds <- seq(from=0.01, to=max(predicted), by=prec)
 results <- data.frame(thresholds, Q=0)
 

 for (i in seq(length(thresholds)) ) {
  code <- data.frame(code=paste0(ifelse(predicted>=thresholds[i], 1, 0), def))
  results[i,"Q"] <- c(sum(merge(cost.matrix, code, by.x="event", by.y="code")[ ,2]))
 }

 th.val <- results[which(results$Q == max(results$Q)), "thresholds"][1]

 plot <- results %>% ggplot(aes(x=thresholds, y=Q)) + 
 geom_line(colour="darkblue", size=0.6) + 
 geom_hline(yintercept=max(results$Q), col="green")+ 
 scale_y_continuous("Cost function Q") + 
 scale_x_continuous(breaks=round(sort(c(seq(from=0, to=1, by=0.2),th.val) ),2))+
 theme_bw()

 foo <- list(figure=plot, solution=results[which(results$Q == max(results$Q)), ])
 return(foo)
}