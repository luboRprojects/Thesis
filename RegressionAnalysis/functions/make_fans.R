make.fans <- function(ico){
 par(mfrow=c(1,2), mar=c(4,5,2,3))
 show.ico <- ico[1]
 data.fan <- data.frame(
  rbind( p08[p08$ICO==show.ico, ], p09[p09$ICO==show.ico, ], 
  p10[p10$ICO==show.ico, ], p11[p11$ICO==show.ico, ], p12[p12$ICO==show.ico, ])) %>% 
  select(-ICO, -year)

fan0(data.fan, frequency=1, xaxt = "n", ylab="Probability of being in 'def' class",
  main=paste("Predicted probabilities for company", show.ico), xlab="" )
axis(1, at = 1:5, labels = 2008:2012)

 show.ico <- ico[2]
 data.fan <- data.frame(
  rbind( p08[p08$ICO==show.ico, ], p09[p09$ICO==show.ico, ], 
  p10[p10$ICO==show.ico, ], p11[p11$ICO==show.ico, ], p12[p12$ICO==show.ico, ])) %>% 
  select(-ICO, -year)

fan0(data.fan, frequency=1, xaxt = "n", ylab="", xlab="",  main=paste("Probabilities for company", show.ico) )
axis(1, at = 1:5, labels = 2008:2012)
 par(mfrow=c(1,1))
}