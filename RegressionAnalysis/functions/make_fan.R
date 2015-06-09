make.fan <- function(ico){

data.fan <- data.frame(
 rbind( p08[p08$ICO==show.ico, ], p09[p09$ICO==show.ico, ],
 p10[p10$ICO==show.ico, ], p11[p11$ICO==show.ico, ], p12[p12$ICO==show.ico, ]
 )
) %>% select(-ICO, -year)

fan0(data.fan, frequency=1, xaxt = "n", main=paste("Development of company", show.ico), ylab="Probability of being Class 'def' member")
axis(1, at = 1:5, labels = 2008:2012)
fan(data.fan)

}