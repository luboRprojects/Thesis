pred_quantiles <- function(icos){
 library(reshape2)

data.summary <- data.frame(
  rbind(p08, p09, p10, p11, p12) )

data.summary %>% filter(ICO %in% icos) %>% 
 melt(id.vars=c("ICO","year")) %>% 
group_by(ICO, year) %>% select(-variable) %>% 
 summarise(
  q01 = quantile(value, probs=0.01),
  q05 = quantile(value, probs=0.05),
  q25 = quantile(value, probs=0.25),
  q50 = quantile(value, probs=0.5),
  q75 = quantile(value, probs=0.75),
  q95 = quantile(value, probs=0.95),
  q99 = quantile(value, probs=0.99)
 ) %>% round(3)


}