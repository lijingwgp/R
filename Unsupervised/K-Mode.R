##############
### K-mode ###
##############

# a first look of the k-mode results
wss <- vector()
for (i in 2:15) {
  wss[i] <- sum(kmodes(sample_1, i, iter.max = 50, weighted = FALSE, fast=T)$withindiff)
}
ggplot(data = data.frame(wss), aes(x=seq(1:15), y=wss)) + geom_point()+ geom_line()+ 
  ggtitle("K-mode clustering") + labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") + 
  theme(plot.title = element_text(hjust = 0.5))

# comparing to clustering methods (balance, purity)
table(sample_1$STAGENAME, try1$cluster)
table(sample_1$STAGENAME, clust_num)

sum(apply(table(sample_1$STAGENAME,try1$cluster), 2, max))/nrow(sample_1)
sum(apply(table(sample_1$STAGENAME, clust_num), 2, max))/nrow(sample_1)

# see if any purity improvements over time
purity <- vector()
for(i in 5:20){
  temp <- kmodes(sample_1, i, iter.max = 50, weighted = FALSE, fast=T)
  purity[i] <- sum(apply(table(sample_1$STAGENAME,temp$cluster), 2, max))/nrow(sample_1)
}
ggplot(data = data.frame(purity), aes(x=seq(1:20), y=purity)) + geom_point()+ geom_line()+ 
  ggtitle("K-mode clustering") + labs(x = "Num.of clusters", y = "Clustering Purity)") + 
  theme(plot.title = element_text(hjust = 0.5))
