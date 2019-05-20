########################
### Data Preparation ###
########################

# the data will be sterile clean in order to not get distracted with other 
# issues that might arise, but I will also write about some difficulties I had, 
# outside the code

library(dplyr)

# ensuring reproducibility for sampling
set.seed(40)

# generating random variable set
# specifying ordered factors, strings will be converted to factors when using
# data.frame(). customer ids come first, we will generate 200 customer ids from 1
# to 200

id <- c(1:200) %>% factor()
budget <- sample(c('small','med','large'),200,replace = T) %>% factor(levels = c('small','med','large'),                                                                      ordered = TRUE)
origins <- sample(c("x", "y", "z"), 200, replace = T, 
                    prob = c(0.7, 0.15, 0.15))
areas <- sample(c('area1','area2','area3','area4'),200,replace = T,
                  prob = c(.3,.1,.5,.2))
sources <- sample(c("facebook", "email", "link", "app"),200,replace = T,
                  prob = c(0.1,0.2, 0.3, 0.4))
dows <- sample(c("mon", "tue", "wed", "thu", "fri", "sat", "sun"), 200, replace = T,
               prob = c(0.1, 0.1, 0.2, 0.2, 0.1, 0.1, 0.2)) %>%
  factor(levels=c("mon", "tue", "wed", "thu", "fri", "sat", "sun"), ordered = TRUE)
dish <- sample(c("delicious", "the one you don't like", "pizza"), 200, replace = T)

# by default, data.frame() will convert all the strings to factors
customers <- data.frame(id, budget, origins, areas, sources, dows, dish)



############################
### Dissimilarity Matrix ###
############################

library(cluster)
gower_dist <- daisy(customers[,-1], metric = c('gower'))



###############################
### Hierarchical clustering ###
###############################

# divisive clustering
divisive_clust <- diana(as.matrix(gower_dist), diss = T, keep.diss = T)
plot(divisive_clust, main = 'Divisive')

# agglomerative clustering
aggl_clust <- hclust(gower_dist, method = 'complete')
plot(aggl_clust, main = 'Agglomerative, complete linkages')



##########################
### Assessing Clusters ###
##########################

# cluster results come out as a table
library(fpc)

cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
    }
  }
  
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}

stats_df_divsive <- cstats.table(gower_dist, divisive_clust, 7)
stats_df_divsive

stats_df_agglo <- cstats.table(gower_dist, aggl_clust, 7)
stats_df_agglo

# using 'Elbow' and 'Silhouette' methods to identify the est number of clusters
library(ggplot2)

# Elbow
# divisive clustering
ggplot(data = data.frame(t(cstats.table(gower_dist, divisive_clust, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

# agglomerative clustering
ggplot(data = data.frame(t(cstats.table(gower_dist, aggl_clust, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

# Silhouette
# divisive
ggplot(data = data.frame(t(cstats.table(gower_dist, divisive_clust, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))

# agglomerative
ggplot(data = data.frame(t(cstats.table(gower_dist, aggl_clust, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))



#####################
### Visualization ###
#####################

library(ggplot2)
library(reshape2)
library(purrr)
library(dplyr)
library(dendextend)

dendro <- as.dendrogram(aggl_clust)
aggl_dendro <- dendro %>% 
  set('branches_k_color', k=7, 
      value=c("darkslategray", "darkslategray4", "darkslategray3", "gold3", 
              "darkcyan", "cyan3", "gold3")) %>%
  set("branches_lwd", 0.6) %>%
  set('labels_colors', value=c('darkslategray')) %>%
  set('labels_cex', .5)
ggd1 <- as.ggdend(aggl_dendro)
ggplot(ggd1, theme = theme_minimal()) + labs(x='Num.observations',y='Height',
                                             title='Dendrogram,k=7')
# radial plot
ggplot(ggd1, labels = T) + 
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")

# heatmap to visualize data distribution across the categorical variables
clust_num <- cutree(aggl_clust, k = 7)
customers_final <- cbind(customers, clust_num)
customer_long <- melt(data.frame(lapply(customers_final, as.character), stringsAsFactors = F),
     id = c('id','clust_num'), factorAsStrings=T)

customer_long_q <- customer_long %>% 
  group_by(clust_num, variable, value) %>%
  mutate(count = n_distinct(id)) %>%
  distinct(clust_num, variable, value, count)
  
heatmap_cust <- ggplot(customer_long_q, aes(x = clust_num, y = factor(value, levels = c("x","y","z", "mon", "tue", "wed", "thu", "fri","sat","sun", "delicious", "the one you don't like", "pizza", "facebook", "email", "link", "app","area1", "area2", "area3", "area4", "small", "med", "large"), ordered = T))) +
  geom_tile(aes(fill = count)) + 
  scale_fill_gradient2(low = "darkslategray1", mid = "yellow", high = "turquoise4")

customer_long_p <- customer_long_q %>%
  group_by(clust_num, variable) %>%
  mutate(perc = count / sum(count)) %>%
  arrange(clust_num)

heatmap.p <- ggplot(customer_long_p, aes(x = clust_num, y = factor(value, levels = c("x","y","z",
                                                                                 "mon", "tue", "wed", "thu", "fri","sat", "sun","delicious", "the one you don't like", "pizza","facebook", "email", "link", "app","area1", "area2", "area3", "area4","small", "med", "large"), ordered = T))) +
  geom_tile(aes(fill = perc), alpha = .85) +
  labs(title = "Distribution of characteristics across clusters", x='Cluster number',
       y=NULL)+
  geom_hline(yintercept = 3.5) +
  geom_hline(yintercept = 10.5) +
  geom_hline(yintercept = 13.5) +
  geom_hline(yintercept = 17.5) +
  geom_hline(yintercept = 21.5) +
  scale_fill_gradient2(low = 'darkslategray1', mid = 'yellow', high = 'turquoise4')

heatmap.p

