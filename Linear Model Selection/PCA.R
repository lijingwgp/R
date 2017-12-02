#PRINCIPAL COMPONENT ANALYSIS
rm(list=ls())

#We're using the USArrests dataset that comes with R

#VISUALIZE THE DATA SET WE'RE USING
states=row.names(USArrests)
states
#name of each variable
names(USArrests)
#mean of each column
apply(USArrests, 2, mean)
#variance of each column
#We have high variance in the Assault variable
apply(USArrests, 2, var)
#This means we should scale the data set to analyze the PCs
#correctly, otherwise Assault will have too much influence

#USING PRCOMP FOR PRINCIPAL COMPONENT ANALYSIS 
#Remember to set "scale" = T if the data isn't already scaled
pr.out=prcomp(USArrests, scale=TRUE)
summary(pr.out)

#The names of outputs (the $ parts that you can call)
names(pr.out)

#We can also see the mean of each column using the $center call 
#(same as line 23)
pr.out$center

#We can see by how much each variable was scaled
pr.out$scale

#If we call dim(myprcomp$x) we can tell we have 50 observations 
#with 4 principal components
dim(pr.out$x)

#The rotation matrix ($rotation) provides the phi weights for each PC,
#Each column of pr.out$rotation contains the corresponding 
#principal component vector
pr.out$rotation

#Now let's build a scree plot to see how much variance is explained
#If you just use "plot(pr.out)" you won't get the scree plot you want
#You'll actually get a barplot of each principal component's 
#standard deviation squared (total data set variance)
#Let's try it
plot(pr.out)

#Therefore, we should build a scree plot ourselves
#First, if you call myprcomp$summary it actually tells you how 
#much variance each principal component explains 
#(the "proportion of variance" line)
summary(pr.out)

#Or we can calculate this by hand
#We know that the proportion of variance explained is a principal 
#component's variance/total variance
#We have the standard deviation of each principal component, so 
#we can then square it and use that to calculate proportion 
#of variance explained
pr.out$sdev
#Therefore, we square the standard deviation to get variance
pr.var=pr.out$sdev^2
pr.var
#Then divide by the sum of variances to get the proportion explained
pve = pr.var/sum(pr.var)
pve

#Now we can actually build the scree plot of proportion of variance 
#explained. We'll also build the cumulative plot
par(mfrow=c(1,2))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", 
     ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", 
     ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')

#Another way to compute PVE is from using the summary(pr.out) values
dev.off()
par(mfrow=c(1,2))
#First, access the proportion of variance explained
summary(pr.out)$importance[2,]
plot(summary(pr.out)$importance[2,], type = "b", ylab = "Proportion of Variance Explained",
     xlab = "Principal Component")
#And now get the cumulative sum values; there are 2 ways to do this
cumsum(pve)
#Or
summary(pr.out)$importance[3,]
plot(summary(pr.out)$importance[3,], type = "b", ylab = "Cumulative Proportion Explained",
     xlab = "Principal Component")
dev.off()

#Plot the first two principal component scores for each observation
#This allows us to see a low-dimensional view of the data
#Scale=0 ensures that the arrows are scaled to represent the phi's
biplot(pr.out, scale=0)

#The previous graph is the exact same but it's harder to read,
#so we rotated it
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

#Interpreting this kinda strange plot:
#The state names are plotted based on their z_ii scores for the 
#first 2 principal components, PC1 and PC2
#The red arrows are the phi's of the four variables for the 
#first 2 principal components
#ex: The word "Murder" is plotted at PC1 phi 0.534 and PC2 phi -0.418, 
#which are the same as $rotation

#We can see that Rape, Assault, and Murder are located in a similar space but Urban Population is far away
#This means that Rape, Assault, and Murder are relatively correlated 
#with each other but Urban Population is less correlated with the other 3
