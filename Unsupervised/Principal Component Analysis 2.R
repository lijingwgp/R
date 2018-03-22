# In this lab, we perform PCA on the USArrests data set, which is part of
# the base R package. The rows of the data set contain the 50 states, in
# alphabetical order.
rm(list=ls())
par(mfrow=c(1,1))
states=row.names(USArrests)
states
# The columns of the data set contain the four variables.
names(USArrests)
# We first briefly examine the data. We notice that the variables have vastly
# different means.
apply(USArrests, 2, mean)
# We see that there are on average three times as many rapes as murders, and 
# more than eight times as many assaults as rapes.
# We next examine the variances of the four variables
apply(USArrests, 2, var)
# Not surprisingly, the variables also have vastly different variances: the
# UrbanPop variable measures the percentage of the population in each state
# living in an urban area, which is not a comparable number to the number
# of rapes in each state per 100,000 individuals. If we failed to scale the
# variables before performing PCA, then most of the principal components
# that we observed would be driven by the Assault variable, since it has by
# far the largest mean and variance. Thus, it is important to standardize the
# variables to have mean zero and standard deviation one before performing
# PCA.
#
# We now perform principal components analysis using the prcomp() function, which 
# is one of several functions in R that perform PCA.
pr.out=prcomp(USArrests, scale=TRUE)
# By default, the prcomp() function centers the variables to have mean zero.
# By using the option scale=TRUE, we scale the variables to have standard
# deviation one. The output from prcomp() contains a number of useful quantities.
names(pr.out)
# The center and scale components correspond to the means and standard
# deviations of the variables that were used for scaling prior to implementing
# PCA.
pr.out$center
pr.out$scale^2
# The rotation matrix provides the principal component loadings; each column
# of pr.out$rotation contains the corresponding principal component
# loading vector. This function names it the rotation matrix, because when we 
# matrix-multiply the X matrix by pr.out$rotation, it gives us the coordinates 
# of the data in the rotated coordinate system. These coordinates are the principal 
# component scores.
pr.out$rotation
# We see that there are four distinct principal components. This is to be
# expected because there are in general min(n − 1, p) informative principal
# components in a data set with n observations and p variables.
#
# Using the prcomp() function, we do not need to explicitly multiply the
# data by the principal component loading vectors in order to obtain the
# principal component score vectors. Rather the 50 × 4 matrix x has as its
# columns the principal component score vectors. That is, the kth column is
# the kth principal component score vector.
dim(pr.out$x)
# We can plot the first two principal components as follows:
biplot(pr.out, scale=0)
# The scale=0 argument to biplot() ensures that the arrows are scaled to
# represent the loadings; other values for scale give slightly different biplots
# with different interpretations.
#
# Notice that this figure is a mirror image the text's. Recall that
# the principal components are only unique up to a sign change, so we can
# reproduce the text's figure by making a few small changes:
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
# The prcomp() function also outputs the standard deviation of each principal
# component. For instance, on the USArrests data set, we can access
# these standard deviations as follows:
pr.out$sdev
# The variance explained by each principal component is obtained by squaring
# these:
pr.var=pr.out$sdev^2
pr.var
# To compute the proportion of variance explained by each principal component,
# we simply divide the variance explained by each principal component
# by the total variance explained by all four principal components:
pve=pr.var/sum(pr.var)
pve
# We see that the first principal component explains 62.0% of the variance
# in the data, the next principal component explains 24.7% of the variance,
# and so forth. We can plot the PVE explained by each component, as well
# as the cumulative PVE, as follows:
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
# The result is the one shown in the text. 
#
# Note that the function cumsum() 
# computes the cumulative sum of the elements of a numeric vector.
