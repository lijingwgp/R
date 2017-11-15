rm(list=ls())
#Set the parameters to particular values
B0=-4
B1=4
B2=6
#Create x-axis and y-axis values
x <- seq(-2, 3, .1)
y <- seq(-2, 2, .1)
#Populate a matric with the values of the logistic function (probabilities) 
#using the parameter and x, y values
z <- matrix(nrow=length(x), ncol=length(y))
for(i in 1:length(x)) {
	for(j in 1:length(y)) {
		z[i, j] <- exp(B0+B1*x[i]+B2*y[j])/(1+exp(B0+B1*x[i]+B2*y[j]))
	}
}
#Draw a perspective plot of the surface defined by the matrix
persp(x,  y,  z,  phi = 40,  theta = 40, 
  xlab = paste(min(x), "through", max(x)), ylab = paste(min(y), "through", max(y)), zlab=paste(round(max(z), 2), "through", round(min(z), 2)), 
  main = paste("Surface for The Logistic Function p(X) \n with B0=", B0, ",  B1=", B1, ",  and B2=", B2, sep=""))
