my.means = c(38,50);
my.sd = c(10,10);

X = seq(my.means[1]-my.sd[1]*5,my.means[1]+my.sd[1]*5, length=100);
dX = dnorm(X, mean=my.means[1], sd=my.sd[1]); #True distribution
dX = dX/max(dX);

X.2 = seq(my.means[2]-my.sd[2]*5,my.means[2]+my.sd[2]*5, length=100);
dX.2 = dnorm(X.2, mean=my.means[2], sd=my.sd[2]); #Sampled distribution
dX.2 = dX.2/max(dX.2);

plot(X, dX, type="l", lty=1, lwd=2, xlab="x value",
     ylab="Density", main="Comparison of Type II error probability",
     xlim=c(min(X,X.2),max(X,X.2)))
lines(X.2,dX.2, lty=2)
abline(v=my.means, lty=c(1,2));
legend("topleft", c("Sample","Type II Error"), lty=c(2,1), col=c("black","black"), lwd=2, title=expression(paste("Type II Error")))

x = (my.means[1]-my.means[2])/my.sd[2];
1-pnorm(x);