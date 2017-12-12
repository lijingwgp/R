set.seed(1);
x = matrix(NA,nrow=200,ncol=2);
x[,1]= rbind(rep(1,100),rep(2,100))
x[,2]= rbind(rnorm(100,23,20),rnorm(100,7,20));
x=data.frame(x); names(x) = c("group","value");
groupmeans = as.matrix(by(x$value,x$group,mean));
x.aov = summary(aov(x$value ~ as.factor(x$group)))

nlen = 50
withinvar = 470; ##From MSE in ANOVA
raw.pwr = matrix(NA, nrow=nlen, ncol=5)
for(i in 1:nlen){
  pwr.i10 = power.anova.test(groups=length(groupmeans), n=1+i,
                             between.var=var(groupmeans), within.var=withinvar, sig.level=.10)
  pwr.i05 = power.anova.test(groups=length(groupmeans), n=1+i,
                             between.var=var(groupmeans), within.var=withinvar, sig.level=.05)
  pwr.i01 = power.anova.test(groups=length(groupmeans), n=1+i,
                             between.var=var(groupmeans), within.var=withinvar, sig.level=.01)
  power10 = pwr.i10$power
  power05 = pwr.i05$power
  power01 = pwr.i01$power
  raw.pwr[i,1] = power10
  raw.pwr[i,2] = power05
  raw.pwr[i,3] = power01
  raw.pwr[i,5] = 1+i
}

plot(raw.pwr[,5], raw.pwr[,1], type="n", ylim=c(0,1),
     ylab="Power",xlab="Replicates (sample size per group)",
     main=expression(paste("Power Analysis: 2 Tx Groups for ", alpha, "=.10, .05, .01"))
     , sub=paste("Within var=",withinvar,"; Between var=", round(var(groupmeans)),2), cex.sub=.75
)
lines(raw.pwr[,5], raw.pwr[,1], type="l", lty=1, lwd=2, col="blue")
lines(raw.pwr[,5], raw.pwr[,2], type="l", lty=2, lwd=2, col="red")
lines(raw.pwr[,5], raw.pwr[,3], type="l", lty=3, lwd=2, col="green")
abline(h=seq(.1:.9, by=.1), col="lightgrey", lty=2); abline(h=.9, col=1, lty=1);
abline(v=c(10,20,30,40), lty=2, col="lightgrey")
abline(v=raw.pwr[,5][round(raw.pwr[,1],2)==.90][1]); text(raw.pwr[,5][round(raw.pwr[,1],2)==.90][1]-2.5, 0, paste("n: ",raw.pwr[,5][round(raw.pwr[,1],2)==.90][1]));
legend("bottomright", c(".10",".05",".01"), lty=c(1,2,3), col=c("blue","red","green"), lwd=2, title=expression(paste(alpha, " levels")))
