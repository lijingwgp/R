x <- seq(-4, 4, length=1000)
hx <- dnorm(x, mean=0, sd=1)

plot(x, hx, type="n", xlim=c(-4, 8), ylim=c(0, 0.5), 
     ylab = "",
     xlab = "",
     main= expression(paste("Type II (", beta, ") error")), axes=FALSE)
axis(1, at = c(-qnorm(.025), 0, -4), 
     labels = expression("p-value", 0, -infinity ))

shift = qnorm(1-0.025, mean=0, sd=1)*1.7
xfit2 <- x + shift
yfit2 <- dnorm(xfit2, mean=shift, sd=1)

# Print null hypothesis area
col_null = "#DDDDDD"
polygon(c(min(x), x,max(x)), c(0,hx,0), col=col_null)
lines(x, hx, lwd=2)

# The alternative hypothesis area

## The red - underpowered area
lb <- min(xfit2)
ub <- round(qnorm(.975),2)
col1 = "#CC2222"

i <- xfit2 >= lb & xfit2 <= ub
polygon(c(lb,xfit2[i],ub), c(0,yfit2[i],0), col=col1)

## The green area where the power is
col2 = "#22CC22"
i <- xfit2 >= ub
polygon(c(ub,xfit2[i],max(xfit2)), c(0,yfit2[i],0), col=col2)

# Outline the alternative hypothesis
lines(xfit2, yfit2, lwd=2)

axis(1, at = (c(ub, max(xfit2))), labels=c("", expression(infinity)), 
     col=col2, lwd=1, lwd.tick=FALSE)


legend("topright", inset=.05, title="Color",
       c("Null hypoteses","Type II error", "True"), fill=c(col_null, col1, col2), horiz=FALSE)

abline(v=ub, lwd=2, col="#000088", lty="dashed")

arrows(ub, 0.45, ub+1, 0.45, lwd=3, col="#008800")
arrows(ub, 0.45, ub-1, 0.45, lwd=3, col="#880000")