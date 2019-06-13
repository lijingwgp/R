library(dplyr)
library(ggplot2)
library(pwr)
library(knitr)
library(lsr)
setwd("C:/Users/jing.o.li/Desktop/SBC/SBC_19")
ship_his_m1 = read.csv('genex_his4.csv',header = TRUE, sep = ',')

# establish churn amount and churn binary variables
# both could be used in AB testing
colnames(ship_his_m1) <- toupper(names(ship_his_m1))
ship_his_m1 <- ship_his_m1 %>%
  filter(P1REVENUE >= 500) %>%
  mutate(CHURN_AMOUNT = P1REVENUE - P3REVENUE, 
         CHURN_BINARY = ifelse(CHURN_AMOUNT >= 500, 1, 0))
ship_his_m1 <- ship_his_m1[,c(213,214)]

#ship_his_m1 = read.csv('True Churn Amount.csv',header = TRUE, sep = ',')
#ship_his_m1 = ship_his_m1$Churn.Amount
#ship_his_m1 = data.frame(ship_his_m1)
#colnames(ship_his_m1) <- 'churn'

# find outliers
find_outliers <- function(data_to_test){
  IQR<- quantile(data_to_test, 0.75, na.rm=TRUE)[[1]] - quantile(data_to_test, 0.25, na.rm=TRUE)[[1]]
  cutoff = subset(data_to_test, data_to_test <=  quantile(data_to_test, 0.25, na.rm=TRUE)-(IQR*1.5) )  
  cutoff2 = subset(data_to_test, data_to_test >=   quantile(data_to_test, 0.75, na.rm=TRUE)+(IQR*1.5) ) 
  ret = c(cutoff,cutoff2)
  return(ret)
}
pos <- which(ship_his_m1$CHURN_AMOUNT %in% find_outliers(ship_his_m1$CHURN_AMOUNT) )
ship_his_m2 <- data.frame(ship_his_m1[-pos,])
colnames(ship_his_m2) <- c('CHURN AMOUNT',"CHURN BINARY")
ggplot(ship_his_m2, aes(x = ship_his_m2$`CHURN AMOUNT`)) + geom_density(alpha=.3) +xlab("CHURN AMOUNT")

# ship_his_m3 <- data.frame(ship_his_m2[ship_his_m2$churn > 0, ])
# colnames(ship_his_m3) <- 'churn'
# summary(ship_his_m3)
# sd(ship_his_m3$churn)

# RCT_random = function(dataframey, values_to_add){
#    set.seed(111)
#    dataframey$values_to_add[sample(1:nrow(dataframey), nrow(dataframey), FALSE)] <- rep(values_to_add)
#    colnames(dataframey)[which(colnames(dataframey)=="values_to_add")] = "Status"
#    return(dataframey) }
# ship_his_m4 <- RCT_random(ship_his_m3, c("Treatment","Control"))

# set up MDE to be used in cohen's d
diffs <- seq(10,100,10)

# prioritize MDE
p <- c() 
for(i in seq(1:length(diffs) ) ) {
  samp1 <- ship_his_m2$`CHURN AMOUNT`
  xnu = diffs[[i]]
  samp2 <- samp1 - rnorm(length(samp1), xnu, xnu/10)
  #samp2 <- samp1 + rnorm(length(samp1), xnu, xnu/10)
  inp <- cohensD(samp1, samp2)
  p[i] <- pwr.2p.test(h=inp, sig.level=0.05, power=0.8, n=NULL)$n}
print(ggplot() + geom_point(aes(x=p, y= diffs), size=3, color="blue", shape=1) + geom_line(aes(x=p, y=diffs), size=1.2, color="blue") + xlab("Sample size")+ ylab("MDE") + ggtitle("Minimum detectable effect vs. sample size"))
mde_tab = data.frame("MDE"=diffs, "Sample size"=p)
kable(mde_tab, digits=2) 

# prioritize power
control <- ship_his_m2$`CHURN AMOUNT`
#treat_lower_estimate <- control *0.95
#treat_upper_estimate <- control *0.9
treat_upper_estimate <- control *1.05
treat_lower_estimate <- control *1.1
power_lw <- c() 
power_hi <- c()
sample_sizes <- seq(1000,40000,2000)
lower_cohen <- cohensD(control, treat_lower_estimate)
upper_cohen <- cohensD(control, treat_upper_estimate)
for(i in sample_sizes){
  a <- pwr.t.test(d = lower_cohen, n=i,  sig.level = 0.05, power = NULL)$power
  power_lw <- c(power_lw, a)
  b <- pwr.t.test(d = upper_cohen, n=i,  sig.level = 0.05, power = NULL)$power
  power_hi <- c(power_hi, b)}
marker <- pwr.t.test(d = lower_cohen, n=NULL,  sig.level = 0.05, power = 0.8)$n
marker2 <- pwr.t.test(d = upper_cohen, n=NULL,  sig.level = 0.05, power = 0.8)$n
ggplot() + geom_ribbon(aes(x=sample_sizes, ymin= power_lw, ymax=power_hi), 
                       alpha=0.2, colour="blue", fill="blue")  + 
  xlab("Sample size") + ylab("Power") + 
  geom_vline(xintercept = marker, linetype="dotted" ) + 
  geom_hline(yintercept=0.8, linetype="dotted", size=2) + 
  geom_vline(xintercept = marker2 , linetype="dotted") + 
  labs(title="Power curve example", caption="Power curve indicating sample sizes needed for a 0.8 power (dotted lines)") +
  theme(plot.caption = element_text(hjust = 0.5)) 
