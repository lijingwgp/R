#############################################
#### Susan's Model for Fixed Start Date ##### 
#############################################
contributions = function(q){
  offersmade = q
  new_analyst = rbinom(1,offersmade, .7)
  retention = c(runif(1,.90,1),runif(4,.95,1),runif(1,.8,1),runif(3,.9,1),runif(1,.8,1),runif(3,.9,1),runif(2,.95,1))
  new_analyst1 = c(0,0,new_analyst,0,0,0,0,0,0,0,0,0,0,0,0)
  demand_hist = c(105,95,75,70,70,110,105,90,65,80,90,120,105,95,75)
  unexpected = rnorm(1, mean = 0, sd = 0.05)
  noise = rnorm(15, mean = 0, sd = 0.1)
  actual = c()
  for(i in 1:15){
    actual[i] = demand_hist[i]*(1+unexpected)*(1+noise[i])
  }
  analyst_next = c(63)
  for(i in 1:14){
    analyst_next[i+1] = analyst_next[i]*retention[i]+new_analyst1[i]
  }
  short = c()
  for(i in 1:15){
    if (analyst_next[i]-actual[i]<0){
      short[i]=-1*(analyst_next[i]-actual[i])
    }
    else
      short[i]=0
  }
  excess = c()
  for(i in 1:15){
    if(analyst_next[i]-actual[i]>0){
      excess[i] = analyst_next[i]-actual[i]
    }
    else
      excess[i] = 0
  }
  excess_cost=6000
  short_cost=3600
  base_contribution=4000
  earnings=c()
  for(i in 1:15){
    earnings[i] = base_contribution*actual[i]-(short[i]*short_cost+excess[i]*excess_cost)
  }
  sum(earnings)
}



###################################################
#### Optimum Offers Made for Fixed Start Date ##### 
###################################################
earnings_total = c()
bestq = c()
for (i in 10:110){
  etmp <- c()
  for (j in 1:100){
    etmp[j]<-contributions(i)
  }
  earnings_total[i-9] <- mean(etmp)
}
(bestq = which.max(earnings_total)+9)


## Shift earnings_total's index backwards so that it starts from 10
new_earnings_total = c()
for(i in 1:length(earnings_total)){
  new_earnings_total[i+9] = earnings_total[i]
}
par(mfrow=c(1,2))
index = seq(1,110,1)
plot(index, new_earnings_total, col=ifelse(index == bestq,"forestgreen","black"), pch = ifelse(index==bestq,19,1), xlab = 'Number of Offers', ylab = 'expected contribution to earnings',
     main = "Expected Earnings with Susan's Assumption")
plot(index, new_earnings_total, col=ifelse(index == 98,"red","black"), pch = ifelse(index==98,19,1),xlab = 'Number of Offers', ylab = 'expected contribution to earnings',
     main = "Expected Earnings with Tom's Assumption")
diff_earnings = new_earnings_total[bestq]-new_earnings_total[98]
max_fixed_earnings = new_earnings_total[bestq]


## Simulate the best q multiple times so that we could draw a distribution
best_bestq = c()
for (a in 1:10){
  for (i in 10:110){
    etmp <- c()
    for (j in 1:100){
      etmp[j]<-contributions(i)
    }
    earnings_total[i-9] <- mean(etmp)
  }
  bestq = which.max(earnings_total)+9
  best_bestq[a]=bestq
}
par(mfrow=c(1,1))
hist(best_bestq, breaks = 10, main = "Distribution of Optimal Offers made\nFixed Start Date", xlab = "Optimial Number of Offers",
     ylab = "Occurance")



################################################
#### Susan's Model for Flexible Start Date ##### 
################################################
flex = function(q){
  offersmade = q
  new_analyst = rbinom(1,offersmade, .7)
  new_analyst_sep = runif(1,.7,1)*(new_analyst/2)
  retention = c(runif(1,.90,1),runif(4,.95,1),runif(1,.8,1),runif(3,.9,1),runif(1,.8,1),runif(3,.9,1),runif(2,.95,1))
  new_analyst1 = c(0,0,new_analyst/2,0,new_analyst_sep,0,0,0,0,0,0,0,0,0,0)
  demand_hist = c(105,95,75,70,70,110,105,90,65,80,90,120,105,95,75)
  unexpected = rnorm(1, mean = 0, sd = 0.05)
  noise = rnorm(15, mean = 0, sd = 0.1)
  actual = c()
  for(i in 1:15){
    actual[i] = demand_hist[i]*(1+unexpected)*(1+noise[i])
  }
  analyst_next = c(63)
  for(i in 1:14){
    analyst_next[i+1] = analyst_next[i]*retention[i]+new_analyst1[i]
  }
  short = c()
  for(i in 1:15){
    if (analyst_next[i]-actual[i]<0){
      short[i]=-1*(analyst_next[i]-actual[i])
    }
    else
      short[i]=0
  }
  excess = c()
  for(i in 1:15){
    if(analyst_next[i]-actual[i]>0){
      excess[i] = analyst_next[i]-actual[i]
    }
    else
      excess[i] = 0
  }
  excess_cost=6000
  short_cost=3600
  base_contribution=4000
  earnings=c()
  for(i in 1:15){
    earnings[i] = base_contribution*actual[i]-(short[i]*short_cost+excess[i]*excess_cost)
  }
  sum(earnings)
}



######################################################
#### Optimum Offers Made for Flexible Start Date ##### 
######################################################
earnings_total = c()
bestq = c()
for (i in 10:110){
  etmp <- c()
  for (j in 1:100){
    etmp[j]<-flex(i)
  }
  earnings_total[i-9] <- mean(etmp)
}
(bestq = which.max(earnings_total)+9)


new_earnings_total = c()
for(i in 1:length(earnings_total)){
  new_earnings_total[i+9] = earnings_total[i]
}
par(mfrow=c(1,1))
index = seq(1,110,1)
plot(index, new_earnings_total, col=ifelse(index == bestq,"forestgreen","black"), pch = ifelse(index==bestq,19,1), xlab = 'Number of Offers', ylab = 'expected contribution to earnings',
     main = "Expected Earnings with Flexible Start Date")


best_bestq = c()
for (a in 1:10){
  for (i in 10:110){
    etmp <- c()
    for (j in 1:100){
      etmp[j]<-flex(i)
    }
    earnings_total[i-9] <- mean(etmp)
  }
  bestq = which.max(earnings_total)+9
  best_bestq[a]=bestq
}
par(mfrow=c(1,1))
hist(best_bestq, breaks = 10, main = "Distribution of Optimal Offers Made\nFlexible Start Date ", xlab = "Optimial Number of Offers",
     ylab = "Occurance")
max_flex_earning = new_earnings_total[bestq]
(diff_earnings_starts=max_flex_earning - max_fixed_earnings)



#################################################################
#### Susan's Model for Mid-Year Recruiting Fixed Start Date ##### 
#################################################################
mid_fixed = function(q1,q2){
  offersmade_jun = q1
  offersmade_dec = q2
  new_analyst_jun = rbinom(1,offersmade_jun,.7)
  new_analyst_dec = rbinom(1,offersmade_dec,.7)
  retention = c(runif(1,.90,1),runif(4,.95,1),runif(1,.8,1),runif(3,.9,1),runif(1,.8,1),runif(3,.9,1),runif(2,.95,1))
  new_analyst1 = c(0,0,new_analyst_jun,0,0,0,0,0,new_analyst_dec,0,0,0,0,0,0)
  demand_hist = c(105,95,75,70,70,110,105,90,65,80,90,120,105,95,75)
  unexpected = rnorm(1, mean = 0, sd = 0.05)
  noise = rnorm(15, mean = 0, sd = 0.1)
  actual = c()
  for(i in 1:15){
    actual[i] = demand_hist[i]*(1+unexpected)*(1+noise[i])
  }
  analyst_next = c(63)
  for(i in 1:14){
    analyst_next[i+1] = analyst_next[i]*retention[i]+new_analyst1[i]
  }
  short = c()
  for(i in 1:15){
    if (analyst_next[i]-actual[i]<0){
      short[i]=-1*(analyst_next[i]-actual[i])
    }
    else
      short[i]=0
  }
  excess = c()
  for(i in 1:15){
    if(analyst_next[i]-actual[i]>0){
      excess[i] = analyst_next[i]-actual[i]
    }
    else
      excess[i] = 0
  }
  excess_cost=6000
  short_cost=3600
  base_contribution=4000
  dec_cost=22000
  earnings=c()
  for(i in 1:15){
    earnings[i] = base_contribution*actual[i]-(short[i]*short_cost+excess[i]*excess_cost)-dec_cost
  }
  sum(earnings)
}



#######################################################################
#### Optimum Offers Made for December Recruiting Fixed Start Date ##### 
#######################################################################
earnings_total1 = c()
earnings_total2 = c()
bestq1 = c()
bestq2 = c()
for(a in 10:110){
  for (i in 10:110){
    etmp <- c()
    for (j in 1:20){
      etmp[j]<-mid_fixed(i,a)
    }
    earnings_total1[i-9] <- mean(etmp)
  }
  earnings_total2[a-9] = max(earnings_total1)
  bestq1[a] = which.max(earnings_total1)+9
}
(bestq2 = which.max(earnings_total2)+9)
(bestq1 = bestq1[bestq2])


new_earnings_total1 = c()
new_earnings_total2 = c()
for(i in 1:length(earnings_total1)){
  new_earnings_total1[i+9] = earnings_total1[i]
}
for(i in 1:length(earnings_total2)){
  new_earnings_total2[i+9] = earnings_total2[i]
}
par(mfrow=c(1,2))
index = seq(1,110,1)
plot(index, new_earnings_total1, col=ifelse(index == bestq1,"forestgreen","black"), pch = ifelse(index==bestq1,19,1), xlab = 'Number of Offers', ylab = 'expected contribution to earnings',
     main = "Expected Earnings with December Recruiting\nQ1")
plot(index, new_earnings_total2, col=ifelse(index == bestq2,"forestgreen","black"), pch = ifelse(index==bestq2,19,1), xlab = 'Number of Offers', ylab = 'expected contribution to earnings',
     main = "Expected Earnings with December Recruiting\nQ2")


q1 = sample(20:50,100,replace = T)
q2 = sample(45:80,100,replace = T)
for(a in 1:30){
  best_earnings = c()
  for(i in 1:100){
    best_earnings[i] = mid_fixed(q1[i],q2[i])
  }
  location[a]=which.max(best_earnings)
}
par(mfrow=c(1,2))
hist(q1[location], breaks = 15, main = "Distribution of Optimal Offers Made\nDecember Recruiting with Fixed Start Date", xlab = "Optimial Number of Offers",
     ylab = "Occurance")
hist(q2[location], breaks = 15, main = "Distribution of Optimal Offers Made\nDecember Recruiting with Fixed Start Date", xlab = "Optimial Number of Offers",
     ylab = "Occurance")



####################################################################
#### Susan's Model for Mid-Year Recruiting Flexible Start Date ##### 
####################################################################
mid_flex = function(q1,q2){
  offersmade_jun = q1
  offersmade_dec = q2
  new_analyst_jun = rbinom(1,offersmade_jun,.7)
  new_analyst_aug = runif(1,.7,1)*(new_analyst_jun/2)
  new_analyst_dec = rbinom(1,offersmade_dec,.7)
  new_analyst_feb = runif(1,.7,1)*(new_analyst_dec/2)
  retention = c(runif(1,.90,1),runif(4,.95,1),runif(1,.8,1),runif(3,.9,1),runif(1,.8,1),runif(3,.9,1),runif(2,.95,1))
  new_analyst1 = c(0,0,new_analyst_jun/2,0,new_analyst_aug,0,0,0,new_analyst_dec/2,0,new_analyst_feb,0,0,0,0)
  demand_hist = c(105,95,75,70,70,110,105,90,65,80,90,120,105,95,75)
  unexpected = rnorm(1, mean = 0, sd = 0.05)
  noise = rnorm(15, mean = 0, sd = 0.1)
  actual = c()
  for(i in 1:15){
    actual[i] = demand_hist[i]*(1+unexpected)*(1+noise[i])
  }
  analyst_next = c(63)
  for(i in 1:14){
    analyst_next[i+1] = analyst_next[i]*retention[i]+new_analyst1[i]
  }
  short = c()
  for(i in 1:15){
    if (analyst_next[i]-actual[i]<0){
      short[i]=-1*(analyst_next[i]-actual[i])
    }
    else
      short[i]=0
  }
  excess = c()
  for(i in 1:15){
    if(analyst_next[i]-actual[i]>0){
      excess[i] = analyst_next[i]-actual[i]
    }
    else
      excess[i] = 0
  }
  excess_cost=6000
  short_cost=3600
  base_contribution=4000
  dec_cost=22000
  earnings=c()
  for(i in 1:15){
    earnings[i] = base_contribution*actual[i]-(short[i]*short_cost+excess[i]*excess_cost)-dec_cost
  }
  sum(earnings)
}



###########################################################################
#### Optimum Offers Made for December Recruiting Flexiable Start Date ##### 
###########################################################################
earnings_total1 = c()
earnings_total2 = c()
bestq1 = c()
bestq2 = c()
for(a in 10:110){
  for (i in 10:110){
    etmp <- c()
    for (j in 1:20){
      etmp[j]<-mid_flex(i,a)
    }
    earnings_total1[i-9] <- mean(etmp)
  }
  earnings_total2[a-9] = max(earnings_total1)
  bestq1[a] = which.max(earnings_total1)+9
}
(bestq2 = which.max(earnings_total2)+9)
(bestq1 = bestq1[bestq2])


new_earnings_total1 = c()
new_earnings_total2 = c()
for(i in 1:length(earnings_total1)){
  new_earnings_total1[i+9] = earnings_total1[i]
}
for(i in 1:length(earnings_total2)){
  new_earnings_total2[i+9] = earnings_total2[i]
}
par(mfrow=c(1,2))
index = seq(1,110,1)
plot(index, new_earnings_total1, col=ifelse(index == bestq1,"forestgreen","black"), pch = ifelse(index==bestq1,19,1), xlab = 'Number of Offers', ylab = 'expected contribution to earnings',
     main = "Expected Earnings with December Recruiting\nQ1")
plot(index, new_earnings_total2, col=ifelse(index == bestq2,"forestgreen","black"), pch = ifelse(index==bestq2,19,1), xlab = 'Number of Offers', ylab = 'expected contribution to earnings',
     main = "Expected Earnings with December Recruiting\nQ2")


q1 = sample(40:70,100,replace = T)
q2 = sample(60:90,100,replace = T)
for(a in 1:30){
  best_earnings = c()
  for(i in 1:100){
    best_earnings[i] = mid_fixed(q1[i],q2[i])
  }
  location[a]=which.max(best_earnings)
}
par(mfrow=c(1,2))
hist(q1[location], breaks = 15, main = "Distribution of Optimal Offers Made\nDecember Recruiting with Flexible Start Date", xlab = "Optimial Number of Offers",
     ylab = "Occurance")
hist(q2[location], breaks = 15, main = "Distribution of Optimal Offers Made\nDecember Recruiting with Fixible Start Date", xlab = "Optimial Number of Offers",
     ylab = "Occurance")