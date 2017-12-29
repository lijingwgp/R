rm(list=ls())
library(reshape2)
library(RMySQL)
library(lattice)



nas = read.csv("NasdaqReturns.csv", sep = ",", header = T)
nas_mat = data.matrix(nas)[,-c(1:3)]
rownames(nas_mat) = nas[,1]
#nas_mat = nas_mat[1:5,1:5]
nas_tran = t(nas_mat)
nas_cov = cov(nas_tran)
small=melt(nas_cov)



connect = dbConnect(RMySQL::MySQL(), dbname="nasdaq", username="root", password="root")
dbListTables(connect)
dbSendQuery(connect, 'use nasdaq')
dbSendQuery(connect, 'drop table covariance')
dbSendQuery(connect, 'drop table return_rate')
dbSendQuery(connect, 'drop table combination')
dbSendQuery(connect, 'create table covariance(stock1 varchar(10),stock2 varchar(10),cov double)')
dbSendQuery(connect, 'create table return_rate(ret double)')
dbGetQuery(connect, 'select count(*) from covariance')
dbGetQuery(connect, 'select count(*) from return_rate')



range = 0
s = 'insert into covariance values'
for(i in 1:2681){
  inserts = c()
  for(j in 1:500){
    inserts = c(inserts,sprintf("('%s','%s',%s)",
                                 small[j+range,1], small[j+range,2], small[j+range,3]))
    joke=paste(inserts, collapse = ',')
  }
  range = range + 500
  hope = paste(s, joke)
  dbSendQuery(connect, hope)
}
remainder = 1340500
for(i in 1:464){
  s = sprintf("insert into covariance values ('%s','%s',%s)",small[i+remainder,1],
              small[i+remainder,2], small[i+remainder,3])
  dbSendQuery(connect, s)
}



avg_ret = c()
for(i in 1:1158){
  avg_ret[i]=mean(nas_tran[,i])
}
for(i in 1:1158){
  s = sprintf('insert into return_rate values (%s)',avg_ret[i])
  dbSendQuery(connect, s)
}



variance = c()
for(i in 1:1158){
  variance[i] = var(nas_tran[,i])
}
min(variance)
which.min(variance)
max(variance)
which.max(variance)
(max(variance)-min(variance))/25



combination = c()
combination=dbGetQuery(connect, 'select * from combination')
xyplot(combination$expected_return ~ combination$risk, type=c("l","p"))

