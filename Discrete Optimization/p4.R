library(pracma)
library(RMySQL)
connect = dbConnect(RMySQL::MySQL(), dbname="transportation", username="root", password="root")


dcs = dbGetQuery(connect, 'select dc_id, lat, lon from ww_dcs')
sts = dbGetQuery(connect, 'select store_id, lon, lat from ww_stores')


dist = matrix(0,nrow = 46*4535,ncol = 3,byrow = F)
colnames(dist) = c('dcs','stores','distance')
k = 1
for(i in 1:46){
  for(j in 1:4535){
    dist[k,3]=haversine(c(dcs$lat[i],dcs$lon[i]),c(sts$lat[j],sts$lon[j]))
    k = k+1
  }
}
k = 1
for(i in 1:46){
  for(j in 1:4535){
    dist[k,1]=dcs$dc_id[i]
    k = k+1
  }
}
k = 1
for(i in 1:46){
  for(j in 1:4535){
    dist[k,2]=sts$store_id[j]
    k = k+1
  }
}


range = 0
s = 'insert into ww_mileage values'
for(i in 1:415){
  inserts = c()
  for(j in 1:500){
    inserts = c(inserts,sprintf("(%s,%s,%s)",
                                dist[j+range,1], dist[j+range,2], dist[j+range,3]))
    joke=paste(inserts, collapse = ',')
  }
  range = range + 500
  hope = paste(s, joke)
  dbSendQuery(connect, hope)
}
remainder = 207500
for(i in 1:1110){
  s = sprintf("insert into ww_mileage values (%s,%s,%s)",dist[i+remainder,1],
              dist[i+remainder,2], dist[i+remainder,3])
  dbSendQuery(connect, s)
}
