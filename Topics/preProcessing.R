rm(list=ls())
bank = read.csv("bank-additional-full.csv",sep=",")
## downsampling
no.indices = which(bank[,21] == "no")
bank.no = bank[no.indices,]
bank.temp1 = bank.no[sample(1:nrow(bank.no),10000*0.7),]
bank.temp2 = setdiff(bank,bank.temp1)
yes.indices = which(bank.temp2[,21] == "yes")
bank.temp2 = bank.temp2[yes.indices,]
bank.temp2 = bank.temp2[sample(1:nrow(bank.temp2),10000*0.3),]
bank.sample = rbind(bank.temp1,bank.temp2)
bank.sample = bank.sample[sample(1:nrow(bank.sample)),]
bank.sample$y=as.character(bank.sample$y)
for (i in 1:nrow(bank.sample)){
  if (bank.sample[i,21]=="yes"){
    bank.sample[i,21]=1
  } else {
    bank.sample[i,21]=0
  }
}
bank.sample$y = as.numeric(bank.sample$y)
bank.sample$temp = rep(0,nrow(bank.sample))
## one-hot-encoding
bank.encoded = model.matrix(temp~., bank.sample)[,-1]
write.csv(bank.encoded, "bankEncoded.csv")

