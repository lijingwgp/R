## Making KS Table and Finding cutoff
options(scipen = 999)
table1 <- quantile('predictions', probs = seq(0,1,0.01))
table2 <- as.data.frame(table1)
table3 <- as.data.frame(table2[rev(rownames(table2)),])
table4 <- as.data.frame(table3[2:nrow(table3),])
table4$Low <- seq(from = (1-0.01), to = 0, by = -0.01)
table4$High <- seq(from = 1, to = 0.01, by = -0.01)
names(table4)[1] <- 'Value'
obs_in_interval <- Grid_prediction2['predictions' >= table4[1,'Value'],]
outputTable <- table4
outputTable$Cumulative_1_Captured <- NULL
outputTable$Cumulative_0_Captured <- NULL
outputTable$Separation <- NULL
outputTable$Obs_Counter <- NULL
outputTable$Obs_Percentage <- NULL
outputTable[1,'Cumulative_1_Captured'] <- sum(obs_in_interval[,label]==1) / sum(Grid_prediction2[,label]==1)
outputTable[1,'Cumulative_0_Captured'] <- sum(obs_in_interval[,label]==0) / sum(Grid_prediction2[,label]==0)
outputTable[1,'Separation'] <- outputTable[1,4] - outputTable[1,5]
outputTable[1,'Obs_Counter'] <- nrow(obs_in_interval)
outputTable[1,'Obs_Percentage'] <- outputTable[1,'Obs_Counter']/nrow(Grid_prediction2)
for(y in 2:nrow(table4))
{
  obs_in_interval <- Grid_prediction2[Grid_prediction2[,xgbcv_final.pred] >= table4[y,'Value'] &
                                        Grid_prediction2[,xgbcv_final.pred] < table4[y-1,'Value'],]
  outputTable[y,'Cumulative_1_Captured'] <- (sum(obs_in_interval[,label]==1)/sum(Grid_prediction2[,label]==1))+
    outputTable[y-1,'Cumulative_1_Captured']
  outputTable[y,'Cumulative_0_Captured'] <- (sum(obs_in_interval[,label]==0)/sum(Grid_prediction2[,label]==0))+
    outputTable[y-1,'Cumulative_0_Captured']
  outputTable[y,'Separation'] <- outputTable[y,4]-outputTable[y,5]
  outputTable[y,'Obs_Counter'] <- nrow(obs_in_interval)
  outputTable[y,'Obs_Percentage'] <- (outputTable[y,'Obs_Counter']/nrow(Grid_prediction2)) +
    outputTable[y-1,'Obs_Percentage']
}
Grid_KS_Table <- outputTable
cutoff <- Grid_KS_Table[which.max(Grid_KS_Table$Separation),1]
