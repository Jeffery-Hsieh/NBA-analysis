###
clear_data <- read.csv('nba_player_data.csv')

new_team_data <- clear_data[!duplicated(clear_data$Gameid),]
new_team_data <- new_team_data[,c(40,37,38,41)]

#加權USG
usg_eFG <- lapply(split(clear_data, clear_data$Gameid),
                  function(z) weighted.mean(as.numeric(z$`eFG%`),as.numeric(z$`USG%`))) 
usg_eFG <- unlist(usg_eFG)
v1 <- as.data.frame(cbind(names(usg_eFG),usg_eFG))
names(v1)[1] <- 'Gameid'
usg_TRB <- lapply(split(clear_data, clear_data$Gameid),
                  function(z) weighted.mean(as.numeric(z$`TRB%`),as.numeric(z$`USG%`))) 
usg_TRB <- unlist(usg_TRB)
v2 <- as.data.frame(cbind(names(usg_TRB),usg_TRB))
names(v2)[1] <- 'Gameid'
usg_TOV <- lapply(split(clear_data, clear_data$Gameid),
                  function(z) weighted.mean(as.numeric(z$`TOV%`),as.numeric(z$`USG%`))) 
usg_TOV <- unlist(usg_TOV)
v3 <- as.data.frame(cbind(names(usg_TOV),usg_TOV))
names(v3)[1] <- 'Gameid'
usg_FT <- lapply(split(clear_data, clear_data$Gameid),
                 function(z) weighted.mean(as.numeric(z$`FTr`),as.numeric(z$`USG%`))) 
usg_FT <- unlist(usg_FT)
v4 <- as.data.frame(cbind(names(usg_FT),usg_FT))
names(v4)[1] <- 'Gameid'
usg_ORtg <- lapply(split(clear_data, clear_data$Gameid),
                   function(z) weighted.mean(as.numeric(z$ORtg),as.numeric(z$`USG%`))) 
usg_ORtg <- unlist(usg_ORtg)
v5 <- as.data.frame(cbind(names(usg_ORtg),usg_ORtg))
names(v5)[1] <- 'Gameid'
usg_DRtg <- lapply(split(clear_data, clear_data$Gameid),
                   function(z) weighted.mean(as.numeric(z$DRtg),as.numeric(z$`USG%`))) 
usg_DRtg <- unlist(usg_DRtg)
v6 <- as.data.frame(cbind(names(usg_DRtg),usg_DRtg))
names(v6)[1] <- 'Gameid'



new_team_data <- merge(new_team_data,v1,by='Gameid')
new_team_data <- merge(new_team_data,v2,by='Gameid')
new_team_data <- merge(new_team_data,v3,by='Gameid')
new_team_data <- merge(new_team_data,v4,by='Gameid')
new_team_data <- merge(new_team_data,v5,by='Gameid')
new_team_data <- merge(new_team_data,v6,by='Gameid')
names(new_team_data)

#找變數




#加權時間
times_eFG <- lapply(split(clear_data, clear_data$Gameid),
                    function(z) weighted.mean(as.numeric(z$`eFG%`),as.numeric(z$times))) 
times_eFG <- unlist(times_eFG)
v1 <- as.data.frame(cbind(names(times_eFG),times_eFG))
names(v1)[1] <- 'Gameid'
times_TRB <- lapply(split(clear_data, clear_data$Gameid),
                    function(z) weighted.mean(as.numeric(z$`TRB%`),as.numeric(z$times))) 
times_TRB <- unlist(times_TRB)
v2 <- as.data.frame(cbind(names(times_TRB),times_TRB))
names(v2)[1] <- 'Gameid'
times_TOV <- lapply(split(clear_data, clear_data$Gameid),
                    function(z) weighted.mean(as.numeric(z$`TOV%`),as.numeric(z$times))) 
times_TOV <- unlist(times_TOV)
v3 <- as.data.frame(cbind(names(times_TOV),times_TOV))
names(v3)[1] <- 'Gameid'
times_FT <- lapply(split(clear_data, clear_data$Gameid),
                   function(z) weighted.mean(as.numeric(z$`FTr`),as.numeric(z$times))) 
times_FT <- unlist(times_FT)
v4 <- as.data.frame(cbind(names(times_FT),times_FT))
names(v4)[1] <- 'Gameid'

new_team_data <- merge(new_team_data,v1,by='Gameid')
new_team_data <- merge(new_team_data,v2,by='Gameid')
new_team_data <- merge(new_team_data,v3,by='Gameid')
new_team_data <- merge(new_team_data,v4,by='Gameid')
View(new_team_data)

#SVM test
names(new_team_data)
dim(new_team_data)

#data class transformation
win_col = grep(pattern = "win",colnames(new_team_data))
feature_col_num = grep(pattern = "usg_*",colnames(new_team_data))

for (i in feature_col_num) {
  col = new_team_data[,i]
  new_team_data[,i] = as.numeric(levels(col))[col]
}
new_team_data[,win_col] = as.factor(new_team_data[,win_col])
summary(new_team_data)


##測試模型
traindata2 <- new_team_data[1:1000,c(4:8,13,14)]
testdata2 <- new_team_data[1001:1128, c(5:8,13,14)]
svmfit2 = svm(as.factor(win) ~ ., data = traindata2, 
              kernel = "linear", 
              cost = 1, scale = FALSE,gamma=0.5)
predict2 = predict(svmfit2,testdata2)
ans2 = table(predict2, new_team_data[1001:1128,4])

svm_tune = tune(svm, train.x=apply(traindata2[,-1],2, as.numeric),
                train.y=traindata2[,1],kernel="polynomial",ranges=list(cost=10^(-1:2),
                                                                       gamma=c(.5,1,2)))

##測試模型(將比賽場次排序)
nu_id = as.numeric(levels(new_team_data$Gameid))[new_team_data$Gameid]
new_team_data$Gameid = nu_id
new2 <- new_team_data[order(new_team_data$Gameid),]
new2$Gameid <- c(1:1128)

traindata3 <- new2[1:1000,c(4:10)]
testdata3 <- new2[1001:1128, c(5:10)]
svmfit3 = svm(as.factor(win) ~ ., data = traindata3, 
              kernel = "linear", 
              cost = 1, scale = FALSE,gamma=0.5)
predict3 = predict(svmfit3,testdata3)
ans3 = table(predict3, new_team_data[1001:1128,4])


##預測第100場，k=5
new22 <- new2[,c(-1:-4,-9:-12)]
k5 <- apply(new22[95:99,],2,mean)
k5 <- as.matrix(t(as.data.frame(k5)))
pp <- predict(svmfit2,k5)
pp

##建構預測模型
#要預測第n場，取前k場的平均數據做預測
nba_predict <- function(data,n,k){
  last_k_game_mean = apply(data[(n-k):(n-1),],2,mean)
  last_k_game_mean <- as.matrix(t(as.data.frame(last_k_game_mean)))
  pred_value = predict(svmfit2,last_k_game_mean)
  return(pred_value)
}
predict_output <- c()
for(i in 1:100){
  vv <- nba_predict(testdata3,6+i,6)
  predict_output <- c(predict_output,vv)
}
table(predict_output,new_team_data[1007:1106,4])

##熱火隊勝負預測
single_team_data <- new2[new2$Team == 'Miami Heat',]
Heat_data$Gameid
Heat_input <- Heat_data[,c(5:10)]
nba_predict(Heat_input,37,5)
Heat_data$win[35:37]
