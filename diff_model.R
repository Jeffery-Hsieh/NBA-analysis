read.csv('diff_data')

#SVM建模
traindata4 <- diff_data[1:1000,c(4:10)]
testdata4 <- diff_data[1001:1173,c(5:10)]
svmfit4 = svm(as.factor(win) ~ .,data=traindata4,
              kernel = "linear",
              cost = 1 ,scale = F,gamma=0.5)
predict4 = predict(svmfit4,testdata4)
ans4 = table(predict4, diff_data[1001:1173,4])

##建構預測模型
#要預測第n場，取前k場的平均數據做預測
nba_predict <- function(data,n,k){
  last_k_game_mean = apply(data[(n-k):(n-1),],2,mean)
  last_k_game_mean <- as.matrix(t(as.data.frame(last_k_game_mean)))
  pred_value = predict(svmfit4,last_k_game_mean)
  return(pred_value)
}

##熱火隊勝負預測
Heat_data <- diff_data[diff_data$Team == 'Miami Heat',]
Heat_data$Gameid
Heat_input <- Heat_data[,c(5:10)]
nba_predict(Heat_input,39,3)
Heat_data$win[37:39]
