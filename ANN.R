library(caret)
library(nnet)
library(neuralnet)

source("data_trans_to_model_input.R")

load("diff_data.RData")

diff_data = trans_to_model_input(diff_data)

set.seed(0108)

features_name = colnames(diff_data)[feature_col_num]

#formula of predicting game result
formula = as.formula(paste0("win ~" ,paste(features_name,collapse = "+") ) )
win = diff_data$win

#row num of diff_data
n <- nrow(diff_data)
#test size:0.7
t_size = round(0.7 * n)
#random sample
t_idx <- sample(seq_len(n), size = t_size)
#train data
traindata <- diff_data[t_idx,]
#test data
testdata <- diff_data[ - t_idx,]

#build ANN model
model <- train(win ~ ., data = traindata, method='nnet', 
               linout = FALSE, trace = FALSE,
               tuneGrid=expand.grid(.size=c(1,5,10),.decay=c(0,0.001,0.1))) 

nnetM <- nnet(formula = win ~ ., linout = F, size = 5, decay = 0, trace = FALSE, data = traindata)

#test score
prediction <- predict(nnetM, testdata, type = 'class')
ans <- table(x = testdata$win, y = prediction, dnn = c("實際", "預測"))
ans


