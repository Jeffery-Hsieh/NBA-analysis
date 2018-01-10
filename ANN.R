library(caret)
library(nnet)
library(neuralnet)

source("data_trans_to_model_input.R")

load("new_team_data.RData")
load("diff_data.RData")

new_team_data = trans_to_model_input(new_team_data)
diff_data = trans_to_model_input(diff_data)

set.seed(0108)

#features col num
win_col = grep(pattern = "win",colnames(data))
feature_col_num = grep(pattern = "usg_*",colnames(data))

features_name = colnames(diff_data)[feature_col_num]

#row num of diff_data
n <- nrow(diff_data)
#test size:0.7
t_size = round(0.7 * n)
#random sample
t_idx = sample(seq_len(n), size = t_size)
#train data
traindata1 = new_team_data[t_idx,]
traindata2 = diff_data[t_idx,]
#test data
testdata1 = new_team_data[ - t_idx,]
testdata2 = diff_data[ - t_idx,]

#build ANN model
model <- train(win ~ ., data = traindata1, method='nnet', 
               linout = FALSE, trace = FALSE,
               tuneGrid=expand.grid(.size=c(1,5,10),.decay=c(0,0.001,0.1))) 

nnetM <- nnet(formula = win ~ ., linout = F, size = 5, decay = 0.1, trace = FALSE, data = traindata1)

#test score
prediction <- predict(nnetM, testdata1, type = 'class')
ans <- table(x = testdata1$win, y = prediction, dnn = c("實際", "預測"))
ans


