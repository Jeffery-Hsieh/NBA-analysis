library(caret)
library(nnet)
library(neuralnet)

load("weighted_try.RData")
set.seed(0108)


features_name = colnames(new_team_data)[feature_col_num]
#formula of predicting game result
formula = as.formula(paste0("win ~" ,paste(features_name,collapse = "+") ) )
win = model_input_data$win

#row num of model_input_data
n <- nrow(model_input_data)
#test size:0.7
t_size = round(0.7 * n)
#random sample
t_idx <- sample(seq_len(n), size = t_size)
#train data
traindata <- model_input_data[t_idx,]
#test data
testdata <- model_input_data[ - t_idx,]

nnetM <- nnet(formula = win ~ ., linout = F, size = 6, decay = 0.001, maxit = 1000, trace = T, data = traindata)
prediction <- predict(nnetM, testdata, type = 'class')
cm <- table(x = testdata$win, y = prediction, dnn = c("實際", "預測"))
cm

library(caret)

#find the best model
model <- train(form = formula, data = model_input_data, method = "neuralnet", tuneGrid = expand.grid(.layer1 = c(1:4), .layer2 = c(1:4), .layer3 = c(0)), learningrate = 0.01)
model
