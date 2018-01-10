library(e1071)
library(caret)

source("data_trans_to_model_input.R")
load("diff_data.RData")

diff_data = trans_to_model_input(diff_data)

#data featurea
win_col = grep(pattern = "win",colnames(diff_data))
feature_col_num = grep(pattern = "usg_*",colnames(diff_data))

#formula for model
features_name = colnames(diff_data)[feature_col_num]

#build model with clean data
traindata1 <- new_team_data[1:1000,c(feature_col_num)]
testdata1 <- new_team_data[1001:1128, c(win_col,feature_col_num)]

before_diff_svmfit = svm(win ~ ., data = traindata1,
              kernel = "polynomial",degree = 1,
              cost = 1, scale = FALSE)

#test score
predict1 = predict(before_diff_svmfit, testdata1)
(ans1 = table(predict1, new_team_data[1001:1128,4]))


#build model with preprocessed data
traindata2 <- diff_data[1:1000,c(win_col,feature_col_num)]
testdata2 <- diff_data[1001:1173,c(win_col,feature_col_num)]
after_diff_svmfit = svm(as.factor(win) ~ .,data = traindata2,
                        kernel = "linear",
                        cost = 1 ,scale = F,gamma=0.5)

#test score
predict2 = predict(after_diff_svmfit,testdata2)
ans2 <- table(x = testdata2$win, y = predict2, dnn = c("實際", "預測"))
ans2
