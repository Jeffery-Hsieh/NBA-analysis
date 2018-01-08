library(e1071)
library(caret)

source("weighted_try.R")

#SVM test
names(new_team_data)
dim(new_team_data)

#find the best model
features_name = colnames(new_team_data)[feature_col_num]
model = train(as.formula(paste0("win ~" ,paste(features_name,collapse = "+") ) ),data = model_input_data,method = "svmPoly",trControl = trainControl(method = "cv",number = 5,verboseIter = TRUE))

#build model
traindata2 <- new_team_data[1:1000,c(feature_col_num)]
testdata2 <- new_team_data[1001:1128, c(win,feature_col_num)]

svmfit2 = svm(win ~ ., data = traindata2,
              kernel = "polynomial",degree = 1,
              cost = 1, scale = FALSE)
predict2 = predict(svmfit2, testdata2)
(ans2 = table(predict2, new_team_data[1001:1128,4]))
