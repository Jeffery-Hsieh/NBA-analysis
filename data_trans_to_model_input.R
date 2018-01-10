trans_to_model_input = function(data){
  
  #num of win_col and input features
  win_col = grep(pattern = "win",colnames(data))
  feature_col_num = grep(pattern = "usg_*",colnames(data))
  
  #data content-factor to numeric
  #for (i in feature_col_num) {
  #  col = data[,i]
  #  data[,i] = as.numeric(levels(col))[col]
  #}
  
  #data[,win_col] = as.factor(data[,win_col])
  
  #input data 
  model_input_data = data[,c(feature_col_num,win_col)]
}

