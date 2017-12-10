data_preprocess = function(each_month_game_box_score_tables,game_result_table_list,month){
  for (i in length(game_result_table_list[[month]]$Date)) {
    #
    date = as.character(game_result_table_list[[month]][["Date"]][i])
    visitor = game_result_table_list[[month]][[`Visitor/Neutral`]][i]
    home = game_result_table_list[[month]][[`Home/Neutral`]][i]
    
    #
    table = each_month_game_box_score_tables[[i]]
    
    team1_players_box_scores = left_join(table[[1]],table[[2]],by = "Starters")
    team2_players_box_scores = left_join(table[[1]],table[[2]],by = "Starters")
    
    #
    selected_month_game = list(list(date = date,visitor = visitor,home = home,
                             team1_box_scores = team1_players_box_scores,
                             team2_box_scores = team2_players_box_scores))
  }
}
