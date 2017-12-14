box_score_table_merge = function(each_game_box_score){
    #
    box_score_table = lapply(table,filter,Starters != "Reserves")
    
    team1_players_box_scores = merge(box_score_table[[1]],box_score_table[[2]],by = "Starters")
    team2_players_box_scores = merge(box_score_table[[3]],box_score_table[[4]],by = "Starters")
    
    #
    selected_month_game = list(team1_box_scores = team1_players_box_scores,
                               team2_box_scores = team2_players_box_scores)
  
}
