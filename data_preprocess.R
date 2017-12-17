box_score_table_merge = function(each_game_box_score){
    #
    box_score_table = lapply(table,filter,Starters != "Reserves")
    
    team1_players_box_scores = merge(box_score_table[[1]],box_score_table[[2]],by = "Starters")
    team2_players_box_scores = merge(box_score_table[[3]],box_score_table[[4]],by = "Starters")
    
    #
    selected_month_game = list(team1_box_scores = team1_players_box_scores,
                               team2_box_scores = team2_players_box_scores)
  
}

game_info_mapping = function(each_month_game,game_date_table){
  result = list()
  for (i in seq_len(nrow(game_date_table))) {
    PTS_1 = as.numeric(game_date_table[i,4])
    PTS_2 = as.numeric(game_date_table[i,6])
    Home_team_name = game_date_table[i,"Home/Neutral"]
    Visitor_team_name = game_date_table[i,"Visitor/Neutral"]
    winner = ifelse(PTS_1 < PTS_2,Home_team_name,Visitor_team_name)
    
    result[i] = list(list(game_date_table[i,"Date"],
                          Visitor_team_name,Home_team_name,
                          ifelse(PTS_1 < PTS_2,Home_team_name,Visitor_team_name),
                          each_month_game[[i]][[1]],
                          each_month_game[[i]][[2]]
                     )
    )
                     
    names(result[[i]]) = c("Date","Visitor_team","Home_team","Winner_team",paste0(Home_team_name,"_box_scores"),paste0(Visitor_team_name,"_box_scores"))
  }
  return(result)
}
