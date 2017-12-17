rm(list = ls(all.names = T))

library(rvest)
library(dplyr)
library(RCurl)
library(XML)

source("data_preprocess.R")

#get the analysis table of each game
get_web_tables = function(url) {
  tables = getURL(url) %>%
  htmlParse() %>%
  readHTMLTable(header = TRUE,stringsAsFactors = FALSE)
}

#get the content of desired html tag
get_web_content = function(urls,xpath) {
    content = lapply(urls,function(x) read_html(x) %>%
      html_nodes(xpath = xpath) %>%
      html_text())
  
  #return type according to input type
  if(class(urls) == "list") {
    return(content)
  }else{
    return(content[[1]])
  }
  
}

#NBA reference web url
NBA_url = "https://www.basketball-reference.com"
NBA_2017_games = paste0(NBA_url,"/leagues/NBA_2017_games.html")

#each games suffix url
each_month_tag_href = read_html(NBA_2017_games) %>%
  html_nodes(xpath = '//div[@class = "filter"]/div/a') %>%
  html_attr("href")
  
each_month_url = paste0(NBA_url,each_month_tag_href)

#get the table of game result
game_result_table_list = get_web_tables(each_month_url)
names(game_result_table_list) = get_web_content(each_month_url,xpath = '//div[@class = "filter"]/div/a')
game_result_table_list$April = game_result_table_list$April[-96,]

#each game box score url
each_game_box_score_tag_href = each_month_url %>%
  lapply(read_html) %>%
  lapply(html_nodes,xpath = '//td[@data-stat = "box_score_text"]/a') %>%
  lapply(html_attr,"href")
  
each_game_box_score_url = lapply(each_game_box_score_tag_href,function(x) paste0(NBA_url,x))

#get the analysis of game on October
October = each_game_box_score_url[[1]]
October_box_score_tables = lapply(as.list(October),function(x) get_web_tables(x) %>% box_score_table_merge()) 
October_game_analysis = game_info_mapping(October_box_score_tables,game_result_table_list$October)

November = each_game_box_score_url[[2]]
November_box_score_tables = lapply(as.list(November),function(x) get_web_tables(x) %>% box_score_table_merge())
November_game_analysis = game_info_mapping(November_box_score_tables,game_result_table_list$November)

December = each_game_box_score_url[[3]]
December_box_score_tables = lapply(as.list(December),function(x) get_web_tables(x) %>% box_score_table_merge())
December_game_analysis = game_info_mapping(December_box_score_tables,game_result_table_list$December)

January = each_game_box_score_url[[4]]
January_box_score_tables = lapply(as.list(January),function(x) get_web_tables(x) %>% box_score_table_merge())
January_game_analysis = game_info_mapping(January_box_score_tables,game_result_table_list$January)

February = each_game_box_score_url[[5]]
February_box_score_tables = lapply(as.list(February),function(x) get_web_tables(x) %>% box_score_table_merge())
February_game_analysis = game_info_mapping(February_box_score_tables,game_result_table_list$February)

March = each_game_box_score_url[[6]]
March_box_score_tables = lapply(as.list(March),function(x) get_web_tables(x) %>% box_score_table_merge())
March_game_analysis = game_info_mapping(March_box_score_tables,game_result_table_list$March)

April = each_game_box_score_url[[7]]
April_box_score_tables = lapply(as.list(April),function(x) get_web_tables(x) %>% box_score_table_merge())
April_game_analysis = game_info_mapping(April_box_score_tables,game_result_table_list$April)

May = each_game_box_score_url[[8]]
May_box_score_tables = lapply(as.list(May),function(x) get_web_tables(x) %>% box_score_table_merge())
May_game_analysis = game_info_mapping(May_box_score_tables,game_result_table_list$May)


June = each_game_box_score_url[[9]]
June_box_score_tables = lapply(as.list(June),function(x) get_web_tables(x) %>% box_score_table_merge())
June_game_analysis = game_info_mapping(June_box_score_tables,game_result_table_list$June)

