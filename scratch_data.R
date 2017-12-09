rm(list = ls(all.names = T))

library(rvest)
library(dplyr)
library(RCurl)

#get the analysis table of each game
get_web_tables = function(url) {
  tables = getURL(url) %>%
  htmlParse() %>%
  readHTMLTable(header = T)
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
NBA_2017_games = paste0(NBA_url,"leagues/NBA_2017_games.html")

#each games suffix url
each_month_tag_href = read_html(NBA_2017_games) %>%
  html_nodes(xpath = '//div[@class = "filter"]/div/a') %>%
  html_attr("href")
  
each_month_url = paste0(NBA_url,each_month_tag_href)

#get the table of game result
game_result_table_list = get_web_tables(each_month_url)
names(game_result_table_list) = get_web_content(each_month_url,xpath = '//div[@class = "filter"]/div/a')

#each game box score url
each_game_box_score_tag_href = paste0(NBA_url,each_month_url) %>%
  lapply(read_html) %>%
  lapply(html_nodes,xpath = '//td[@data-stat = "box_score_text"]/a') %>%
  lapply(html_attr,"href")
  
each_game_box_score_url = lapply(box_score_url,function(x) paste0(NBA_url,x))
October = each_game_box_score_url[[1]]
each_game_box_score_tables = lapply(October,get_web_tables)
