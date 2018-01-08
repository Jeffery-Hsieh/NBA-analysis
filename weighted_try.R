library(dplyr)

set.seed(22)
###
clear_data <- read.csv('nba_player_data.csv')

new_team_data <- clear_data[!duplicated(clear_data$Gameid),]
new_team_data <- new_team_data[,c(40,37,38,41)]

#加權USG
usg_eFG <- lapply(split(clear_data, clear_data$Gameid),
                  function(z) weighted.mean(as.numeric(z$`eFG%`),as.numeric(z$`USG%`))) 
usg_eFG <- unlist(usg_eFG)
v1 <- as.data.frame(cbind(names(usg_eFG),usg_eFG))
names(v1)[1] <- 'Gameid'
usg_TRB <- lapply(split(clear_data, clear_data$Gameid),
                  function(z) weighted.mean(as.numeric(z$`TRB%`),as.numeric(z$`USG%`))) 
usg_TRB <- unlist(usg_TRB)
v2 <- as.data.frame(cbind(names(usg_TRB),usg_TRB))
names(v2)[1] <- 'Gameid'
usg_TOV <- lapply(split(clear_data, clear_data$Gameid),
                  function(z) weighted.mean(as.numeric(z$`TOV%`),as.numeric(z$`USG%`))) 
usg_TOV <- unlist(usg_TOV)
v3 <- as.data.frame(cbind(names(usg_TOV),usg_TOV))
names(v3)[1] <- 'Gameid'
usg_FT <- lapply(split(clear_data, clear_data$Gameid),
                 function(z) weighted.mean(as.numeric(z$`FTr`),as.numeric(z$`USG%`))) 
usg_FT <- unlist(usg_FT)
v4 <- as.data.frame(cbind(names(usg_FT),usg_FT))
names(v4)[1] <- 'Gameid'
usg_ORtg <- lapply(split(clear_data, clear_data$Gameid),
                 function(z) weighted.mean(as.numeric(z$ORtg),as.numeric(z$`USG%`))) 
usg_ORtg <- unlist(usg_ORtg)
v5 <- as.data.frame(cbind(names(usg_ORtg),usg_ORtg))
names(v5)[1] <- 'Gameid'
usg_DRtg <- lapply(split(clear_data, clear_data$Gameid),
                   function(z) weighted.mean(as.numeric(z$DRtg),as.numeric(z$`USG%`))) 
usg_DRtg <- unlist(usg_DRtg)
v6 <- as.data.frame(cbind(names(usg_DRtg),usg_DRtg))
names(v6)[1] <- 'Gameid'



new_team_data <- merge(new_team_data,v1,by='Gameid')
new_team_data <- merge(new_team_data,v2,by='Gameid')
new_team_data <- merge(new_team_data,v3,by='Gameid')
new_team_data <- merge(new_team_data,v4,by='Gameid')
new_team_data <- merge(new_team_data,v5,by='Gameid')
new_team_data <- merge(new_team_data,v6,by='Gameid')


#找變數




#加權時間
times_eFG <- lapply(split(clear_data, clear_data$Gameid),
                    function(z) weighted.mean(as.numeric(z$`eFG%`),as.numeric(z$times))) 
times_eFG <- unlist(times_eFG)
v1 <- as.data.frame(cbind(names(times_eFG),times_eFG))
names(v1)[1] <- 'Gameid'
times_TRB <- lapply(split(clear_data, clear_data$Gameid),
                    function(z) weighted.mean(as.numeric(z$`TRB%`),as.numeric(z$times))) 
times_TRB <- unlist(times_TRB)
v2 <- as.data.frame(cbind(names(times_TRB),times_TRB))
names(v2)[1] <- 'Gameid'
times_TOV <- lapply(split(clear_data, clear_data$Gameid),
                    function(z) weighted.mean(as.numeric(z$`TOV%`),as.numeric(z$times))) 
times_TOV <- unlist(times_TOV)
v3 <- as.data.frame(cbind(names(times_TOV),times_TOV))
names(v3)[1] <- 'Gameid'
times_FT <- lapply(split(clear_data, clear_data$Gameid),
                   function(z) weighted.mean(as.numeric(z$`FTr`),as.numeric(z$times))) 
times_FT <- unlist(times_FT)
v4 <- as.data.frame(cbind(names(times_FT),times_FT))
names(v4)[1] <- 'Gameid'

new_team_data <- merge(new_team_data,v1,by='Gameid')
new_team_data <- merge(new_team_data,v2,by='Gameid')
new_team_data <- merge(new_team_data,v3,by='Gameid')
new_team_data <- merge(new_team_data,v4,by='Gameid')
View(new_team_data)

#data content-factor to numeric

#num of win_col and input features
win_col = grep(pattern = "win",colnames(new_team_data))
feature_col_num = grep(pattern = "usg_*",colnames(new_team_data))

for (i in feature_col_num) {
  col = new_team_data[,i]
  new_team_data[,i] = as.numeric(levels(col))[col]
}

new_team_data[,win_col] = as.factor(new_team_data[,win_col])
summary(new_team_data)

#input data 
model_input_data = new_team_data[,c(feature_col_num,win_col)]
