library("rvest")
library("tidyverse")

html = read_html("https://www.basketball-reference.com/leagues/NBA_2023_per_game.html")
table = html %>% html_table()
Player_database=table[[1]]
Player_database = Player_database[order(Player_database$PTS,decreasing = TRUE),]
Player_database = Player_database[27:705,]
Player_database$PTS = as.double(Player_database$PTS)
top_50 = Player_database[order(Player_database$PTS,decreasing = TRUE),][1:54,]
top_50$Player = as.character(top_50$Player)
unique(top_50$Player)
top_50 = top_50[-c(7,14,15,20),]
top_50$Rk = 1:50
write.csv(top_50,"C:/Users/Naman/Downloads/top_50.csv")
