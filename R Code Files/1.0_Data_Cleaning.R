################################################# 1.0 DATA CLEANING ################################################# 
#####################################################################################################################
library(dplyr)
library(tidyverse)
library(hoopR)

# Retrieve player stats from last complete season (2022-23) from HoopR package 
df <- as.data.frame(nba_leaguedashplayerstats(season='2022-23'))
head(df)

# Re-format column names 
colnames(df) <- gsub("LeagueDashPlayerStats.","",colnames(df))

# Convert numeric columns to type numeric
df <- df %>% mutate(across(-c("TEAM_ID","TEAM_ABBREVIATION","NICKNAME","PLAYER_NAME","PLAYER_ID"), as.numeric))

# Ensure no player has more than 1 row
df %>% group_by(PLAYER_NAME) %>% tally() %>% arrange(-n) 

length(unique(df$PLAYER_ID))  # 539 Unique Players in the data 

# Ensure every player in the data played in at least one game, and for at least one minute
df <- df %>% filter(GP > 0 & MIN > 0)
head(df)

# Determine if there are any missing/null values 
sum(is.na(df))
head(df)

# Convert the data to be on a per-game basis
convert_cols <- c("MIN","FGM","FGA","FG3M","FG3A","FTM","FTA","OREB","DREB","REB","AST","TOV","STL","BLK","BLKA","PF","PTS")
for(col in convert_cols) {
  df[[col]] <- df[[col]] / df$GP
}

# Select per-game stats only 
df <- df[, 1:32]
head(df)

# Rename a few columns for joins later on 
df <- df %>% rename("Player" = PLAYER_NAME, "Tm" = TEAM_ABBREVIATION)
head(df)

# Import advanced stats, retrieved from Basketball Reference
data <- read.csv('/Users/laurenmanis/Desktop/NBA Draft Project/player_stats_adv.csv')

# Ensure no player has more than 1 row
data %>% group_by(Player) %>% tally() %>% arrange(-n) 

length(unique(data$Player))  # 539 Unique Players in the data - matched with HoopR data

# Ensure every player in the data played in at least one game, and for at least one minute
data <- data %>% filter(G > 0 & MP > 0)
head(data)

# Drop Index columns, created from csv conversion 
data <- data %>% select(-c("X.1","X"))

# Determine if there are any missing/null values 
sum(is.na(data))

# Identify where the missing values are 
colSums(is.na(data))

# Replace all NA with 0, working under the assumption that if someone has 3 point attempts, they did not shoot any threes, etc
data[is.na(data)] <- 0

# Drop unnecessary columns 
data <- data %>% select(-c("Age","MP","Player.additional"))

# Ensure that all players are named consistently across the dataframes
player_df <- df %>% select(Player) %>% arrange(Player) %>% pull()
player_data <- data %>% select(Player) %>% arrange(Player) %>% pull()

missing_a <- setdiff(player_df, player_data)
missing_b <- setdiff(player_data, player_df)

# Re-format names that are not matching 
name_corrections <- c(
  "AJ Green" = "A.J. Green",
  "Boban Marjanovic" = "Boban Marjanović",
  "Bogdan Bogdanovic" = "Bogdan Bogdanović",
  "Bojan Bogdanovic" = "Bojan Bogdanović",
  "Dario Saric" = "Dario Šarić",
  "Davis Bertans" = "Dāvis Bertāns",
  "Dennis Schroder" = "Dennis Schröder",
  "Goran Dragic" = "Goran Dragić",
  "Jeff Dowtin Jr." = "Jeff Dowtin",
  "John Butler Jr." = "John Butler",
  "Jonas Valanciunas" = "Jonas Valančiūnas",
  "Juancho Hernangomez" = "Juancho Hernangómez",
  "Jusuf Nurkic" = "Jusuf Nurkić",
  "Kevin Knox II" = "Kevin Knox",
  "Kristaps Porzingis" = "Kristaps Porziņģis",
  "Luka Doncic" = "Luka Dončić",
  "Luka Samanic" = "Luka Šamanić",
  "Marcus Morris Sr." = "Marcus Morris",
  "Moussa Diabate" = "Moussa Diabaté",
  "Nikola Jokic" = "Nikola Jokić",
  "Nikola Jovic" = "Nikola Jović", 
  "Nikola Vucevic" = "Nikola Vučević",
  "Reggie Bullock Jr." = "Reggie Bullock",
  "Robert Williams III" = "Robert Williams",
  "Theo Maledon" = "Théo Maledon",
  "Vlatko Cancar" = "Vlatko Čančar",
  "Willy Hernangomez" = "Willy Hernangómez",
  "Xavier Tillman" = "Xavier Tillman Sr."
)

df <- df %>% mutate(Player = ifelse(Player %in% names(name_corrections), name_corrections[Player], Player))
data <- data %>% mutate(Player = ifelse(Player %in% names(name_corrections), name_corrections[Player], Player))

# Join the two dataframes
df <- data %>%
  left_join(df, by="Player") %>%
  select(-c("PLAYER_ID","NICKNAME","TEAM_ID","Tm.x","AGE","GP","PFD")) %>%
  rename("Team" = Tm.y)

head(df)

# Determine if there are any missing values
sum(is.na(df))
colSums(is.na(df))        # It appears that 1 player was not properly joined

# Drop this row
df <- df[rowSums(is.na(df)) == 0, ]
head(df)

length(df$Player) # 538 Players remaining 

# Drop a player from the dataframe because he has a strange stat line and is messing up the clutering algorithm
df <- df %>% filter(Player != 'Stanley Umude')

# For players who play multiple positions, give them the first listed one
df$Pos <- ifelse(nchar(df$Pos) > 3, substr(df$Pos, 1, 2), df$Pos)
unique(df$Pos)


