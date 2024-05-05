################################# 5.0 NEW TEAM PER-GAME STATS & PROJECTED % OF WINS ###########################
###############################################################################################################

########################################## 5.1 AGGREGATING TEAM STATS #####################################
###########################################################################################################
# Determine/project players minutes per game, stats per minute 
box <- df %>%
  group_by(Team) %>%
  arrange(cluster_id, -G, -MIN) %>%
  slice_head(n = 12) %>%
  mutate(total_minutes = sum(MIN),
         rnum = row_number()) %>%
  # Assign starters their full minutes 
  mutate(starter_mins = ifelse(rnum < 6, MIN, 0),
         total_starter_mins = sum(starter_mins)) %>%
  # Compute remaining minutes for the game
  mutate(remaining_mins = ifelse(rnum < 6, 0, 240 - total_starter_mins),
         # Calculate percentage of minutes each non-starter plays out of non-starter total minutes
         percent_of_non_starter_mins = ifelse(rnum >= 6, MIN / sum(MIN[rnum >= 6]), 0),
         # Scale non-starter minutes to fit into the remaining game time
         scaled_MP = ifelse(rnum < 6, MIN, percent_of_non_starter_mins * remaining_mins),
         mins_pct = scaled_MP / 48) %>%
  ungroup()

head(box)
view(box)
colnames(box)

# Convert all box score metrics to be per minute
convert_cols <- c('FGM','FGA','FG3M','FG3A','FTM','FTA','OREB','DREB','REB','AST','TOV','STL','BLK','BLKA','PF','PTS')

for(col in convert_cols) {
  box[[col]] <- box[[col]] / box$MIN
}

# Convert metrics to per 48 minutes 
for(col in convert_cols) {
  box[[col]] <- box[[col]] * 48
}

# Multiply each metric by % of 48 minutes player is projected to play (mins_pct)
scale_cols <- c('FGM','FGA','FG3M','FG3A','FTM','FTA','OREB','DREB','REB','AST','TOV','STL','BLK','BLKA','PF','PTS','WS.48')

for(col in scale_cols) {
  box[[col]] <- box[[col]] * box$mins_pct
}

head(box)

# Select box score metrics only
box_cols <- c('Player', 'Team', 'Pos', 'G', 'cluster_id', 'scaled_MP', 'mins_pct', 'FGM', 'FGA', 'FG3M', 'FG3A', 
              'FTM', 'FTA','OREB', 'DREB', 'REB', 'AST', 'TOV', 'STL', 'BLK', 'BLKA', 'PF', 'PTS', 'PLUS_MINUS', 'WS.48') 

box <- box[, box_cols]
view(box)
# Sense Check: Do these numbers make sense?
# Highest scorer is Embiid w 33 PPG - Makes sense, LeBron with almost 29 PPG - Looks reasonable
# Jokic has 12 rebounds per game - Seems plausible, Austin Reaves with 8 PPG - Makes sense  

# Aggregate stats to team-level 
team_box <- box %>%
  group_by(Team) %>%
  summarise(across(c(FGM, FGA, FG3M, FG3A, FTM, FTA, OREB, DREB, REB, AST, TOV, STL, BLK, BLKA, PF, PTS, PLUS_MINUS, WS.48),
                   sum, na.rm = TRUE)) 

head(team_box)

# Sense check - do these numbers look plausible 
mean(team_box$PTS)    # Avg PPG = 119, ranges from 97 to 141... 141 is really high but we'll keep it for now (Hint: its Philly with Embiid, Harden, and Maxey all with high PPG)
min(team_box$PTS)
max(team_box$PTS)

mean(team_box$FG3M)   # Avg 3 pointers = 13

mean(team_box$REB)    # Avg REB = 44

# All of these metrics make sense for a team in an NBA game 

# Convert Plus/Minus to be per 82 games 
team_box <- team_box %>% mutate(PM = as.numeric(PLUS_MINUS) / 82)  

# Compute shooting percentages 
team_box <- team_box %>%
  mutate(FG_PCT = FGM / FGA,
         FG3_PCT = FG3M / FG3A,
         FT_PCT = FTM / FTA)

# Rank each team in the relevant stat categories for pie % projections 
# PLUS_MINUS_RANK, FG_PCT_RANK, TOV_RANK, DREB_RANK, BLKA_RANK, STL_RANK

team_box <- team_box %>%
  mutate(
    PLUS_MINUS_RANK = rank(-PM, ties.method = "first"),
    FG_PCT_RANK = rank(-FG_PCT, ties.method = "first"),
    TOV_RANK = rank(-TOV, ties.method = "first"),
    DREB_RANK = rank(-DREB, ties.method = "first"),
    BLKA_RANK = rank(-BLKA, ties.method = "first"),
    STL_RANK = rank(-STL, ties.method = "first")
  )

head(team_box)

################################### 5.1.1 AGGREGATING TEAM STATS FUNCTION #################################
###########################################################################################################

# Now that we have ensured all of the logic and aggregation looks right, let's create one large function to implement all of the above 
calculate_team_stats <- function(df) {
  # Preprocess and compute player-level stats
  box <- df %>%
    group_by(Team) %>%
    arrange(cluster_id, -G, -MIN) %>%
    slice_head(n = 12) %>%
    mutate(total_minutes = sum(MIN),
           rnum = row_number()) %>%
    # Assign starters their full minutes 
    mutate(starter_mins = ifelse(rnum < 6, MIN, 0),
           total_starter_mins = sum(starter_mins)) %>%
    # Compute remaining minutes for the game
    mutate(remaining_mins = ifelse(rnum < 6, 0, 240 - total_starter_mins),
           # Calculate percentage of minutes each non-starter plays out of non-starter total minutes
           percent_of_non_starter_mins = ifelse(rnum >= 6, MIN / sum(MIN[rnum >= 6]), 0),
           # Scale non-starter minutes to fit into the remaining game time
           scaled_MP = ifelse(rnum < 6, MIN, percent_of_non_starter_mins * remaining_mins),
           mins_pct = scaled_MP / 48) %>%
    ungroup()
  
  # Convert all box score metrics to be per minute and then to per 48 minutes
  convert_cols <- c('FGM','FGA','FG3M','FG3A','FTM','FTA','OREB','DREB','REB','AST','TOV','STL','BLK','BLKA','PF','PTS')
  for (col in convert_cols) {
    box[[col]] <- (box[[col]] / box$MIN) * 48
  }
  
  # Multiply each metric by % of 48 minutes player is projected to play (mins_pct)
  scale_cols <- c('FGM','FGA','FG3M','FG3A','FTM','FTA','OREB','DREB','REB','AST','TOV','STL','BLK','BLKA','PF','PTS','WS')
  for (col in scale_cols) {
    box[[col]] <- box[[col]] * box$mins_pct
  }
  
  # Select box score metrics only
  box_cols <- c('Player', 'Team', 'Pos', 'G', 'cluster_id', 'scaled_MP', 'mins_pct', 'FGM', 'FGA', 'FG3M', 'FG3A', 
                'FTM', 'FTA','OREB', 'DREB', 'REB', 'AST', 'TOV', 'STL', 'BLK', 'BLKA', 'PF', 'PTS', 'PLUS_MINUS', 'WS') 
  box <- box[, box_cols]
  
  # Aggregate stats to team-level and compute additional metrics
  team_box <- box %>%
    group_by(Team) %>%
    summarise(across(c(FGM, FGA, FG3M, FG3A, FTM, FTA, OREB, DREB, REB, AST, TOV, STL, BLK, BLKA, PF, PTS, PLUS_MINUS, WS),
                     sum, na.rm = TRUE),
              PM = sum(PLUS_MINUS, na.rm = TRUE) / 82,
              FG_PCT = sum(FGM, na.rm = TRUE) / sum(FGA, na.rm = TRUE),
              FG3_PCT = sum(FG3M, na.rm = TRUE) / sum(FG3A, na.rm = TRUE),
              FT_PCT = sum(FTM, na.rm = TRUE) / sum(FTA, na.rm = TRUE),
              .groups = 'drop') %>%
    mutate(
      PLUS_MINUS_RANK = rank(-PM, ties.method = "first"),
      FG_PCT_RANK = rank(-FG_PCT, ties.method = "first"),
      TOV_RANK = rank(TOV, ties.method = "first"),
      DREB_RANK = rank(-DREB, ties.method = "first"),
      BLKA_RANK = rank(-BLKA, ties.method = "first"),
      STL_RANK = rank(-STL, ties.method = "first")
    )
  
  return(team_box)
}

# Run the function
team_box <- calculate_team_stats(df)

######################################## 5.2 TEAM WIN PROJECTIONS #####################################
#######################################################################################################

# Project % of total available wins (pie %) for each post-expansion team 
predicted_pie <- predict(stepwise, newdata = team_box)
team_box$predicted_pie = round(predicted_pie, 4)

total_predicted_pie = sum(team_box$predicted_pie, na.rm = TRUE)  # Sums to 1.024, slightly too high

# Scale the predictions to sum to 1 
team_box <- team_box %>% mutate(pie_pred = predicted_pie / total_predicted_pie)
sum(team_box$pie_pred)  # Now it sums to 1

# Sense check - do the predictions make sense?
view(team_box)

mean(team_box$pie_pred)   # Mean pie pct = 3.13% -- This looks correct, mean should be slightly lower than in prior seasons 0.333 because of the 2 additional teams
min(team_box$pie_pred)    # Min pie pct = 1.55%
max(team_box$pie_pred)    # Min pie pct = 4.67%

team_box %>% arrange(-pie_pred) %>% slice_head(n=1)   # BOS is the team with the highest % of the pie, 123 PPG, 49 REB, Multiple Cluster 1 Players - this makes sense 
team_box %>% arrange(pie_pred) %>% slice_head(n=1)    # SAS is the team with the lowest % of the pie, 118 PPG, -PLUS MINUS, and let's be honest, they're just not a good team right now 

#################################### 5.3 MEASURING COMPETITIVE BALANCE ################################
#######################################################################################################

# Convert predicted pie % to predicted number of wins 
pie_total <- 32 * 41

team_box <- team_box %>% mutate(proj_wins = round(pie_pred * pie_total,0))

mean(team_box$proj_wins)    # Mean wins = 41 - perfect
min(team_box$proj_wins)     # Min wins = 20 - SAS 
max(team_box$proj_wins)     # Max wins = 61 - BOS

win_range <- max(team_box$proj_wins) - min(team_box$proj_wins)   
win_range

# Visualize distribution of wins                                  # In order to maximize competition, we want the tightest distribution, ie the least variance, where the largest portion of teams are at/around the mean (41) 
# Histogram
hist(team_box$proj_wins, main = "Distribution of Wins",
     xlab = "Projected Wins", freq = FALSE, col = "#5671A7")  
dens <- density(team_box$proj_wins)
lines(dens, col="black", lwd=2)  

# Pie chart
total_wins <- sum(team_box$proj_wins)
percent_labels <- paste(team_box$Team, "-", round(team_box$proj_wins / total_wins * 100, 1), "%")

rb_palette <- c("#1D428A","#5671A7","#C7D0E2","#F1C3CB","#D64C62","#C8102E")

pie(team_box$proj_wins, labels = team_box$Team, main = "Projected Wins by Team", col = rb_palette)



