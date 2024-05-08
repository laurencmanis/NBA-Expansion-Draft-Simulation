######################################## 6.0 FULL EXPANSION DRAFT SIMULATION ##################################
###############################################################################################################
library(purrr)
library(ggplot2)
library(ggforce)

######################################## 6.1 FULL PROCESS DEFINITION ##################################
########################################################################################################
# Define a process to run the entire expansion draft process for each possible value of protected players 
teams_clone <- teams  

full_sim <- function(n) {
  teams_clone <- teams  
  set.seed(123)
  
  # Simulate player protection for existing teams
  protect_df <- teams_clone %>%
    group_by(Team) %>%
    group_modify(~protect(.x, n)) %>%
    ungroup()
  
  players <- protect_df %>%
    filter(protected == 0) %>%
    arrange(cluster_id,-PTS,-WS) %>%
    mutate(player_rank = row_number()) %>%
    group_by(Team) %>%
    arrange(cluster_id, player_rank) %>%
    mutate(player_team_rank = row_number()) %>%
    ungroup()
  
  # Simulate expansion draft
  draft_results <- simulate_expansion_draft(players)
  team_a <- draft_results$'Team A'
  team_b <- draft_results$'Team B'
  
  team_a$Team <- "Team A"
  team_b$Team <- "Team B"
  
  # Update selected players' team in original df
  team_a_player_names <- team_a$Player
  team_b_player_names <- team_b$Player
  
  indices_team_a <- which(df$Player %in% team_a_player_names)
  indices_team_b <- which(df$Player %in% team_b_player_names)
  
  # Update the Team column for selected players
  df$Team[indices_team_a] <- "Team A"
  df$Team[indices_team_b] <- "Team B"
  
  player_det <- df[, c("Player","Team","cluster_id")]
  
  # Compute new team stats 
  team_box <- calculate_team_stats(df)
  predicted_pie <- predict(stepwise, newdata = team_box)
  team_box$predicted_pie = round(predicted_pie, 4)
  
  # Project team wins 
  total_predicted_pie = sum(team_box$predicted_pie, na.rm = TRUE)
  team_box <- team_box %>%
    mutate(pie_pred = predicted_pie / total_predicted_pie,
           proj_wins = round(pie_pred * (32 * 41), 0))
  
  team_a_proj_wins <- data.frame(proj_wins = team_box %>% filter(Team == 'Team A') %>% pull(proj_wins))
  team_b_proj_wins <- data.frame(proj_wins = team_box %>% filter(Team == 'Team B') %>% pull(proj_wins))
  
  win_range <- max(team_box$proj_wins) - min(team_box$proj_wins)
  
  return(list(
    n = n, 
    win_range = win_range, 
    mean_proj_wins = mean(team_box$proj_wins, na.rm = TRUE), 
    team_a_wins = team_a_proj_wins, 
    team_b_wins = team_b_proj_wins,  
    team_stats = team_box$proj_wins, 
    player_details = player_det
  ))
}

results <- list()
for (n in 6:11) {
  results[[(n)]] <- full_sim(n)
}

# Extract elements from results to create a final view 
results_df <- map_dfr(results, ~data.frame(
  n = .x$n,
  win_range = .x$win_range,
  mean_proj_wins = .x$mean_proj_wins,
  team_a_wins = .x$team_a_wins,
  team_b_wins = .x$team_b_wins
))


########################################### 6.2 ACTUAL SIMULATION ######################################
########################################################################################################

# Initialize empty dataframes 
results_df <- data.frame()
all_wins_df <- data.frame()
player_details_df <- data.frame()

# Define the range of n and number of simulations
n_values <- 6:11
sims <- 1000

# Loop over each n and run the simulations
for (n in n_values) {
  for (i in 1:sims) {
    sim_result <- full_sim(n)
    results_df <- rbind(results_df, data.frame(
      n = sim_result$n,
      win_range = sim_result$win_range,
      mean_proj_wins = sim_result$mean_proj_wins,
      team_a_wins = sim_result$team_a_wins$proj_wins, 
      team_b_wins = sim_result$team_b_wins$proj_wins   
    ))
    
    temp_df <- data.frame(team_stats = sim_result$team_stats)
    temp_df$n = n 
    all_wins_df <- rbind(all_wins_df, temp_df)
 
    player_temp_df <- sim_result$player_details
    player_temp_df$n = n
    player_temp_df$sim_id = i
    player_details_df <- rbind(player_details_df, player_temp_df)
  }
}

# ...Takes a while to run... 

results_df
all_wins_df
player_details_df

########################################## 6.3 RESULTS ANALYSIS #######################################
#######################################################################################################

results_agg <- results_df %>%
  group_by(n) %>%
  summarise(
    range = mean(win_range),
    mean_wins = mean(mean_proj_wins),
    min_mean_proj_wins = min(mean_proj_wins, na.rm = TRUE),
    max_mean_proj_wins = max(mean_proj_wins, na.rm = TRUE),
    median_wins = median(mean_proj_wins),
    team_a_wins = round(mean(team_a_wins)),
    team_b_wins = round(mean(team_b_wins)),
    team_a_diff = team_a_wins - mean_wins,
    team_b_diff = team_b_wins - mean_wins,
    sum_diff = team_a_diff + team_b_diff
  ) 

results_agg <- results_agg %>% arrange(-sum_diff) # As the number of players protected increases, the projected wins for the expansion teams decrease 
# With more players protected, the quality the available talent pool is lower, leading to weaker rosters, fewer wins

# Ideally, we want the tightest distribution of projected wins, and the scenario that gives the expansion teams the best chance to compete and achieve the mean number of wins 

# The range will always be quite wide: ~42 win difference between the top and bottom teams in the league 
# Ie, there will always be a team like the 2023-24 Pistons or Wizards, who record fewer than 20 wins 
# There will also always be teams who dominate in the regular season, like the 2023-23 Celtics who win 60+ games

# Determine mean projected wins per team
sim_wins_df <- all_wins_df %>% 
  group_by(n) %>%
  mutate(id = ((row_number() - 1) %/% 32) + 1) %>%
  ungroup()

# Determine mean number of wins projected for each team (32) across all trials of the same N
wide_df <- sim_wins_df %>%
  group_by(n, id) %>%
  mutate(col = paste("Team", row_number(), sep = "_")) %>%
  pivot_wider(names_from = col, values_from = team_stats)

n6_wins <- wide_df %>% filter(n==6) 

# Loop through each column starting from the third to the 34th
mean_team_wins_6 <- list()  
for (i in 3:34) {
  current_mean <- mean(n6_wins[[i]], na.rm = TRUE)  
  mean_team_wins_6 <- c(mean_team_wins_6, current_mean) 
}
names(mean_team_wins_6) <- colnames(n6_wins)[3:34]

# Repeat for every N...
n7_wins <- wide_df %>% filter(n==7) 
mean_team_wins_7 <- list()  
for (i in 3:34) {
  current_mean <- mean(n7_wins[[i]], na.rm = TRUE)  
  mean_team_wins_7 <- c(mean_team_wins_7, current_mean) 
}
names(mean_team_wins_7) <- colnames(n7_wins)[3:34]

n8_wins <- wide_df %>% filter(n==8) 
mean_team_wins_8 <- list()  
for (i in 3:34) {
  current_mean <- mean(n8_wins[[i]], na.rm = TRUE)  
  mean_team_wins_8 <- c(mean_team_wins_8, current_mean) 
}
names(mean_team_wins_8) <- colnames(n8_wins)[3:34]

n9_wins <- wide_df %>% filter(n==9) 
mean_team_wins_9 <- list()  
for (i in 3:34) {
  current_mean <- mean(n9_wins[[i]], na.rm = TRUE)  
  mean_team_wins_9 <- c(mean_team_wins_9, current_mean) 
}
names(mean_team_wins_9) <- colnames(n9_wins)[3:34]

n10_wins <- wide_df %>% filter(n==10) 
mean_team_wins_10 <- list()  
for (i in 3:34) {
  current_mean <- mean(n10_wins[[i]], na.rm = TRUE)  
  mean_team_wins_10 <- c(mean_team_wins_10, current_mean) 
}
names(mean_team_wins_10) <- colnames(n10_wins)[3:34]

n11_wins <- wide_df %>% filter(n==11) 
mean_team_wins_11 <- list()  
for (i in 3:34) {
  current_mean <- mean(n11_wins[[i]], na.rm = TRUE)  
  mean_team_wins_11 <- c(mean_team_wins_11, current_mean) 
}
names(mean_team_wins_11) <- colnames(n11_wins)[3:34]

# Join all of the above into one larger dataframe 
team_wins_df <- data.frame(
  wins_6 = unlist(mean_team_wins_6),
  wins_7 = unlist(mean_team_wins_7),
  wins_8 = unlist(mean_team_wins_8),
  wins_9 = unlist(mean_team_wins_9),
  wins_10 = unlist(mean_team_wins_10),
  wins_11 = unlist(mean_team_wins_11)
)

# Reshape the dataframe
team_wins_long <- team_wins_df %>%
  pivot_longer(cols = everything(),names_to = c(".value", "n"),names_sep = "_"
  ) %>%
  mutate(n = as.integer(n)) %>%
  arrange(n)

team_wins_summ <- team_wins_long %>%
  group_by(n) %>%
  summarise(
    stdev = sd(wins),
    minw = min(wins),
    maxw = max(wins),
    under_41 = sum(wins <= 41),
    over_41 = sum(wins > 41),
    pct_over = over_41 / (over_41 + under_41),
    w_under_25 = sum(wins < 25),
    w_over_55 = sum(wins >= 55),
    stdev_1 = sum(wins > 41 - stdev & wins < 41 + stdev)
  )  

# Determine distribution of players by cluster for each team under each N
player_summ <- player_details_df %>% 
  replace(is.na(.), 0) %>%
  group_by(Team, n, sim_id, cluster_id) %>%
  summarise(player_count = n_distinct(Player)) %>%
  group_by(Team, n, cluster_id) %>%
  summarise(cluster_players = mean(player_count))

player_summ_wide <- player_summ %>%
  pivot_wider(names_from = cluster_id, values_from = cluster_players, names_prefix = 'cluster') %>%
  replace(is.na(.), 0) %>%
  mutate(
    c1_2 = sum(cluster1 + cluster2),
    c7_8 = sum(cluster7 + cluster8),
    c3_4_5_6 = sum(cluster3 + cluster4 + cluster5 + cluster6),
  )

player_summ_agg <- player_summ_wide %>%
  group_by(n) %>%
  summarise(
    max_c1 = max(cluster1),
    mac_c1_2 = max(c1_2),
    max_c7_8 = max(c7_8),
    max_c8 = max(cluster8),
    min_c1_2 = min(c1_2),
    mult_c1 = n_distinct(case_when(cluster1 > 1 ~ Team)),
    mult_c1_2 = n_distinct(case_when(c1_2 > 3 ~ Team)),
    mult_c7_8 = n_distinct(case_when(c7_8 > 3 ~ Team)),
    mult_c8 = n_distinct(case_when(cluster8 > 1 ~ Team))
)

player_cluster_summ <- player_summ %>%
  group_by(n, cluster_id) %>%
  summarise(mean_cluster_players = round(mean(cluster_players)))

player_cluster_summ %>% pivot_wider(names_from = cluster_id, values_from = mean_cluster_players)

########################################## 6.3.1 VISUALIZATIONS #######################################
#######################################################################################################

team_wins_long$n <- as.factor(team_wins_long$n)

# Create a box plot to visualize the distribution of prjoected wins for each n
ggplot(team_wins_long, aes(x = n, y = wins, fill = n)) + 
  geom_boxplot() +
  labs(
    x = "N (Number of Protected Players)",
    y = "Mean Wins",
    title = "Distribution of Mean Wins by N"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_manual(values = rb_palette)

# The Median for wins is consistently around 41 games for each N
# N=8 has the tightest box for the IQR - more teams are falling within the 25th and 75th percentiles than in any other scenario
# N=7 or N=9 appears to have the shortest whiskers, though by a small margin 
# N=11 appears to have the highest median wins 

# Plot the distribution of wins for each N
ggplot(team_wins_long, aes(x = wins)) +
  geom_histogram(aes(y = ..density..), bins = 8, fill = "#F1C3CB", color = "#E48897") +  
  geom_density(aes(y = ..density..), color = "#C8102E", linewidth = 0.9) + 
  facet_wrap(~ n, scales = "fixed") +  
  labs(
    x = "Mean Wins",
    y = "Density",
    title = "Histogram and Density of Wins for Each N"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_blank(), 
    strip.text = element_text(face = "bold", size=14),  
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )


# Plot the distribution of Mean Players by Cluster for each N
ggplot(player_cluster_summ, aes(x = as.factor(cluster_id), y = mean_cluster_players, fill = as.factor(cluster_id))) +
  geom_col(fill = "#C7D0E2", color = "#1D428A") + 
  facet_wrap(~ n, scales = "fixed") + 
  labs(
    x = "Cluster ID",
    y = "Mean Num. Players",
    title = "Average Players Per Cluster Per Team for Each N"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) 


# Plot projected wins for each expansion team for each N
long_dat <- results_agg %>%
  pivot_longer(cols = c(team_a_wins, team_b_wins),names_to = "team",values_to = "wins"
  )

ggplot(long_dat, aes(x = factor(n), y = wins, fill = team)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) + 
  labs(
    x = "N (Number of Protected Players)",
    y = "Projected Wins",
    title = "Projected Wins for Each Expansion Team by N"
  ) +
  geom_hline(yintercept = 41, color = "black", size = 0.2) + 
  scale_fill_manual(
    values = c("team_a_wins" = "#1D428A", "team_b_wins" = "#C8102E"),
    labels = c("Team A Wins", "Team B Wins"))+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 12) 
  )

# Available players from the top 4 clusters for each N
avail_players <- player_details_df %>% 
  filter(Team == 'Team A' | Team == 'Team B', cluster_id < 5) %>% 
  group_by(n) %>% 
  summarise(players_sum = n_distinct(Player))

ggplot(avail_players, aes(x = factor(n), y = players_sum)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), fill='#8EA1C5', color='#1D428A') + 
  labs(
    x = "N (Number of Protected Players)",
    y = "Available Players",
    title = "Total Available Players From the Top 4 Clusters"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none"
  )


########################## 6.4 DETERMINING OPTIMAL NUMBER OF PROTECTED PLAYERS ####################
###################################################################################################

# Just for fun, let's see what the expansion team rosters could look like under N=7
n <- 7

protect_df <- teams_clone %>%
  group_by(Team) %>%
  group_modify(~protect(.x, n)) %>%
  ungroup()

players <- protect_df %>%
  filter(protected == 0) %>%
  arrange(cluster_id, -PTS, -WS) %>%
  ungroup()

# Simulate expansion draft
draft_results <- simulate_expansion_draft(players)

teama <- draft_results$`Team A`     # Notable Players: Nicolas Batum, Aaron Nesmith, Saddiq Bey, Herb Jones
teamb <- draft_results$`Team B`     # Notable Players: Quentin Grimes, Al Horford, Coby White, Tyus Jones

teama %>% group_by(cluster_id) %>% summarise(players = n())   # Min Cluster = 4, Cluster 4 Players: 6
teamb %>% group_by(cluster_id) %>% summarise(players = n())   # Min Cluster = 4, Cluster 4 Players: 5
