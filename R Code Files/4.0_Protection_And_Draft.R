############################################ 4.0 PLAYER PROTECTION & DRAFT ########################################### 
####################################  Logic & Function Definition, Method Testing ####################################  
######################################################################################################################

############################################ 4.1 PLAYER PROTECTION ########################################### 
###############################################################################################################
# PLAYER PROTECTION RULES & LOGIC 
# Mutating a new binary column, protected, indicating if a player will be protected by the team or not 
# Protection Sequence/Assumptions:
    ## 0. Teams will protect the maximum number of players allowed
    ## 1. Teams will protect their All-Stars 
    ## 2. If they are still under the maximum, teams will protect their starters (ie best player from each position)
    ## 3. If they are still under the maximum, teams will protect additional players from the top remaining clusters (ie next best players)

# Function to apply above protection logic to each team
protect <- function(data, n) {
  data$protected <- 0
  
  # 1. Protect players with cluster_id == 1 (All-Stars)
  if (sum(data$protected) < n) {
    data$protected <- ifelse(data$cluster_id == 1, 1, data$protected)
  }
  
  # 2. Protect players with team_rank == 1 if still under limit (Starters)
  if (sum(data$protected) < n) {
    indices_to_protect <- which(data$team_rank == 1 & data$protected == 0)
    needed_protections = min(n - sum(data$protected), length(indices_to_protect))
    data$protected[indices_to_protect[1:needed_protections]] <- 1
  }
  
  # 3. Randomly protect players from the next highest clusters until the limit is reached
  current_cluster_id <- 2
  while (sum(data$protected) < n && current_cluster_id <= max(data$cluster_id)) {
    available_indices <- which(data$cluster_id == current_cluster_id & data$protected == 0)
    if (length(available_indices) > 0) {
      selected_index <- sample(available_indices, 1)
      data$protected[selected_index] <- 1
      if (sum(data$protected) >= n) {
        break
      }
    }
    if (length(available_indices) == 0) {
      current_cluster_id <- current_cluster_id + 1
    }
  }
  return(data)
}


# Testing the function on one team
tor_protect <- tor %>%
  group_by(Team) %>%
  group_modify(~protect(.x, 7)) %>%
  ungroup()

# Looks good
# Apply the function to the whole dataset
n <- 6

protect_df <- teams %>%
  group_by(Team) %>%
  group_modify(~protect(.x, n)) %>%
  ungroup()

# Remove protected players, leaving only those available to be drafted 
players <- protect_df %>%
  filter(protected == 0) %>%
  # Rank players based on the same criteria, using cluster, WS, pts
  arrange(cluster_id, -PTS, -MIN, -WS) %>%
  mutate(player_rank = row_number()) %>%
  # Rank every player within their own team, regardless of position
  group_by(Team) %>%
  arrange(cluster_id, player_rank) %>%
  mutate(player_team_rank = row_number()) %>%
  ungroup()

########################################## 4.2 EXPANSION DRAFT SIMULATION #####################################
###############################################################################################################
# EXPANSION DRAFT RULES & LOGIC
# The two expansion teams (Team A, Team B) rotate picking 
# Teams will choose the best players remaining, regardless of position but within reason 
    # Best players are determined by cluster, then by player_rank, then by player_team_rank, with some randomness  
# Each expansion team can only select one player from each of the 30 teams
    # Once a player is chosen, all other players from his team are now unavailable to the same expansion team 
# Each expansion team will not select more than 4 players of the same position 
# Each expansion team must select between 14 and 29 players 
# The draft ends once either both teams have selected the max number of players (29), 
    # OR when at least 1 player has been chosen from each of the 30 teams 

simulate_expansion_draft <- function(df) {
  # Initialize data frames for Team A and Team B to store selected players
  team_a_picks <- data.frame(Player = character(), Pos = character(), Team = character(), stringsAsFactors = FALSE)
  team_b_picks <- data.frame(Player = character(), Pos = character(), Team = character(), stringsAsFactors = FALSE)
  
  # Initialize lists to keep track of team selections to prevent selecting more than one player from the same team
  team_a_selections <- character()
  team_b_selections <- character()
  
  # Initialize position counters to ensure no more than 4 players of the same position are chosen
  team_a_positions <- setNames(rep(0, 5), c("PG", "SG", "SF", "PF", "C"))
  team_b_positions <- setNames(rep(0, 5), c("PG", "SG", "SF", "PF", "C"))
  
  # Initialize position counters to ensure no more than 4 players of the same position are chosen
  position_limits <- list(Team_A = setNames(rep(0, 5), c("PG", "SG", "SF", "PF", "C")),
                          Team_B = setNames(rep(0, 5), c("PG", "SG", "SF", "PF", "C")))
  
  # Initialize a vector to count selections from each NBA team across both teams
  team_selection_counts <- rep(0, length(unique(df$Team)))
  names(team_selection_counts) <- unique(df$Team)
  
  # Coin flip to determine which team picks first
  current_team <- ifelse(rbinom(1, 1, 0.5) == 1, "Team A", "Team B")
  
  # Identify positions where the team is maxed out
  exceeded_positions <- names(position_limits[[current_team]][position_limits[[current_team]] >= 4])
  
  while(TRUE) {
    # Apply filters to ensure team and position limits are adhered to
    pre_filter_players <- df %>%
      filter(!(Team %in% c(team_a_selections, team_b_selections))) %>%  
      filter(if (length(exceeded_positions) > 0) !(Pos %in% exceeded_positions) else TRUE) %>%
      arrange(cluster_id)
    
    # Debug statement to ensure available players before further filtering
    print(paste("Filtered players available:", nrow(pre_filter_players)))
    
    # Ensure there are available players to select from
    if (nrow(pre_filter_players) == 0) {
      print("No available players left, breaking...")
      break
    }
    
    # Identify the top cluster and select a player
    top_cluster_id <- min(pre_filter_players$cluster_id)
    top_cluster_players <- filter(pre_filter_players, cluster_id == top_cluster_id)
    
    print(paste("Top cluster ID:", top_cluster_id, "with", nrow(top_cluster_players), "players available."))
    
    # Ensure there are players in the top cluster to choose from
    if (nrow(top_cluster_players) > 0) {
      player_to_pick <- sample_n(top_cluster_players, 1)
      
      # Update team picks and selections
      if (current_team == "Team A") {
        team_a_picks <- rbind(team_a_picks, player_to_pick)
        team_a_selections <- c(team_a_selections, player_to_pick$Team)
        position_limits$Team_A[player_to_pick$Pos] <- position_limits$Team_A[player_to_pick$Pos] + 1
      } else {
        team_b_picks <- rbind(team_b_picks, player_to_pick)
        team_b_selections <- c(team_b_selections, player_to_pick$Team)
        position_limits$Team_B[player_to_pick$Pos] <- position_limits$Team_B[player_to_pick$Pos] + 1
      }
      team_selection_counts[player_to_pick$Team] <- team_selection_counts[player_to_pick$Team] + 1
      
      # Rotate teams
      current_team <- ifelse(current_team == "Team A", "Team B", "Team A")
    } else {
      print("No players available in the top cluster.")
      break
    }
    
    # Check draft end conditions
    if ((length(team_a_selections) >= 14 && length(team_b_selections) >= 14 && all(team_selection_counts >= 1)) ||
        (length(team_a_selections) >= 29 || length(team_b_selections) >= 29)) {
      print("Draft end conditions met. Stopping the draft.")
      break
    }
  }
  
  return(list("Team A" = team_a_picks, "Team B" = team_b_picks))
}  

# Testing the draft simulation function on a subset of the data 
test <- sample_n(players, 60)  
draft_results <- simulate_expansion_draft(test)

# Applying the function to the entire dataset of available players 
draft_results <- simulate_expansion_draft(players)
team_a <- draft_results$'Team A'
team_b <- draft_results$'Team B'

# Update the Team column to reflect the respective draft teams
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

head(df)

