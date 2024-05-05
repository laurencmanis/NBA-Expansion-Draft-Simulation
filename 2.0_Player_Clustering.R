############################################### 02. PLAYER CLUSTERING ############################################### 
#####################################################################################################################

# Select columns to cluster by 
cols <- df %>% select(-c("Player","Team","Rk","Pos","W","L","W_PCT"))

# Scale columns
scaled_cols <- as.data.frame(lapply(cols, function(x) scale(x)))

# Set seed for reproducibility
set.seed(123)

# Determine optimal number of player clusters 
k_clusters <- function(k){
  kmeans(scaled_cols,k,iter.max=100,nstart = 100)$tot.withinss
}

k_values <- 1:10

vals <- map_dbl(k_values,k_clusters)

# Plot K vs WCSS
plot(k_values,vals,type='b',xlab='number of clusters',ylab='total sum of squares')

# Create clusters 
clusters <- kmeans(scaled_cols, 8, iter.max = 50, nstart = 20)
df$cluster <- clusters$cluster

# Examine clusters
cluster_df <- df %>%
  group_by(cluster) %>%
  summarise(pts = mean(PTS),
            mp = mean(MIN),
            ast = mean(AST),
            reb = mean(REB),
            fg_pct = mean(FG_PCT),
            ft_pct = mean(FT_PCT),
            vorp = mean(VORP),
            ws = mean(WS),
            ts = mean(TS.),
            per = mean(PER),
            usg = mean(USG.))
            
cluster_df

# Cluster 1: All-stars with top-level contributions; high scoring (26.1 pts), assists (6.21), and rebounds (6.70); elite in VORP (4.24) and win shares (8.48), with strong PER (23.9) and usage (29.9).
    # Player Examples: Giannis Antetokounmpo, Steph Curry, Jalen Brunson
# Cluster 2: Minimal contributors, largely non-rotation players; limited minutes (4.09 mp) and low stats across the board; negative VORP (-0.0571) and win shares (-0.0714), extremely low efficiency (TS of 0.293).
    # Player Examples: Leandro Bolmaro, Deonte Burton, Joe Wieskamp
# Cluster 3: Key rotation players; solid contribution with 11.0 pts, 4.19 reb, and 3.03 ast; good efficiency and positive impact in VORP (0.673) and win shares (3.36).
    # Player Examples: Jose Alvarado, Cole Anthony, OG Anunoby
# Cluster 4: Limited minutes role players; contribute on a smaller scale with 2.96 pts and 1.56 reb; low overall impact with slightly negative VORP (-0.188).
    # Player Examples: Thanasis Antetokounmpo, Ryan Arcidiacono, Dalano Banton
# Cluster 5: Bench contributors; moderate playing time with 6.45 pts and 2.28 reb; slightly below neutral VORP (-0.0243) and modest win shares (1.21).
    # Player Examples: Ochai Agbaji, MarJon Beauchamp, Nickeil Alexander-Walker
# Cluster 6: Effective role players/Starters; notable for rebounding (8.93 reb) and scoring (13.3 pts) with high field goal percentage (0.601); strong contributions reflected in VORP (1.73) and win shares (6.38).
    # Player Examples: Steven Adams, Bam Adebayo, Deandre Ayton
# Cluster 7: High-performance players/Starters; score 19.5 pts, 4.51 ast, and 4.69 reb; solid efficiency with a good VORP (1.04) and win shares (3.06).
    # Player Examples: LaMelo Ball, Paolo Banchero, RJ Barrett
# Cluster 8: Solid rotation players; contribute 5.32 pts and 3.61 reb in limited minutes; efficient with a field goal percentage of 0.576 and good true shooting (0.626).
    # Player Examples: Precious Achiuwa, Mo Bamba, Thaddeus Young

cluster_df <- cluster_df %>% arrange(-pts, -ws, -mp) %>% mutate(cluster_id=row_number())

# Visualizations of contributions by cluster
plot(cluster_df$cluster_id, cluster_df$ws, main = "Win Shares by Cluster", xlab = "Cluster", ylab = "WS") 
plot(cluster_df$cluster_id, cluster_df$pts, main = "PPG by Cluster", xlab = "Cluster", ylab = "Points") 

# Add ranked cluster_id to main df 
df <- df %>%
  mutate(cluster_id = case_when(
    cluster == 2 ~ 1, 
    cluster == 7 ~ 2,
    cluster == 1 ~ 3,
    cluster == 8 ~ 4,
    cluster == 4 ~ 5,
    cluster == 3 ~ 6,
    cluster == 5 ~ 7,
    cluster == 6 ~ 8))

# Players per cluster 
table(df$cluster_id)
prop.table(table(df$cluster_id))

# Distribution of Players per cluster 
hist(df$cluster_id, main = "Distribution of Players by Cluster", xlab = "Cluster", freq = FALSE, col = "#1D428A")  

# Organize data into teams, rank players by cluster within each position
teams <- df %>%
  select(Team, Player, Pos, cluster_id, PTS, MIN, WS) %>%
  group_by(Team, Pos) %>%
  arrange(cluster_id, -PTS, -MIN, -WS) %>% 
  mutate(team_rank = row_number())

# Example 
tor <- teams %>% filter(Team == 'TOR')

# Drop any UFAs from the dataset as they cannot be protected nor selected - FA Data retrieved from HoopsHype
fa <- read.csv('/Users/laurenmanis/Desktop/NBA Draft Project/free_agents.csv') %>% rename("Player" = PLAYER)

teams <- teams %>%
  anti_join(fa, by='Player')

