############################################## 3.0. PROJECTING TEAM WINS ############################################# 
#####################################################################################################################
library(hoopR)
library(glmnet)
library(caret)
library(leaps)
library(tidymodels)
library(rpart)
library(stats)
library(MASS)

# Create a dataframe with team stats over the last 10 seasons 
full_df <- as.data.frame(nba_leaguedashteamstats(season = '2022-23')) %>% mutate(season = 2022) %>%
  rbind(as.data.frame(nba_leaguedashteamstats(season = '2021-22')) %>% mutate(season = 2021)) %>%
  rbind(as.data.frame(nba_leaguedashteamstats(season = '2020-21')) %>% mutate(season = 2020)) %>%
  rbind(as.data.frame(nba_leaguedashteamstats(season = '2019-20')) %>% mutate(season = 2019)) %>%
  rbind(as.data.frame(nba_leaguedashteamstats(season = '2018-19')) %>% mutate(season = 2018)) %>%
  rbind(as.data.frame(nba_leaguedashteamstats(season = '2017-18')) %>% mutate(season = 2017)) %>%
  rbind(as.data.frame(nba_leaguedashteamstats(season = '2016-17')) %>% mutate(season = 2016)) %>%
  rbind(as.data.frame(nba_leaguedashteamstats(season = '2015-16')) %>% mutate(season = 2015)) %>%
  rbind(as.data.frame(nba_leaguedashteamstats(season = '2014-15')) %>% mutate(season = 2014)) %>%
  rbind(as.data.frame(nba_leaguedashteamstats(season = '2013-14')) %>% mutate(season = 2013))

head(full_df)

# Re-format column names 
colnames(full_df) <- gsub("LeagueDashTeamStats.","",colnames(full_df))

# Convert numeric columns to type numeric
full_df <- full_df %>% 
  mutate(across(-TEAM_NAME, as.numeric))

# Create a new column for percent of total available wins throughout the regular season = pie_pct 
full_df <- full_df %>% mutate(pie_pct = W / ((GP / 2) * 30))

# Select Columns for EDA & modeling 
## For modeling, we will use team rankings in each major stat area 
## We will do this because when applying this model to project wins for teams-post expansion, those teams will not include 
## Free Agents, many of whom are major contributors, so a model based on actual per-game metrics would likely 
## under-predict win values for teams because the inputs would be unusually low.
## Using relative rankings should solve this issue. Also, projecting a "piece of the pie" ie, percent of total available wins
## will account for the fact that the number of games will change (increase) after the expansion occurs. 

dat <- full_df[, 34:56]

# Initial EDA - What features may be predictive of wins / piece of the pie 
mean(dat$pie_pct)     # 0.033 - Teams at 0.500 will get 0.0333 pct of the avaialble wins (= 41 in 82 game seasons with 30 teams)
median(dat$pie_pct)   # 0.034

dat %>% group_by(season) %>% summarise(mean_pct = mean(pie_pct), min_wins = min(pie_pct), max_wins=max(pie_pct))

# Team with the most wins generally gets around 0.05 percent, ~61 wins 
# Team with the fewest wins generally gets around 0.02 percent, ~25 wins 

# Explore relationships between features and outcome of interest, percent of total available wins 
for (col in colnames(dat)) {
  if (col != "pie_pct") {  
    plot(dat[[col]], dat$pie_pct, main = paste("Pie Pct &", col), xlab = col, ylab = "Pie Pct")
  }
}

# Columns with clear negative linear relationships with pie_pct (ie columns where as the team ranks better, the pie_pct increases):
# PLUS_MINUS_RANK, FG_PCT_RANK

# Columns with potential negative linear relationships with pie_pct (ie columns where as the team ranks better, the pie_pct increases):
# PTS_RANK, BLKA_RANK, AST_RANK, REB_RANK, DREB_RANK, FG3_PCT_RANK, FGM_RANK

# There are several variables that seem to have strong linear relationships and potentially be predictive of pie_pct
# Maybe this can be modeled using Linear Regression

#################################### 3.1 LINEAR REGRESSION ####################################  
###############################################################################################  
# Define a baseline intercept-only model
intercept_only <- lm(pie_pct ~ 1, data=dat)
summary(intercept_only)$r.squared

# Define model with all predictors
all <- lm(pie_pct ~ ., data=dat)
all_rsq <- summary(all)$r.squared   #  0.9148
all_rsq

# R-Square is pretty high BUT Dataset has 22 features, which is a lot, and most aren't statistically significant
# We probably don't need to use all of them to get a similar, so let's explore some models with feature-selection techniques 

#################### 3.2 STEPWISE LINEAR REGRESSION ####################  
# Using stepAIC for stepwise regression
stepwise <- stepAIC(intercept_only, scope=list(lower=intercept_only, upper=all), direction="both")
summary(stepwise)

step_rsq <- summary(stepwise)$r.squared   # 0.9113 
step_rsq

# The stepwise regression model only uses 6 features, to get a similar R-square, where all variables are statisticallyt significant 
## Features: PLUS_MINUS_RANK, FG_PCT_RANK, TOV_RANK, DREB_RANK, BLKA_RANK, STL_RANK
### Does this model make sense from a basketball perspective: Sure - it seems to consider defense as most important in winning games 

# Extract the formula from the fitted model
step_model <- formula(stepwise)

#################### 3.3 LASSO REGRESSION ####################  
y <- dat$pie_pct
x <- data.matrix(dat[, 1:22])

# Perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

# Find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min

# Fit model using best lambda
lasso <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(lasso)

# Lasso features (9 TOTAL): FG_PCT_RANK, FG3_PCT_RANK, FT_PCT_RANK, OREB_RANK, DREB_RANK, TOV_RANK, STL_RANK, BLKA_RANK, PLUS_MINUS_RANK

y_pred <- predict(lasso, s = best_lambda, newx = x)

# Compute R-squared
sst <- sum((y - mean(y))^2)
sse <- sum((y_pred - y)^2)
lasso_rsq <- 1 - sse/sst  # 0.9111
lasso_rsq

# R-Squared is similar to the other 2 models, but this model still uses more features for a slightly lower R-Square than the Stepwise model
# Of the three linear regression models, the stepwise is the winner, because it achieves close to the highest possible r-square while using the fewest number of features 

# Plotting the actual distribution of wins 
hist(full_df$W, main = 'Distribution of Wins')     # Looks left-skewed normal ish 

hist((full_df %>% filter(season == '2022'))$W, main = "Distribution of Wins 2022", xlab="Wins")
hist((full_df %>% filter(season == '2021'))$W, breaks = 10, main = "Distribution of Wins 2021", xlab="Wins")
hist((full_df %>% filter(season == '2018'))$W, breaks = 10, main = "Distribution of Wins 2018", xlab="Wins")

# Distribution of wins converges to normal over time BUT wins do not follow a normal distribution at the season level