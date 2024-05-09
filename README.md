# NBA Expansion Draft Simulation

## Project Overview
This project was developed as my capstone for the NBA Future Analytics Stars program. It aims to determine the optimal number of players each existing NBA franchise should be permitted to protect in an expansion draft scenario that introduces two new teams. The primary goal is to develop a framework that maximizes competitive balance across the league.

### Defining Competitive Balance
Competitive balance is defined in this project as a scenario where:
- The majority of teams achieve or surpass the average number of wins.
- There is a balanced number of teams with winning and losing records.
- The gap between the strongest and weakest teams is minimal.
- Talent distribution is equitable across all teams, ensuring that expansion teams have a viable chance of achieving the mean number of wins.

## Research and Methodology
The structural framework for the expansion draft was influenced by various historical precedents, including the 2004 Charlotte Bobcats NBA expansion, the most recent in NBA history. Additional insights were drawn from the G League, WNBA, and recent NHL expansion drafts to tailor the rules applicable to the NBA's current context, where teams can have up to 15 players on their rosters, compared to the 12 during the last NBA expansion. Scenarios were tested where teams could protect between 6 to 11 players, considering the introduction of two new teams and the risk of losing up to two players per existing team.

## Results
The analysis concluded that the optimal number of protected players for each NBA team in an expansion draft should be seven (7) to best preserve competitive balance across the league.

Further details on the expansion draft structure, rules, assumptions, and methodologies are thoroughly documented in the accompanying PDF slide deck.

## Models and Code
### Data Collection:
- Utilized the HoopR package to access NBA team and player statistics, with original data sourced from Basketball Reference.
- Directly downloaded additional data sets from Basketball Reference.
- Gathered information on free agents from HoopsHype.

### Modeling Approach
1. Player Clustering: Employed k-means clustering to categorize players into performance-based tiers.
2. Team Win Projections: Developed a regression model to predict the percentage of total available wins attributed to each team in a season.
3. Simulation Functions: Defined functions to simulate player protections for existing teams and the selection process in the expansion draft.
4. Statistical Framework: Created a framework to project and scale team total per-game stats, which were input into the win projections model.
5. Comprehensive Simulations: Conducted 1,000 simulations for each potential number of protected players (N=6 to N=11), assessing outcomes of the expansion draft.
Results

## Optimal Protection Strategy (N=7)
- Competitive Balance: Achieved the smallest range of projected wins, indicating a tight distribution of competitive capabilities across the league.
- Expansion Team Success: Ensured that expansion team wins were close to the league mean.
- Balanced Records: Generated an equal or larger number of teams with records above and below a .500 winning percentage.
- Talent Distribution: Minimized the number of teams with more than three players from the lowest clusters (7 and 8), promoting a balanced distribution of talent.

### Visual Analysis
Examined comprehensive statistical visualizations, including box plots, histograms, distributions, and bar plots (available in the visualizations folder), to determine that the optimal value of N is 7.

## Future Improvements
Next Steps in Project Development:
- Integrate Contract Dynamics: Incorporate contract values and identify less favorable contracts into the protection logic.
- Consider Player Development: Include factors such as players' ages, potential for development, and career trajectories in the protection strategy.
- Project Long-Term Performance: Extend projections of each player's and team's performance to upcoming seasons, taking into account historical data and anticipated career paths.
- Assess Long-Term Competitive Balance: Evaluate the impact on competitive balance over an extended period, such as three or five seasons, to gauge sustained effects.

## Acknowledgements
HoopR: Special thanks to Saiem Gilani and all contributors to the HoopR package from the SportsDataverse, which was instrumental in accessing and analyzing NBA data for this project. HoopR: The SportsDataverse's R Package for Men's Basketball Data.

The NBA & NBA Future Analytics Stars Program: I am grateful to the NBA and all the mentors and participants in the Future Analytics Stars program for providing me with this incredible opportunity to develop and showcase my project.
