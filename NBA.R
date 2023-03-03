library(tidyverse)
library(ggimage)
library(caret)
library(ggthemes)
library(reactablefmtr)

# Load data
nba <- read_csv("Desktop/Basketball Analysis/NBA Project/Data/NBATeams.csv")
nba <- nba[-1] # Remove index column
nba <- nba %>% # Sort by team
  arrange(Team)
View(nba)

# Create preview of data for medium
set.seed(7)
sampe_df <- nba %>%
  sample_n(size = 15) %>%
  mutate("filler" = rep("...", 15)) %>%
  select(Team, Season, GP, MPG, PPG, filler, Pace, eDiff, `Win %`)
sampe_df %>% # 538 style table
  reactable(
    theme = fivethirtyeight(),
    defaultColDef = colDef(
      maxWidth = 60,
      align = "center"
    ),
    columns = list(
      Team = colDef(
        maxWidth = 100,
        align = "left"
      ),
      Season = colDef(
        maxWidth = 70,
        align = "left"
      ),
      filler = colDef(
        name = "...",
        maxWidth = 30
        ),
      `Win %` = colDef(
        maxWidth = 60
      )
    ),
    defaultPageSize = 15
  )

# Win % Distribution
col <- "darkred"
ggplot(nba, aes(x = `Win %`)) + 
  geom_histogram(aes(y = ..density..), colour = col, fill = col, alpha = 0.5) +
  geom_density(alpha = .2, fill = col, color = col) +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  labs(
    title = "Distribution of Win Percentage",
    subtitle = "From the 2004-2005 NBA Season to the 2021-2022 NBA Season"
  ) + ylab("Density") + xlab("Win Percentage")

# Data frame of team logo image links for 538 style tables
icons_df <- read.csv("https://raw.githubusercontent.com/kcuilla/reactablefmtr/main/vignettes/RAPTOR_by_team_19-20.csv")

# Helper function to match team name in icons_df with nba 
extract_id <- function(x) { 
  str_vec <- str_split(x, " ")[[1]]
  n <- length(str_vec)
  if (n > 2) {
    if (str_vec[n] %in% c("Clippers", "Lakers")) {
      if (str_vec[n] == "Clippers") {
        "L.A. Clippers"
      } else {
        "L.A. Lakers"
      }
    } else {
      str_vec <- str_vec[-n]
      paste(str_vec, collapse = " ")
    }
  } else {
    str_vec[-n]
  }
}
extract_id <- Vectorize(extract_id)

# Extract team name columns with image links
icons_df <- icons_df %>% 
  mutate(Team = TEAM_NAME, Image = TEAM_LOGO) %>%
  select(-c(TEAM_NAME, TEAM_LOGO)) %>%
  mutate(Team = extract_id(Team)) %>% 
  distinct(Team, Image) %>% 
  add_row(Team = "Portland", "Image" = NA) # Portland missing for some reason

# Teams with highest win %
win_df <- nba %>%
  left_join(icons_df, by = "Team") %>% # Join image links to df
  arrange(desc(`Win %`)) %>%
  slice(1:15) %>%
  select(Image, Team, Season, `Win %`)
win_df %>% # 538 style table
  reactable(
    theme = fivethirtyeight(),
    columns = list(
      # add logos
      Image = colDef(
        name = "Team",
        maxWidth = 70,
        align = "center",
        cell = embed_img(height = "25", width = "40")
      ),
      Team = colDef(maxWidth = 150, name = ""),
      # add color scales
      Season = colDef(
        maxWidth = 60
      ), 
      # add color scales
      `Win %` = colDef(
        maxWidth = 60,
        cell = function(x)
          sprintf("%.3f", x),
        style = color_scales(win_df, colors = c("gray97", "palegreen3"))
      )
    ),
    defaultPageSize = 15
  )

# Calculate TER
eDiff_mdl <- lm(eDiff ~ PPG + FGM + FTM + FGA + FTA + DRB + ORB + APG + SPG + BPG + PF + TOV, data = nba)
summary(eDiff_mdl)
nba <- nba %>% # Add TER
  mutate(TER = scale(3/4 * PPG - 2 * FGA - 3/4 * FTA + 1.5 * DRB + 9/4 * ORB + 1/5 * APG + 2 * SPG + 1/4 * BPG + 1/5 * PF - 2 * TOV))
View(nba)

# Table of TER estimates and weights
mdl_coefs <- summary(eDiff_mdl)$coefficients %>% 
  data.frame() %>%
  rownames_to_column(var = "Variable")
colnames(mdl_coefs) <- c("Variable", "Estimate", "Std", "tval", "Pval")
Ws <- c(NA, 3/4, 0, 0, -2, -3/4, 1.5, 9/4, 1/5, 2, 1/4, 1/5, -2)
mdl_coefs %>% # 538 style table
  mutate(Weights = Ws) %>%
  slice(-1) %>% # Remove intercept
  select(Variable, Estimate, Weights, Pval) %>%
  reactable(
    theme = fivethirtyeight(),
    columns = list(
      Variable = colDef(
        name = "Variable",
        maxWidth = 100
      ),
      Estimate = colDef(
        maxWidth = 100,
        cell = function(x)
          sprintf("%.2f", x)
      ),
      Weights = colDef(
        maxWidth = 100,
        name = "Weight",
        cell = function(x)
          sprintf("%.2f", x)
      ),
      Pval = colDef(
        maxWidth = 100,
        name = "p-value",
        cell = function(x)
          sprintf("%.2f", x)
      )
    ),
    defaultPageSize = 12
  )

# Teams with highest TER
TER_df <- nba %>%
  left_join(icons_df, by = "Team") %>% # Join image links to df
  arrange(desc(TER)) %>%
  mutate(TER = round(TER, 2)) %>%
  slice(1:15) %>%
  select(Image, Team, Season, TER)
TER_df %>% # 538 style table
  reactable(
    theme = fivethirtyeight(),
    columns = list(
      # add logos
      Image = colDef(
        name = "Team",
        maxWidth = 70,
        align = "center",
        cell = embed_img(height = "25", width = "40")
      ),
      Team = colDef(maxWidth = 150, name = ""),
      Season = colDef(
        maxWidth = 60
      ),
      # add color scales
      TER = colDef(
        maxWidth = 60,
        cell = function(x)
          sprintf("%+0.1f", x),
        style = color_scales(TER_df, colors = c("white", "#42c2ca"))
      )
    ),
    defaultPageSize = 15
  )

# TER Distribution
ggplot(nba, aes(x = TER)) + 
  geom_histogram(aes(y = ..density..), colour = "darkorange2", fill = "darkorange2", alpha = 0.5)+
  geom_density(alpha = .2, fill = "darkorange2", color = "darkorange2") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  labs(
    title = "Distribution of TER",
    subtitle = "From the 2004-2005 NBA Season to the 2021-2022 NBA Season"
  ) + ylab("Density") + xlab("TER")

# eDiff Distribution
ggplot(nba, aes(x = eDiff)) + 
  geom_histogram(aes(y = ..density..), colour = "goldenrod", fill = "goldenrod", alpha = 0.5)+
  geom_density(alpha = .2, fill = "goldenrod", color = "goldenrod") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  labs(
    title = "Distribution of Net Rating",
    subtitle = "From the 2004-2005 NBA Season to the 2021-2022 NBA Season"
  ) + ylab("Density") + xlab("Net Rating")

# TER vs Pace
nba %>%
  select(TER) %>% pull() %>%
  quantile(prob = seq(0, 1, len = 21)) # TER quantiles
nba %>%
  select(Pace) %>% pull() %>%
  quantile(prob = seq(0, 1, len = 21)) # Pace quantiles
pace_means <- nba %>% 
  summarize(TER = mean(TER), Pace = mean(Pace))
pace_means
pace_top_teams <- nba %>% # Table for scatter plot
  filter(Pace >= 100.905 & TER >= 1.587) %>%  # 95th percentiles
  select(Team, Season, Pace, TER) %>%
  mutate(
    Name = c("18-19 Bucks", "19-20 Bucks", "20-21 Bucks"), 
    image = rep("Desktop/Basketball Analysis/NBA Project/Images/bucks.png", 3)
         )
pace_top_teams
nba %>% # Scatter plot
  filter(Team != "Milwaukee" | (Season < 2019 | Season > 2021)) %>%
  ggplot(aes(y = Pace, x = TER)) +
  geom_point(size = 6, shape = 21, color = "black", fill = "lightgray", alpha = 1) + xlim(-4, 4) + ylim(83, 107) + 
  geom_vline(xintercept = pace_means$TER, color = "black", lwd = 1) +
  geom_hline(yintercept = pace_means$Pace, color = "black", lwd = 1) +
  geom_image(data = pace_top_teams, aes(x = TER, y = Pace, image = image), size = 0.08) +
  geom_text(data = pace_top_teams, aes(x = TER, y = Pace, label = Name), vjust = -2, hjust = -.15, color = "black", size = 4.5) +
  theme_fivethirtyeight() + 
  theme(
    axis.title = element_text()
  ) + 
  labs(
    title = "Team Efficiency Rating vs Pace",
    subtitle = "From the 2004-2005 NBA Season to the 2021-2022 NBA Season",
  ) +
  ylab("Pace") + xlab("Team Efficiency Rating")

# Three pointers over the years
nba %>%
  group_by(Season) %>%
  summarize(`Avg 3PA` = mean(`3PA`), `Avg 3PM` = mean(`3PM`), `Avg 3P%` = mean(`3P%`)) %>%
  ggplot(aes(x = Season)) + ylim(0, 50) +
  geom_area(aes(y = `Avg 3PA`, fill = "3PA"), color = "black", alpha = 0.5) + 
  geom_area(aes(y = `Avg 3PM`, fill = "3PM"), color = "black", alpha = .5) + 
  theme_fivethirtyeight() +
  theme(
    axis.title = element_text(), 
    legend.position = "right", 
    legend.direction = "vertical",
    legend.key.size = unit(1, 'cm')
    ) + 
  labs(
    title = "Three Point Shooting Over the Years",
    subtitle = "From the 2004-2005 NBA Season to the 2021-2022 NBA Season",
    fill = "Legend"
  ) + scale_fill_manual(values = c("lightblue", "darkblue")) +
  ylab("Average") + xlab("Season")

# DRtg vs 3PA
per_poss <- nba %>%
  mutate(`3PA` = `3PA` * GP / Poss * 100) # 3PA per 100 possessions
per_poss %>%
  select(`3PA`) %>% pull() %>%
  quantile(prob = seq(0, 1, len = 21)) # 3PA quantiles
nba %>%
  select(DRtg) %>% pull() %>%
  quantile(prob = seq(0, 1, len = 21)) # DRtg quantiles
drtg_means <- per_poss %>% 
  summarize(`3PA` = mean(`3PA`), DRTg = mean(DRtg))
drtg_means
drtg_top_teams <- per_poss %>% # Teams with low DRtg and high 3PA
  filter(DRtg <= 103.9 & `3PA` >= 32.8) %>%  # 85th percentiles
  select(Team, Season, `3PA`, DRtg, TER) %>%
  mutate(
    Name = c("14-15 Rockets", "19-20 Bucks"),
    image = c(
      "Desktop/Basketball Analysis/NBA Project/Images/rockets.png",
      "Desktop/Basketball Analysis/NBA Project/Images/bucks.png"
      )
    )
drtg_top_teams
per_poss %>% # Scatter plot
  filter(Team != "Houston" | Season != 2015) %>%
  filter(Team != "Milwaukee" | Season != 2020) %>%
  ggplot(aes(x = DRtg, y = `3PA`)) +
  geom_point(size = 6, shape = 21, color = "black", fill = "lightgray", alpha = 1) + 
  geom_vline(xintercept = drtg_means$DRTg, color = "black", lwd = 1.5) +
  geom_hline(yintercept = drtg_means$`3PA`, color = "black", lwd = 1.5) +
  geom_image(data = drtg_top_teams, aes(x = DRtg, y = `3PA`, image = image), size = c(0.05, 0.08)) +
  geom_text(data = drtg_top_teams, aes(x = DRtg, y = `3PA`, label = Name), vjust = 2.7, hjust = .5, color = "black", size = 5) + coord_flip() + # Mixed up coords (oops)
  theme_fivethirtyeight() + 
  theme(
    axis.title = element_text()
  ) + 
  labs(
    title = "Three Point Attempts (Per 100 Poss) vs Defensive Rating",
    subtitle = "From the 2004-2005 NBA Season to the 2021-2022 NBA Season",
  ) +
  xlab("Defensive Rating") + ylab("Three Point Attempts Per 100 Possessions")

# ORtg vs DRtg
nba %>%
  select(ORtg) %>% pull() %>%
  quantile(prob = seq(0, 1, len = 21)) # ORtg quantiles
nba %>%
  select(DRtg) %>% pull() %>%
  quantile(prob = seq(0, 1, len = 21)) # DRtg quantiles
rtg_means <- nba %>% 
  summarize(ORtg = mean(ORtg), DRtg = mean(DRtg))
rtg_means
rtg_top_teams <- nba %>% # Scatter plot
  filter(DRtg <= 104.600 & ORtg >= 114.205) %>%  # 95th percentile ORtg and 20th percentile DRtg
  select(Team, Season, ORtg, DRtg) %>%
  mutate(
    Name = c("15-16 Warriors", "16-17 Warriors"),
    image = rep("Desktop/Basketball Analysis/NBA Project/Images/warriors.png", 2)
  )
rtg_top_teams
nba %>% # Scatter plot
  ggplot(aes(x = ORtg, y = DRtg)) +
  geom_point(size = 6, shape = 21, color = "black", fill = "lightgray", alpha = 1) + ylim(95, 120) + xlim(95, 120) + 
  geom_vline(xintercept = rtg_means$DRtg, color = "black", lwd = 1) +
  geom_hline(yintercept = rtg_means$ORtg, color = "black", lwd = 1) +
  geom_image(data = rtg_top_teams, aes(x = ORtg, y = DRtg, image = image), size = 0.03, asp = 1.5) + 
  geom_text(data = rtg_top_teams, aes(x = ORtg, y = DRtg, label = Name), vjust = c(2.5, -1.6), hjust = .2, color = "black", size = 4.5) +
  theme_fivethirtyeight() + 
  theme(
    axis.title = element_text()
  ) + 
  labs(
    title = "Offensive Rating vs Defensive Rating",
    subtitle = "From the 2004-2005 NBA Season to the 2021-2022 NBA Season",
  ) +
  ylab("Defensive Rating") + xlab("Offensive Rating")

# Clustering
set.seed(24)
cluster_df <- nba %>% # Create df for kmeans
  mutate(`3PA` = `3PA` * GP / Poss * 100) %>% # Per Poss 3PA
  mutate(TER = scale(3/4 * PPG - 2 * FGA - 3/4 * FTA + 1.5 * DRB + 9/4 * ORB + 1/5 * APG + 2 * SPG + 1/4 * BPG + 1/5 * PF - 2 * TOV)) %>%
  mutate(TER = scale(TER * GP / Poss * 100)) %>% # Per Poss TER
  select(ORtg, DRtg, `3PA`, Pace, TER, `Win %`) %>% as.data.frame()
kmeans_wss <- numeric(15) # Find optimal number of clusters (k)
for (i in 1:15) {
  temp <- kmeans(cluster_df, i, nstart = 30)
  kmeans_wss[i] <- temp$tot.withinss
}
plot(1:15, kmeans_wss, type = "b") # k = 4 seems optimal
k4_out <- kmeans(cluster_df, 4, nstart = 30)
cluster_final <- cluster_df %>%
  mutate(Team = nba$Team, Season = nba$Season, Cluster = as.factor(k4_out$cluster))

# Cluster plot
x_centers <- k4_out$centers[, 3]
y_centers <- k4_out$centers[, 2]
cluster_final <- cluster_final %>%
  mutate(x_center = x_centers[k4_out$cluster], y_center = y_centers[k4_out$cluster])
cols <- c("steelblue4", "steelblue3", "slategray2", "steelblue1")
cols2 <- c("steelblue4", "steelblue3", "slategray3", "steelblue1")
lbls <- c("ORtg = 104.6, Pace = 91.8", 
          "ORtg = 107.4, Pace = 91.1",
          "ORtg = 111.5, Pace = 99.3",
          "ORtg = 107.9, Pace = 95.7")
lbl_df <- tibble(
  labels = lbls,
  `3PA` = c(13, 25, 42, 32),
  DRtg = c(114.5, 98, 104.5, 100.5),
  Cluster = as.factor(1:4)
)
cluster_final %>%
  ggplot(aes(fill = Cluster)) +
  geom_segment(aes(x = `3PA`, xend = x_center, y = `DRtg`, yend = y_center, color = Cluster), lty = "solid", lwd = 1.5, alpha = 0.5) +
  geom_point(aes(x_center, y_center, color = Cluster), size = 10) +
  geom_point(aes(`3PA`, `DRtg`, color = Cluster), show.legend = FALSE, size = 5) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) + 
  theme_stata() +
  theme(
    plot.title = element_text(color = "black", hjust = -.001),
    plot.subtitle = element_text(color = "black", hjust = -.001),
    axis.title = element_text(), legend.position = "right", legend.direction = "vertical",
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
    ) + 
  geom_text(data = lbl_df, aes(x = `3PA`, y = `DRtg`, label = lbls), color = cols2, size = 4.5) +
  xlab("3PA Per 100 Possessions") + 
  ylab("Defensive Rating") + 
  labs(title = "3PA (Per 100 Poss) vs Defensive Rating",
       subtitle = "Clustered using K-Means")
k4_out$centers %>% # Create table for cluster centers
  as_tibble() %>%
  mutate(Cluster = 1:4) %>%
  select(Cluster, ORtg, DRtg, `3PA`, Pace, TER, `Win %`) %>%
  reactable(
    theme = fivethirtyeight(),
    defaultColDef = colDef(
      maxWidth = 70,
      cell = function(x)
        sprintf("%.3f", x),
      align = "center"
    ),
    columns = list(
      Cluster = colDef(
        name = "Cluster",
        align = "center",
        cell = function(x)
          sprintf("%.0f", x),
      )
    )
  )

# Extract win rates without teams from earliest season (2005)
no_earliest_wr <- nba %>% 
  filter(Season != 2005) %>% 
  select(`Win %`) %>%
  pull()

# Remove teams from latest season (2022)
no_latest_df <- nba %>% 
  filter(Season != 2022)

# Create new df with next yr win rate column
df <- no_latest_df %>%
  mutate(next_yr_wr = no_earliest_wr) 
View(df)

# Correlation table for next yr win rate
cor_tbl <- df %>%
  select(-c(Team, Season, `W's`, `L's`, next_yr_wr, MOV)) %>%
  cor(df %>% select(next_yr_wr)) %>% abs() %>% round(3) %>%
  data.frame() %>%
  arrange(desc(next_yr_wr))
colnames(cor_tbl) <- "Corr"
cor_tbl <- cor_tbl %>%
  slice(c(1:15)) %>%
  rownames_to_column(var = "Variable")
cor_tbl %>% # 538 style table
  reactable(
    theme = fivethirtyeight(),
    columns = list(
      Variable = colDef(maxWidth = 150, name = "Variable"),
      # add color scales
      Corr = colDef(
        maxWidth = 250,
        align = "center",
        name = "Correlation",
        cell = data_bars(
          cor_tbl,
          align_bars = "left",
          text_position = "outside-end",
          fill_color = c("lightsteelblue1", "steelblue3"),
          number_fmt = scales::number_format(accuracy = 0.01),
          background = "transparent"
        ),
        style = list(borderLeft = "1px dashed rgba(0, 0, 0, 0.3)")
      )
    ),
    defaultPageSize = 15
  )

# Predicting current season win %
summary(lm(`Win %` ~ TER, data = nba))
summary(lm(`Win %` ~ eDiff, data = nba))
nba %>% # Scatter plot
  ggplot(aes(x = TER, y = `Win %`)) +
  geom_point(size = 6, shape = 21, color = "black", fill = "gray") + 
  geom_smooth(method = "lm", color = "darkorange2") +
  theme_fivethirtyeight() + 
  theme(
    axis.title = element_text()
  ) + 
  labs(
    title = "Team Efficiency Rating vs Win Percentage",
    subtitle = "Variation in Outcome Variable Explained: 80%",
  ) +
  ylab("Win Percentage") + xlab("Team Efficiency Rating")
nba %>% # Scatter plot
  ggplot(aes(x = eDiff, y = `Win %`)) +
  geom_point(size = 6, shape = 21, color = "black", fill = "gray") + 
  geom_smooth(method = "lm", color = "goldenrod") + 
  theme_fivethirtyeight() + 
  theme(
    axis.title = element_text()
  ) + 
  labs(
    title = "Net Rating vs Win Percentage",
    subtitle = "Variation in Outcome Variable Explained: 93%",
  ) +
  ylab("Win Percentage") + xlab("Net Rating")

# Predicting next season win %
summary(lm(next_yr_wr ~ TER, data = df))
summary(lm(next_yr_wr ~ `Win %`, data = df))
summary(lm(next_yr_wr ~ eDiff, data = df))
df %>% # Scatter plot
  ggplot(aes(x = TER, y = next_yr_wr)) +
  geom_point(size = 6, shape = 21, color = "black", fill = "gray") + 
  geom_smooth(method = "lm", color = "darkorange2") + 
  theme_fivethirtyeight() + 
  theme(
    axis.title = element_text()
  ) + 
  labs(
    title = "Team Efficiency Rating vs Next Season's Win Percentage",
    subtitle = "Variation in Outcome Variable Explained: 33%",
  ) +
  ylab("Next Season's Win Percentage") + xlab("Team Efficiency Rating")
df %>% # Scatter plot
  ggplot(aes(x = eDiff, y = next_yr_wr)) +
  geom_point(size = 6, shape = 21, color = "black", fill = "gray") + 
  geom_smooth(method = "lm", color = "goldenrod") + 
  theme_fivethirtyeight() + 
  theme(
    axis.title = element_text()
  ) + 
  labs(
    title = "Net Rating vs Next Season's Win Percentage",
    subtitle = "Variation in Outcome Variable Explained: 39%",
  ) +
  ylab("Next Season's Win Percentage") + xlab("Net Rating")
df %>% # Scatter plot
  ggplot(aes(x = `Win %`, y = next_yr_wr)) +
  geom_point(size = 6, shape = 21, color = "black", fill = "gray") + 
  geom_smooth(method = "lm", color = "darkred") + 
  theme_fivethirtyeight() + 
  theme(
    axis.title = element_text()
  ) + 
  labs(
    title = "Current Season's Win Percentage vs Next Season's Win Percentage",
    subtitle = "Variation in Outcome Variable Explained: 38%",
  ) +
  ylab("Next Season's Win Percentage") + xlab("Current Season's Win Percentage")
  