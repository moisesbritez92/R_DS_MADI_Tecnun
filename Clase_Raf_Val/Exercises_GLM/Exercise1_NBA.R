# https://www.kaggle.com/datasets
# https://www.kaggle.com/datasets/drgilermo/nba-players-stats?resource=download

# load data
season <- read_csv("NBA/Seasons_Stats.csv")
nba_2017 <- season %>%
  dplyr::filter(Year == 2017) %>%
  dplyr::select(Player, MP, PTS) %>%
  dplyr::filter(!is.na(MP), !is.na(PTS), MP > 0, PTS > 0)


ggplot(nba_2017, aes(x = MP, y = PTS)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  labs(
    title = "Minutes Played vs Points Scored (NBA 2017 Season)",
    x = "Minutes Played",
    y = "Points"
  ) +
  theme_minimal()

# 3. Compute correlations
pearson  <- cor(nba_2017$MP, nba_2017$PTS, method = "pearson")
spearman <- cor(nba_2017$MP, nba_2017$PTS, method = "spearman")

pearson
spearman


cor.test(nba_2017$MP, nba_2017$PTS, method = "pearson")
cor.test(nba_2017$MP, nba_2017$PTS, method = "spearman")
