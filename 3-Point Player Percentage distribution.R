library(tidyverse)
library(sqldf)
library(nbastatR)
library(sqldf)

player_totals_1980_20 <- bref_players_stats(seasons = 1981:2020, tables = c("totals"), assign_to_environment = FALSE)
# player_totals_2000_21 <- player_totals_2000_21 %>% mutate(slugTeamBREF = ifelse(slugTeamBREF == "CHO", "CHA", ifelse(slugTeamBREF == "BRK", "BKN", slugTeamBREF)))

player_totals_1980_20 <- filter(player_totals_1980_20, fg3aTotals > 0)

nba_3pt_stats <- sqldf("SELECT slugSeason, SUM(fg3mTotals) AS h_fg3mTotals, SUM(fg3aTotals) AS h_fg3aTotals, AVG(fg3mTotals) AS h_average_attempts,
                                SUM(fg3mTotals)/SUM(fg3aTotals) AS h_pctFG3 
                                FROM player_totals_1980_20
                                GROUP BY slugSeason")


# colors <- c("Full Season Expected Value" = "red", "2015-2020 Percentage" = "black", "Current Season Percentage" = "blue")
# 
# current_past_totals %>% ggplot(aes(x = reorder(namePlayer, -pctFG3))) +
#   geom_point(aes(y = pctFG3, color = "Current Season Percentage"), size = 2) +
#   geom_point(aes(y = h_pctFG3, color = "2015-2020 Percentage"), size = 2) +
#   geom_point(aes(namePlayer, ev, color = "Full Season Expected Value"), size = 2) +
#   geom_errorbar(aes(x = namePlayer, y = ev, ymin = ci_L, ymax = ci_U), size = 0.5, width = 0.6) +
#   geom_text(aes(y = ifelse(pctFG3 > ci_U, pctFG3, ci_U), label = fg3aTotals), nudge_y = 0.01)+
#   theme_light() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1)) +
#   labs( x = "Player", y = "3-Point Percentage", color = "Legend") +
#   scale_color_manual(values = colors)

nba_3pt_stats %>% ggplot(aes(x = slugSeason, y = h_pctFG3)) +
  geom_point() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1))

nba_3pt_stats %>% ggplot(aes(x = slugSeason, y = h_average_attempts)) +
  geom_point() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1))


test %>% ggplot(aes(x = pctFG3)) +
  geom_density()

test <- player_totals_1980_20 %>% filter(fg3mTotals >= 82, yearSeason %in% c(2016,2017,2018,2019,2020))

