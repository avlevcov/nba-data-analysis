library(tidyverse)
library(sqldf)
library(nbastatR)
library(sqldf)
standard_error <- function(mean, sample_size){
  standard_error <- sqrt(mean * (1 - mean) / sample_size)
  return(standard_error)
}

B <- function(c_se, h_se){
  B <- c_se^2 / (c_se^2 + h_se^2)
  return(B)
}
source
ev_posterior_distribution <- function(c_mean, h_mean, B){
  h_mean + (1 - B) * (c_mean - h_mean)
}

se_posterior_distribution <- function(c_se, h_se){
  sqrt(1 / ((1/h_se^2) + (1/c_se^2)))
}

player_list <- data.frame(names = c("Kevin Durant", "LeBron James", "Kyrie Irving", "Stephen Curry", "Giannis Antetokounmpo", "James Harden", "Damian Lillard", "Joel Embiid", "Nikola Jokic", "Nikola Vucevic"))

player_totals_2015_20 <- bref_players_stats(seasons = 2016:2020, tables = c("totals"), assign_to_environment = FALSE)
player_totals_2020 <- bref_players_stats(seasons = 2021, tables = c("totals"), assign_to_environment = FALSE)
player_totals_2020 <- player_totals_2020 %>% mutate(slugTeamBREF = ifelse(slugTeamBREF == "CHO", "CHA", ifelse(slugTeamBREF == "BRK", "BKN", slugTeamBREF)))

player_per_game_2020 <- bref_players_stats(seasons = 2021, tables = c("per game"), assign_to_environment = FALSE)

current_season_teams_info <- teams_seasons_info(seasons = c(2021), season_types = "Regular Season")
current_season_teams_info <- mutate(current_season_teams_info, total_games = winsTeam + lossesTeam, games_left = 72 - winsTeam - lossesTeam)

total_3s_taken_filter <- mean(player_totals_2020$fg3aTotals)

top_3fg_shooters_2020 <- player_totals_2020[order(-player_totals_2020$pctFG3),]
top_3fg_shooters_2020 <- top_3fg_shooters_2020 %>% filter(fg3aTotals >= total_3s_taken_filter)


top_3fg_shooters_2020 <- sqldf("SELECT * FROM top_3fg_shooters_2020 ORDER BY pctFG3 DESC LIMIT 20")

selected_player_totals <- sqldf("SELECT namePLAYER, SUM(fg3mTotals) AS h_fg3mTotals, SUM(fg3aTotals) AS h_fg3aTotals, 
                                SUM(fg3mTotals)/SUM(fg3aTotals) AS h_pctFG3 
                                FROM player_totals_2015_20 
                                WHERE (namePLAYER IN (SELECT namePLAYER FROM top_3fg_shooters_2020) OR
                                namePLAYER IN (SELECT * FROM player_list)) 
                                GROUP BY namePLAYER")

current_past_totals <- sqldf("SELECT A.namePLAYER, A.h_fg3mTotals, A.h_fg3aTotals, A.h_pctFG3,
                             B.fg3mTotals, B.fg3aTotals, B.pctFG3, B.slugTeamsBREF, C.total_games, C.games_left
                             FROM selected_player_totals A 
                             INNER JOIN player_totals_2020 B ON A.namePLAYER = B.namePLaYER
                             INNER JOIN current_season_teams_info C ON B.slugTeamBREF = C.slugTeam")

current_past_totals <- current_past_totals %>% 
                              mutate( ev = ev_posterior_distribution(pctFG3, h_pctFG3, B(standard_error(pctFG3, fg3aTotals),standard_error(h_pctFG3, h_fg3aTotals))),
                              ci_L = ev - qnorm(0.975) * se_posterior_distribution(standard_error(pctFG3, fg3aTotals), standard_error(h_pctFG3, h_fg3aTotals)),
                              ci_U = ev + qnorm(0.975) * se_posterior_distribution(standard_error(pctFG3, fg3aTotals), standard_error(h_pctFG3, h_fg3aTotals)))

current_past_totals <- sqldf("SELECT A.namePlayer, A.h_fg3mTotals, A.h_fg3aTotals, A.h_pctFG3, A.fg3mTotals, A.fg3aTotals, A.pctFG3, A.ev, A.ci_L, A.ci_U, 
                              B.fg3aPerGame, A.slugTeamsBREF, A.total_games, A.games_left
                              FROM current_past_totals A
                              INNER JOIN player_per_game_2020 B ON A.namePlayer = B.namePlayer")

current_past_totals <- current_past_totals %>% mutate(remaining_3s = round(fg3aPerGame * games_left, 0),
                                                      ev = (ev * remaining_3s + fg3mTotals)/ (remaining_3s + fg3aTotals),
                                                      ci_L = (ci_L * remaining_3s + fg3mTotals)/ (remaining_3s + fg3aTotals),
                                                      ci_U = (ci_U * remaining_3s + fg3mTotals)/ (remaining_3s + fg3aTotals))


colors <- c("Full Season Expected Value" = "red", "2015-2020 Percentage" = "black", "Current Season Percentage" = "blue")

current_past_totals %>% ggplot(aes(x = reorder(namePlayer, -pctFG3))) +
  geom_point(aes(y = pctFG3, color = "Current Season Percentage"), size = 2) +
  geom_point(aes(y = h_pctFG3, color = "2015-2020 Percentage"), size = 2) +
  geom_point(aes(namePlayer, ev, color = "Full Season Expected Value"), size = 2) +
  geom_errorbar(aes(x = namePlayer, y = ev, ymin = ci_L, ymax = ci_U), size = 0.5, width = 0.6) +
  geom_text(aes(y = ifelse(pctFG3 > ci_U, pctFG3, ci_U), label = fg3aTotals), nudge_y = 0.01)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1)) +
  labs( x = "Player", y = "3-Point Percentage", color = "Legend") +
  scale_color_manual(values = colors)