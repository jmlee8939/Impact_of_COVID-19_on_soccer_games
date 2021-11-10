library('tidyverse')

setwd('C:/Users/Infosci_center/Documents/GitHub/soccer/soccer')

B_PL <- read.csv('Before_COVID_PL.csv')
B_LIGA <- read.csv('Before_COVID_LIGA.csv')
B_SE <- read.csv('Before_COVID_SE.csv')
B_BL <- read.csv('Before_COVID_BL.csv')

A_PL <- read.csv('After_COVID_PL.csv')
A_LIGA <- read.csv('After_COVID_LIGA.csv')
A_SE <- read.csv('After_COVID_SE.csv')
A_BL <- read.csv('After_COVID_BL.csv')


all_data <- rbind(B_PL, B_LIGA, B_SE, B_BL, A_PL, A_LIGA, A_SE, A_BL)
all_data <- all_data %>% rename(c('full_home_score' = 'HomeGoals', 'full_away_score' = 'AwayGoals',
                            'home' = 'HomeTeam', 'away' = 'AwayTeam', 'season' = 'Season'))

d <- na.omit(all_data)
teams <- unique(c(d$HomeTeam %>% as.character(), d$AwayTeam %>% as.character()))
seasons <- unique(d$Season)
# A list for JAGS with the data from d where the strings are coded as
# integers
data_list <- list(HomeGoals = d$HomeGoals, AwayGoals = d$AwayGoals,
                  HomeTeam = as.numeric(factor(d$HomeTeam %>% as.character(), levels = teams)),
                  AwayTeam = as.numeric(factor(d$AwayTeam %>% as.character(), levels = teams)),
                  Season = as.numeric(factor(d$Season, levels = seasons)), n_teams = length(teams),
                  n_games = nrow(d), n_seasons = length(seasons))
# Convenience function to generate the type of column names Jags outputs.
col_name <- function(name, ...) {
  paste0(name, "[", paste(..., sep = ","), "]")
}


par(mfcol = c(2, 1), mar = rep(2.2, 4))
hist(c(d$AwayGoals, d$HomeGoals), xlim = c(-0.5, 8), breaks = -1:10 + 0.5,
     main = "실제 한 경기당 골 수 히스토그램 ")
mean_goals <- mean(c(d$AwayGoals, d$HomeGoals))
hist(rpois(9999, mean_goals), xlim = c(-0.5, 8), breaks = -1:9 + 0.5,
     main = paste0("경기당 골 수(",round(mean_goals,3), ")를 평균으로 하는 Poisson distribution"))

