############## Modeling Match results in Soccer using a Hierarchical bayesian poisson model
getwd()
setwd('C:/Users/jaemini_man/Desktop/GitHub/soccer/soccer')

### install packages
install.packages(c('rjags','coda','mcmcplots','stringr','plyr'))
install.packages('coda')
install.packages('mcmcplots')
install.packages('stringr')
install.packages('plyr')
install.packages('tidyverse')
install.packages('bayesboot')
install.packages('rjags')

### library 
library(tidyverse)
library(readr)
library(lubridate)
library(hash)
library(rjags)
library(coda)
library(mcmcplots)
library(stringr)
library(plyr)
library(xtable)
library(bayesboot)
library(readr)



### load data 
Load_Data <- function(league_name) {
  
  Before_COVID <- read_csv(paste0('Before_COVID_', league_name, '.csv'))
  After_COVID <- read_csv(paste0('After_COVID_', league_name, '.csv'))
  After_COVID <- After_COVID %>% mutate(season = rep(paste0(league_name, '9999')))
  total <- rbind(Before_COVID, After_COVID)
  df <- total %>% rename(c('full_home_score' = 'HomeGoals', 'full_away_score' = 'AwayGoals',
                              'home' = 'HomeTeam', 'away' = 'AwayTeam', 'season' = 'Season'))
  
  # -1 = Away win, 0 = Draw, 1 = Home win
  df$MatchResult <- sign(df$HomeGoals - df$AwayGoals)
  df <- df %>% select(HomeGoals, AwayGoals, Season, HomeTeam, AwayTeam, date_no)
  
  # Creating a data frame d with only the complete match results
  d <- na.omit(df)
  teams <- unique(c(d$HomeTeam %>% as.character(), d$AwayTeam %>% as.character()))
  seasons <- unique(d$Season)
  # A list for JAGS with the data from d where the strings are coded as
  # integers
  data_list <- list(HomeGoals = d$HomeGoals, AwayGoals = d$AwayGoals,
                    HomeTeam = as.numeric(factor(d$HomeTeam %>% as.character(), levels = teams)),
                    AwayTeam = as.numeric(factor(d$AwayTeam %>% as.character(), levels = teams)),
                    Season = as.numeric(factor(d$Season, levels = seasons)), n_teams = length(teams),
                    n_games = nrow(d), n_seasons = length(seasons))
  return(list(data_list, teams, seasons))
}
BL <- Load_Data("BL")
LIGA <- Load_Data("LIGA")
SE <- Load_Data('SE')
PL <- Load_Data("PL")

# Convenience function to generate the type of column names Jags outputs.
col_name <- function(name, ...) {
  paste0(name, "[", paste(..., sep = ","), "]")
}


# model
base_string <- "model {
for(i in 1:n_games) {
  HomeGoals[i] ~ dpois(lambda_home[Season[i], HomeTeam[i], AwayTeam[i]])
  AwayGoals[i] ~ dpois(lambda_away[Season[i], HomeTeam[i], AwayTeam[i]])
}
for(season_i in 1:n_seasons) {
  for(home_i in 1:n_teams) {
    for(away_i in 1:n_teams) {
      lambda_home[season_i, home_i, away_i] <- exp( home_baseline[season_i] +
                                                      skill[season_i, home_i] - skill[season_i, away_i])
      lambda_away[season_i, home_i, away_i] <- exp( away_baseline[season_i] +
                                                      skill[season_i, away_i] - skill[season_i, home_i])
    }
  }
}
skill[1, 1] <- 0
for(j in 2:n_teams) {
  skill[1, j] ~ dnorm(group_skill, group_tau)
}
group_skill ~ dnorm(0, 0.0625)
group_tau <- 1/pow(group_sigma, 2)
group_sigma ~ dunif(0, 3)
home_baseline[1] ~ dnorm(0, 0.0625)
away_baseline[1] ~ dnorm(0, 0.0625)

for(season_i in 2:n_seasons) {
  skill[season_i, 1] <- 0
  for(j in 2:n_teams) {
    skill[season_i, j] ~ dnorm(skill[season_i - 1, j], season_tau)
  }
  home_baseline[season_i] ~ dnorm(0, 0.0625)
  away_baseline[season_i] ~ dnorm(0, 0.0625)
}
season_tau <- 1/pow(season_sigma, 2)
season_sigma ~ dunif(0, 3)
}"

###### BL sampling
BL_model <- jags.model(textConnection(base_string), data = BL[[1]], n.chains = 3, n.adapt = 10000)
update(BL_model, 10000)
BL_sample <- coda.samples(BL_model, variable.names = c("home_baseline", "away_baseline",
                                            "skill", "season_sigma", "group_sigma", 
                                            "group_skill"), n.iter = 90000, thin = 8)
BL_sample <- as.matrix(BL_sample)
save(BL_sample, file = 'BL_sample.RData')

###### SE sampling
SE_model <- jags.model(textConnection(base_string), data = SE[[1]], n.chains = 3, n.adapt = 10000)
update(SE_model, 10000)
SE_sample <- coda.samples(SE_model, variable.names = c("home_baseline", "away_baseline",
                                                   "skill", "season_sigma", "group_sigma", 
                                                   "group_skill"), n.iter = 90000, thin = 8)
SE_sample <- as.matrix(SE_sample)
save(SE_sample, file = 'SE_sample.RData')


###### LIGA sampling
LIGA_model <- jags.model(textConnection(base_string), data = LIGA[[1]], n.chains = 3, n.adapt = 10000)
update(LIGA_model, 10000)
LIGA_sample <- coda.samples(LIGA_model, variable.names = c("home_baseline", "away_baseline",
                                                       "skill", "season_sigma", "group_sigma", 
                                                       "group_skill"), n.iter = 90000, thin = 8)
LIGA_sample <- as.matrix(LIGA_sample)
save(LIGA_sample, file = 'LIGA_sample.RData')


#########BL
colnames(BL_sample)
home_base <- BL_sample[, str_detect(string = colnames(BL_sample), "home_baseline\\[")] 
away_base <- BL_sample[, str_detect(string = colnames(BL_sample), "away_baseline\\[")] 
home_advantage <- exp(home_base) - exp(away_base)
colnames(home_advantage) <- BL[[3]] %>% replace(., .=='BL9999', 'After COVID-19')
par(mfrow = c(1,1))
par(mar = c(5, 5, 5, 5), xaxs = "i")
caterplot(home_advantage, labels.loc = "above", val.lim = c(-0.4, 0.9), reorder = FALSE)
title("BundesLiga Home advantage in 10seasons")

#########LIGA
colnames(LIGA_sample)
home_base <- LIGA_sample[, str_detect(string = colnames(LIGA_sample), "home_baseline\\[")] 
away_base <- LIGA_sample[, str_detect(string = colnames(LIGA_sample), "away_baseline\\[")] 
home_advantage <- exp(home_base) - exp(away_base)
colnames(home_advantage) <- LIGA[[3]] %>% replace(., .=='LIGA9999', 'After COVID-19')
par(mfrow = c(1,1))
par(mar = c(5, 5, 5, 5), xaxs = "i")
caterplot(home_advantage, labels.loc = "above", val.lim = c(-0.4, 0.9), reorder = FALSE)
title("LaLiga Home advantage in 10seasons")

#########SE
colnames(SE_sample)
home_base <- SE_sample[, str_detect(string = colnames(SE_sample), "home_baseline\\[")] 
away_base <- SE_sample[, str_detect(string = colnames(SE_sample), "away_baseline\\[")] 
home_advantage <- exp(home_base) - exp(away_base)
colnames(home_advantage) <- SE[[3]] %>% replace(., .=='SE9999', 'After COVID-19')
par(mfrow = c(1,1))
par(mar = c(5, 5, 5, 5), xaxs = "i")
caterplot(home_advantage, labels.loc = "above", val.lim = c(-0.4, 0.9), reorder = FALSE)
title("Serie A Home advantage in 10seasons")


par(mar = c(6, 6, 6, 6))
home.before.COVID.seasons <- colnames(BL_sample)[str_detect(colnames(BL_sample), 'home')] %>% head(., -1)
away.before.COVID.seasons <- colnames(BL_sample)[str_detect(colnames(BL_sample), 'away')] %>% head(., -1)
plotPost(exp(BL_sample[, home.before.COVID.seasons] %>% 
               c()) - exp(BL_sample[, away.before.COVID.seasons] %>% c()), 
         compVal = 0,
         xlab = "Home advantage in number of goals", xlim = c(-0.2, 0.6))
title('Before_COVID_19 (9 seasons)')
plotPost(exp(BL_sample[, "home_baseline[10]"]) - exp(BL_sample[, "away_baseline[10]"]), 
         compVal = 0,
         xlab = "Home advantage in number of goals", xlim = c(-0.2, 0.6))
title('After_COVID_19')

load('bases3.RData')

BL_home_ad <- exp(BL_sample[, home.before.COVID.seasons] %>%
                    c()) - exp(BL_sample[, away.before.COVID.seasons] %>% c())
SE_home_ad <- exp(SE_sample[, home.before.COVID.seasons] %>% 
                    c()) - exp(SE_sample[, away.before.COVID.seasons] %>% c())
LIGA_home_ad <- exp(LIGA_sample[, home.before.COVID.seasons] %>% 
                                    c()) - exp(LIGA_sample[, away.before.COVID.seasons] %>% c())
PL_home_ad <- exp(bases3[, home.before.COVID.seasons] %>% 
                                  c()) - exp(bases3[, away.before.COVID.seasons] %>% c())

temp <- cbind(c(BL_home_ad, SE_home_ad, LIGA_home_ad, PL_home_ad))

plotPost(temp, 
         compVal = 0,
         xlab = "Home advantage in number of goals", xlim = c(-0.2, 0.6))
title('Before_COVID_19 (9 seasons, 4 leagues)')

BL_home_ad2 <- exp(BL_sample[, "home_baseline[10]"]) - exp(BL_sample[, "away_baseline[10]"])
SE_home_ad2 <- exp(SE_sample[, "home_baseline[10]"]) - exp(SE_sample[, "away_baseline[10]"])
LIGA_home_ad2 <- exp(LIGA_sample[, "home_baseline[10]"]) - exp(LIGA_sample[, "away_baseline[10]"])
PL_home_ad2 <- exp(bases3[, "home_baseline[10]"]) - exp(bases3[, "away_baseline[10]"])

temp2 <- cbind(c(BL_home_ad2, SE_home_ad2, LIGA_home_ad2, PL_home_ad2))
plotPost(temp2, 
         compVal = 0,
         xlab = "Home advantage in number of goals", xlim = c(-0.2, 0.6))
title('After_COVID_19')

# selection of d in prediction 2

PL


