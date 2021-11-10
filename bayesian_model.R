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
?read_csv
Before_COVID_PL <- read_csv('Before_COVID_PL.csv')
After_COVID_PL <- read_csv('After_COVID_PL.csv')
After_COVID_PL <- After_COVID_PL %>% filter(season == 'PL1920') %>%  mutate(season = rep('PL9999'))
PL_total <- rbind(Before_COVID_PL, After_COVID_PL)
dim(Before_COVID_PL)
dim(After_COVID_PL)

colnames(Before_COVID_PL)
colnames(After_COVID_PL)
# PL.new <- PL %>% filter(date_no > 18707)
PL <- PL_total %>% rename(c('full_home_score' = 'HomeGoals', 'full_away_score' = 'AwayGoals',
                            'home' = 'HomeTeam', 'away' = 'AwayTeam', 'season' = 'Season'))

# -1 = Away win, 0 = Draw, 1 = Home win
PL$MatchResult <- sign(PL$HomeGoals - PL$AwayGoals)
PL <- PL %>% select(HomeGoals, AwayGoals, Season, HomeTeam, AwayTeam, date_no)

# Creating a data frame d with only the complete match results
d <- na.omit(PL)
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

data_list
seasons

# 축구 결과가 ????????? 분포??? ???르는 지 
par(mfcol = c(2, 1), mar = rep(2.2, 4))
hist(c(d$AwayGoals, d$HomeGoals), xlim = c(-0.5, 8), breaks = -1:9 + 0.5,
     main = "Distribution of the number of goals\nscored by a team in a match.")
mean_goals <- mean(c(d$AwayGoals, d$HomeGoals))
hist(rpois(9999, mean_goals), xlim = c(-0.5, 8), breaks = -1:9 + 0.5,
     main = "Random draw from a Poisson distribution with\nthe same mean as the distribution above.")

# 
m1_string <- "model {
for(i in 1:n_games) {
HomeGoals[i] ~ dpois(lambda_home[HomeTeam[i],AwayTeam[i]])
AwayGoals[i] ~ dpois(lambda_away[HomeTeam[i],AwayTeam[i]])
}
for(home_i in 1:n_teams) {
for(away_i in 1:n_teams) {
lambda_home[home_i, away_i] <- exp(baseline + skill[home_i] - skill[away_i])
lambda_away[home_i, away_i] <- exp(baseline + skill[away_i] - skill[home_i])
}
}
skill[1] <- 0
for(j in 2:n_teams) {
skill[j] ~ dnorm(group_skill, group_tau)
}
group_skill ~ dnorm(0, 0.0625)
group_tau <- 1 / pow(group_sigma, 2)
group_sigma ~ dunif(0, 3)
baseline ~ dnorm(0, 0.0625)
}"

# Compiling model 1
m1 <- jags.model(textConnection(m1_string), data = data_list, n.chains = 3,
                 n.adapt = 5000)
# Burning some samples on the altar of the MCMC god
update(m1, 5000)
# Generating MCMC samples
s1 <- coda.samples(m1, variable.names = c("baseline", "skill", "group_skill",
                                          "group_sigma"), n.iter = 10000, thin = 2)
# Merging the three MCMC chains into one matrix
ms1 <- as.matrix(s1)
s1
teams

plot(s1[, col_name("skill", which(teams == "Liverpool"))])

plot(s1[, col_name("skill", which(teams == "Manchester City"))])

plot(s1[, col_name("skill", which(teams == "Watford"))])

# model 2
m2_string <- "model {
for(i in 1:n_games) {
HomeGoals[i] ~ dpois(lambda_home[HomeTeam[i],AwayTeam[i]])
AwayGoals[i] ~ dpois(lambda_away[HomeTeam[i],AwayTeam[i]])
}
for(home_i in 1:n_teams) {
for(away_i in 1:n_teams) {
lambda_home[home_i, away_i] <-
exp( home_baseline + skill[home_i] - skill[away_i])

lambda_away[home_i, away_i] <-
exp( away_baseline + skill[away_i] - skill[home_i])
}
}
skill[1] <- 0
for(j in 2:n_teams) {
skill[j] ~ dnorm(group_skill, group_tau)
}
group_skill ~ dnorm(0, 0.0625)
group_tau <- 1/pow(group_sigma, 2)
group_sigma ~ dunif(0, 3)
home_baseline ~ dnorm(0, 0.0625)
away_baseline ~ dnorm(0, 0.0625)
}"

m2 <- jags.model(textConnection(m2_string), data = data_list, n.chains = 3,
                 n.adapt = 5000)
update(m2, 5000)
s2 <- coda.samples(m2, variable.names = c("home_baseline", "away_baseline",
                                          "skill", "group_sigma", "group_skill"), n.iter = 10000, thin = 2)
ms2 <- as.matrix(s2)

plotPost(exp(ms2[, "home_baseline"]) - exp(ms2[, "away_baseline"]), compVal = 0,
         xlab = "Home advantage in number of goals")


m3_string <- "model {
for(i in 1:n_games) {
HomeGoals[i] ~ dpois(lambda_home[Season[i], HomeTeam[i],AwayTeam[i]])
AwayGoals[i] ~ dpois(lambda_away[Season[i], HomeTeam[i],AwayTeam[i]])
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
home_baseline[season_i] ~ dnorm(home_baseline[season_i - 1], season_tau)
away_baseline[season_i] ~ dnorm(away_baseline[season_i - 1], season_tau)
}
season_tau <- 1/pow(season_sigma, 2)
season_sigma ~ dunif(0, 3)
}"

m3 <- jags.model(textConnection(m3_string), data = data_list, n.chains = 3,
                 n.adapt = 10000)
update(m3, 10000)
s3 <- coda.samples(m3, variable.names = c("home_baseline", "away_baseline",
                                          "skill", "season_sigma", "group_sigma", "group_skill"), n.iter = 90000,
                   thin = 8)
ms3 <- as.matrix(s3)


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


base_string_fixed <- "model {
for(i in 1:n_games) {
HomeGoals[i] ~ dpois(lambda_home[Season[i], HomeTeam[i], AwayTeam[i]])
AwayGoals[i] ~ dpois(lambda_away[Season[i], HomeTeam[i], AwayTeam[i]])
}
for(season_i in 1:n_seasons) {
for(home_i in 1:n_teams) {
for(away_i in 1:n_teams) {
lambda_home[season_i, home_i, away_i] <- exp(home_baseline[1] +
skill[season_i, home_i] - skill[season_i, away_i])
lambda_away[season_i, home_i, away_i] <- exp(away_baseline[1] +
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
home_baseline[season_i] <- home_baseline[1]
away_baseline[season_i] <- away_baseline[1]
}
season_tau <- 1/pow(season_sigma, 2)
season_sigma ~ dunif(0, 3)
}"



base <- jags.model(textConnection(base_string), data = data_list, n.chains = 3,
                 n.adapt = 10000)
update(base, 10000)
s4 <- coda.samples(base, variable.names = c("home_baseline", "away_baseline",
                                          "skill", "season_sigma", "group_sigma", "group_skill"), n.iter = 90000,
                   thin = 8)
bases3 <- as.matrix(s4)
bases3 <- output
##### plot home advantage

colnames(bases3)
home_base <- bases3[, str_detect(string = colnames(bases3), "home_baseline\\[")] 
away_base <- bases3[, str_detect(string = colnames(bases3), "away_baseline\\[")] 
home_advantage <- exp(home_base) - exp(away_base)
colnames(home_advantage) <- seasons %>% replace(., .=='PL9999', 'After COVID-19')

bases3[,'home_baseline']

par(mfrow = c(1,1))
par(mar = c(5, 5, 5, 5), xaxs = "i")
caterplot(home_advantage, labels.loc = "above", val.lim = c(-0.4, 0.9), reorder = FALSE)
title("Home advantage in 10seasons")

seasons
##### plot home advantage


par(mar = c(6, 6, 6, 6))
home.before.COVID.seasons <- colnames(bases3)[str_detect(colnames(bases3), 'home')] %>% head(., -1)
away.before.COVID.seasons <- colnames(bases3)[str_detect(colnames(bases3), 'away')] %>% head(., -1)
plotPost(exp(bases3[, home.before.COVID.seasons] %>% c()) - exp(bases3[, away.before.COVID.seasons] %>% c()), compVal = 0,
         xlab = "Home advantage in number of goals", xlim = c(-0.2, 0.6))
title('Before_COVID_19 (9 seasons)')

plotPost(exp(bases3[, "home_baseline[10]"]) - exp(bases3[, "away_baseline[10]"]), compVal = 0,
         xlab = "Home advantage in number of goals", xlim = c(-0.2, 0.6))
title('After_COVID_19')


save(bases3, file = 'PL_sampling.RData')
load('PL_sampling.RData')
head(bases3)

# The ranking of the teams for after COVID break.
team_skill <- bases3[, str_detect(string = colnames(bases3), "skill\\[10,")]
team_skill <- (team_skill - rowMeans(team_skill)) + bases3[, "home_baseline[10]"]
team_skill <- exp(team_skill)
colnames(team_skill) <- teams
team_skill <- team_skill[, order(colMeans(team_skill), decreasing = T)]
par(mfrow = c(1,1))
par(mar = c(2, 0.7, 0.7, 0.7), xaxs = "i")
caterplot(team_skill, labels.loc = "above", val.lim = c(0.7, 3.8))


# The ranking of the teams for the 2019/20 season.
team_skill <- bases3[, str_detect(string = colnames(bases3), "skill\\[8,")]
team_skill <- (team_skill - rowMeans(team_skill)) + bases3[, "home_baseline[8]"]
team_skill <- exp(team_skill)
colnames(team_skill) <- teams
team_skill <- team_skill[, order(colMeans(team_skill), decreasing = T)]
par(mfrow = c(1,1))
par(mar = c(2, 0.7, 0.7, 0.7), xaxs = "i")
caterplot(team_skill, labels.loc = "above", val.lim = c(0.7, 3.8))


##### predicting match result


n <- nrow(bases3)
bases3_pred <- sapply(1:nrow(PL), function(i) {
  home_team <- which(teams == PL$HomeTeam[i])
  away_team <- which(teams == PL$AwayTeam[i])
  season <- which(seasons == PL$Season[i])
  home_skill <- bases3[, col_name("skill", season, home_team)]
  away_skill <- bases3[, col_name("skill", season, away_team)]
  home_baseline <- bases3[, col_name("home_baseline", season)]
  away_baseline <- bases3[, col_name("away_baseline", season)]
  home_goals <- rpois(n, exp(home_baseline + home_skill - away_skill))
  away_goals <- rpois(n, exp(away_baseline + away_skill - home_skill))
  home_goals_table <- table(home_goals)
  away_goals_table <- table(away_goals)
  match_results <- sign(home_goals - away_goals)
  match_results_table <- table(match_results)
  mode_home_goal <- as.numeric(names(home_goals_table)[ which.max(home_goals_table)])
  mode_away_goal <- as.numeric(names(away_goals_table)[ which.max(away_goals_table)])
  match_result <- as.numeric(names(match_results_table)[which.max(match_results_table)])
  rand_i <- sample(seq_along(home_goals), 1)
  c(mode_home_goal = mode_home_goal, mode_away_goal = mode_away_goal, match_result = match_result,
    mean_home_goal = mean(home_goals), mean_away_goal = mean(away_goals),
    rand_home_goal = home_goals[rand_i], rand_away_goal = away_goals[rand_i],
    rand_match_result = match_results[rand_i])
})
bases3_pred <- t(bases3_pred)
bases3_pred[,'rand_match_result']

hist(bases3_pred[,'rand_match_result'])


sum(sign(tmp1[, 'HomeGoals'] - tmp1[, 'AwayGoals']) == tmp2[,'rand_match_result'])/nrow(tmp2)

tmp1 <- tail(PL, 500)
tmp2 <- tail(bases3_pred, 500)
tmp2[]

n <- nrow(ms3)
m3_pred <- sapply(1:nrow(PL), function(i) {
  home_team <- which(teams == PL$HomeTeam[i])
  away_team <- which(teams == PL$AwayTeam[i])
  season <- which(seasons == PL$Season[i])
  home_skill <- ms3[, col_name("skill", season, home_team)]
  away_skill <- ms3[, col_name("skill", season, away_team)]
  home_baseline <- ms3[, col_name("home_baseline", season)]
  away_baseline <- ms3[, col_name("away_baseline", season)]
  home_goals <- rpois(n, exp(home_baseline + home_skill - away_skill))
  away_goals <- rpois(n, exp(away_baseline + away_skill - home_skill))
  home_goals_table <- table(home_goals)
  away_goals_table <- table(away_goals)
  match_results <- sign(home_goals - away_goals)
  match_results_table <- table(match_results)
  mode_home_goal <- as.numeric(names(home_goals_table)[ which.max(home_goals_table)])
  mode_away_goal <- as.numeric(names(away_goals_table)[ which.max(away_goals_table)])
  match_result <- as.numeric(names(match_results_table)[which.max(match_results_table)])
  rand_i <- sample(seq_along(home_goals), 1)
  c(mode_home_goal = mode_home_goal, mode_away_goal = mode_away_goal, match_result = match_result,
    mean_home_goal = mean(home_goals), mean_away_goal = mean(away_goals),
    rand_home_goal = home_goals[rand_i], rand_away_goal = away_goals[rand_i],
    rand_match_result = match_results[rand_i])
})
m3_pred <- t(m3_pred)



sum(sign(tmp1[, 'HomeGoals'] - tmp1[, 'AwayGoals']) == tmp2[,'rand_match_result'])/nrow(tmp2)






mean(PL$MatchResult == bases3_pred[, 'match_result'])
mean(PL$MatchResult == m3_pred[, 'match_result'])
head(bases3_pred)

aa <- bases3_pred %>% as.data.frame() %>% 
  mutate(sub = mean_home_goal - mean_away_goal) %>% 
  mutate(match_result = case_when(abs(sub) < 0.20 ~ 0, sub >= 0.20 ~ 1, sub <= -0.20 ~ -1)) %>% select(match_result)
mean(PL$MatchResult == aa)

bb <- bases3_pred %>% as.data.frame() %>% select(match_result)


##### 가까운 20경기 ??????
temp <- cbind(tail(PL, 20),tail(bases3_pred[, c('mean_home_goal', 'mean_away_goal')], 20))
tmp <- temp %>% select(-c(Season)) %>%
  rename(c('mean_home_goal' = 'lambda_home_goal', 'mean_away_goal' = 'lambda_away_goal')) %>% 
  mutate(Estimated1 = pull(tail(bb, 20)), Estimated2 = pull(tail(aa,20)))

write.csv(tmp, 'predict_example.csv')


mean(PL$MatchResult == aa)
mean(PL$MatchResult == bb)


PL.new
############### 못본경기 ??????

n <- nrow(bases3)
bases.new_pred <- sapply(1:nrow(PL.new), function(i) {
  home_team <- which(teams == PL.new$HomeTeam[i])
  away_team <- which(teams == PL.new$AwayTeam[i])
  season <- which(seasons == PL.new$Season[i])
  home_skill <- bases3[, col_name("skill", season, home_team)]
  away_skill <- bases3[, col_name("skill", season, away_team)]
  home_baseline <- bases3[, col_name("home_baseline", season)]
  away_baseline <- bases3[, col_name("away_baseline", season)]
  home_goals <- rpois(n, exp(home_baseline + home_skill - away_skill))
  away_goals <- rpois(n, exp(away_baseline + away_skill - home_skill))
  home_goals_table <- table(home_goals)
  away_goals_table <- table(away_goals)
  match_results <- sign(home_goals - away_goals)
  match_results_table <- table(match_results)
  mode_home_goal <- as.numeric(names(home_goals_table)[ which.max(home_goals_table)])
  mode_away_goal <- as.numeric(names(away_goals_table)[ which.max(away_goals_table)])
  match_result <- as.numeric(names(match_results_table)[which.max(match_results_table)])
  rand_i <- sample(seq_along(home_goals), 1)
  c(mode_home_goal = mode_home_goal, mode_away_goal = mode_away_goal, match_result = match_result,
    mean_home_goal = mean(home_goals), mean_away_goal = mean(away_goals),
    rand_home_goal = home_goals[rand_i], rand_away_goal = away_goals[rand_i],
    rand_match_result = match_results[rand_i])
})
bases.new_pred <- t(bases.new_pred)
bases.new_pred

Estimated1 <- sign(bases.new_pred[, 'mean_home_goal']-bases.new_pred[, 'mean_away_goal'])
Estimated2 <- bases.new_pred %>% as.data.frame() %>% 
  mutate(sub = mean_home_goal - mean_away_goal) %>% 
  mutate(match_result = case_when(abs(sub) < 0.20 ~ 0, sub >= 0.20 ~ 1, sub <= -0.20 ~ -1)) %>% 
  select(match_result) %>% pull

tmp2 <- cbind(PL.new %>% select(c('HomeGoals', 'AwayGoals', 'HomeTeam', 'AwayTeam', 'MatchResult')),
      bases.new_pred[,c('mean_home_goal', 'mean_away_goal')] %>%
        rename(c('mean_home_goal' = 'lambda_home_goal', 'mean_away_goal' = 'lambda_away_goal')),
      Estimated1, Estimated2)

write.csv(tmp2, 'predict_example2.csv')


PL.new$MatchResult <- sign(PL.new$HomeGoals - PL.new$AwayGoals)
tmp <- as.data.frame(tmp)

sum(tmp$MatchResult == Estimated1)/nrow(tmp)
sum(tmp$MatchResult == Estimated2)/nrow(tmp)

bases.new_pred
home
teams
seasons
n <- 32750
home_team <- which(teams == 'Leicester')
away_team <- which(teams == 'Manchester City')
season <- which(seasons == 'PL2021')
home_skill <- bases3[, col_name("skill", season, home_team)]
away_skill <- bases3[, col_name("skill", season, away_team)]
home_baseline <- bases3[, col_name("home_baseline", season)]
away_baseline <- bases3[, col_name("away_baseline", season)]
home_goals <- rpois(n, exp(home_baseline + home_skill - away_skill))
away_goals <- rpois(n, exp(away_baseline + away_skill - home_skill))

hist(home_goals, breaks = (-1:11) + 0.5, xlim = c(-0.5, 11),
     main = 'Distribution of the number of home goals (Leicester)')
hist(away_goals, breaks = (-1:11) + 0.5, xlim = c(-0.5, 11),
     main = 'Distribution of the number of away goals (Manchester City)')
hist(away_goals)


mean(home_skill)
mean(away_skill)
mean(home_baseline)
mean(away_baseline)


aa <- home_goals - away_goals
sum(sign(aa) == 1)/length(aa)
sum(sign(aa) == 0)/length(aa)
sum(sign(aa) == -1)/length(aa)

hist(sign(aa), breaks = c(-3:2)+0.5, xlab = '경기결과', main = '경기결과 Histogram')


tmp <- cbind(home_goals, away_goals)
tmp <- tmp %>% as.data.frame() %>% unite(result, home_goals, away_goals, sep = ':')
tmp <- cbind(tmp, home_goals, away_goals)
tmp %>% as.data.frame %>% count(result)
barplot(table(tmp$result))

tmp <- tmp %>% as.tibble()
aa <- tmp %>% mutate(temp = paste(home_goals,away_goals,sep=':')) %>% select(temp) %>% type.convert() %>% 
  group_by(temp) %>% dplyr::summarize(n=n()) %>% arrange(desc(n)) %>% slice(.,(1:10))
barplot(pull(aa['n']), main = '경기결과', names=pull(aa['temp']), ylim = c(0,5000))


path
#######################################

Sampling <- function(path, league_n, mode=0){
  
  print(paste0('start_', league_n))
  filename1 <- paste0(path, '/Before_COVID_', league_n, '.csv')
  filename2 <- paste0(path, '/After_COVID_', league_n, '.csv')
  COVID_season <- paste0(league_n, '1920')
  
  Before_COVID <- read_csv(filename1)
  After_COVID <- read_csv(filename2)
  After_COVID <- After_COVID %>% filter(season == COVID_season) %>% mutate(season = rep('PL9999'))
  total_dt <- rbind(Before_COVID, After_COVID)
  dt <- total_dt %>% rename(c('full_home_score' = 'HomeGoals', 'full_away_score' = 'AwayGoals',
                              'home' = 'HomeTeam', 'away' = 'AwayTeam', 'season' = 'Season'))
  dt$MatchResult <- sign(dt$HomeGoals - dt$AwayGoals)
  dt <- dt %>% select(HomeGoals, AwayGoals, Season, HomeTeam, AwayTeam, date_no)
  d <- na.omit(dt)
  teams <- unique(c(d$HomeTeam %>% as.character(), d$AwayTeam %>% as.character()))
  seasons <- unique(d$Season)
  
  data_list <- list(HomeGoals = d$HomeGoals, AwayGoals = d$AwayGoals,
                    HomeTeam = as.numeric(factor(d$HomeTeam %>% as.character(), levels = teams)),
                    AwayTeam = as.numeric(factor(d$AwayTeam %>% as.character(), levels = teams)),
                    Season = as.numeric(factor(d$Season, levels = seasons)), n_teams = length(teams),
                    n_games = nrow(d), n_seasons = length(seasons))
  col_name <- function(name, ...) {
    paste0(name, "[", paste(..., sep = ","), "]")
  }
  if (mode == 0) {
    model = base_string
    n_tag = "indep"
  }
  else {
    model = base_string_fixed
    n_tag = "fixed"
  }
  base <- jags.model(textConnection(model), data = data_list, n.chains = 3,
                     n.adapt = 10000)
  update(base, 10000)
  samp <- coda.samples(base, variable.names = c("home_baseline", "away_baseline",
                                              "skill", "season_sigma", "group_sigma", "group_skill"), n.iter = 90000,
                     thin = 8)
  bases <- as.matrix(samp)
  print(paste('Done', league_n, n_tag, 'sampling', sep='_'))
  file_n <- paste0(league_n, '_sampling_', n_tag, '.RData' )
  save(bases, file = file_n)
  print(paste('Done_', file_n, 'save'))
  return(bases)
}

path <- getwd()
output0 <- Sampling(path = path, league_n = 'PL', mode=0)
output <- Sampling(path = path, league_n = 'PL', mode=1) 
output2 <- Sampling(path = path, league_n = 'LIGA', mode=0) 
output3 <- Sampling(path = path, league_n = 'LIGA', mode=1)
output4 <- Sampling(path = path, league_n = 'SE', mode=0) 
output5 <- Sampling(path = path, league_n = 'SE', mode=1)
output6 <- Sampling(path = path, league_n = 'BL', mode=0) 
output7 <- Sampling(path = path, league_n = 'BL', mode=1)

colnames(output)

team_label <- function(path, league_n, mode=0){
  
  print(paste0('start_', league_n))
  filename1 <- paste0(path, '/Before_COVID_', league_n, '.csv')
  filename2 <- paste0(path, '/After_COVID_', league_n, '.csv')
  
  Before_COVID <- read_csv(filename1)
  After_COVID <- read_csv(filename2)
  After_COVID <- After_COVID %>% mutate(season = rep('PL9999'))
  #total_dt <- rbind(Before_COVID, After_COVID)
  total_dt <- Before_COVID
  dt <- total_dt %>% rename(c('full_home_score' = 'HomeGoals', 'full_away_score' = 'AwayGoals',
                              'home' = 'HomeTeam', 'away' = 'AwayTeam', 'season' = 'Season'))
  dt$MatchResult <- sign(dt$HomeGoals - dt$AwayGoals)
  dt <- dt %>% select(HomeGoals, AwayGoals, Season, HomeTeam, AwayTeam, date_no)
  d <- na.omit(dt)
  teams <- unique(c(d$HomeTeam %>% as.character(), d$AwayTeam %>% as.character()))
  seasons <- unique(d$Season)
  return(list(teams, seasons))
}


pl <- team_label(path, 'PL', mode=1)[[1]]
se <- team_label(path, 'SE', mode=1)[[1]]
liga <- team_label(path, 'LIGA', mode=1)[[1]]
bl <- team_label(path, 'BL', mode=1)[[1]]

team_name_list <- list(pl, liga, se, bl)
save(team_name_list, file = 'team_name_list_new.RData')

?save
for (i in 1:length(colnames(output))) {
  temp = colnames(output)[i]
  if (str_detect(temp, 'skill') == TRUE) {
    temp
  }
}

str_detect('adf', 'ad')
colnames(output)[1] <- 'away_baseline'
output
'ads' %in% 'adasdfas'
output0 <- load('PL_sampling.RData')
output0 <- bases3


write.csv(output0, file = 'PL_indep_samples_new.csv')
write.csv(output, file = 'PL_fixed_samples_new.csv')
write.csv(output2, file = 'LIGA_indep_samples_new.csv')
write.csv(output3, file = 'LIGA_fixed_samples_new.csv')
write.csv(output4, file = 'SE_indep_samples_new.csv')
write.csv(output5, file = 'SE_fixed_samples_new.csv')
write.csv(output6, file = 'BL_indep_samples_new.csv')
write.csv(output7, file = 'BL_fixed_samples_new.csv')





Before_COVID_PL <- read_csv('Before_COVID_PL.csv')
After_COVID_PL <- read_csv('After_COVID_PL.csv')
After_COVID_PL <- After_COVID_PL %>% mutate(season = rep('PL9999'))
PL_total <- rbind(Before_COVID_PL, After_COVID_PL)
dim(Before_COVID_PL)
dim(After_COVID_PL)

colnames(Before_COVID_PL)
colnames(After_COVID_PL)
# PL.new <- PL %>% filter(date_no > 18707)
PL <- PL_total %>% rename(c('full_home_score' = 'HomeGoals', 'full_away_score' = 'AwayGoals',
                            'home' = 'HomeTeam', 'away' = 'AwayTeam', 'season' = 'Season'))

# -1 = Away win, 0 = Draw, 1 = Home win
PL$MatchResult <- sign(PL$HomeGoals - PL$AwayGoals)
PL <- PL %>% select(HomeGoals, AwayGoals, Season, HomeTeam, AwayTeam, date_no)

# Creating a data frame d with only the complete match results
d <- na.omit(PL)
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

# 축구 결과가 ????????? 분포??? ???르는 지 
par(mfcol = c(2, 1), mar = rep(2.2, 4))
hist(c(d$AwayGoals, d$HomeGoals), xlim = c(-0.5, 8), breaks = -1:9 + 0.5,
     main = "Distribution of the number of goals\nscored by a team in a match.")
mean_goals <- mean(c(d$AwayGoals, d$HomeGoals))
hist(rpois(9999, mean_goals), xlim = c(-0.5, 8), breaks = -1:9 + 0.5,
     main = "Random draw from a Poisson distribution with\nthe same mean as the distribution above.")





