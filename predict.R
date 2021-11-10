############## Modeling Match results in Soccer using a Hierarchical bayesian poisson model
getwd()
setwd('C:/Users/jaemini_man/Desktop/GitHub/soccer/soccer')

### install packages
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


PL_b <- read_csv('Before_COVID_PL.csv')
PL_a <- read_csv('After_COVID_PL.csv')
PL_b <- PL_b %>% rename(c('full_home_score' = 'HomeGoals', 'full_away_score' = 'AwayGoals',
                  'home' = 'HomeTeam', 'away' = 'AwayTeam', 'season' = 'Season'))
PL_a <- PL_a %>% rename(c('HomeGoals' = 'full_home_score', 'AwayGoals' = 'full_away_score',
                          'HomeTeam' = 'home', 'AwayTeam' = 'away', 'Season' = 'season'))
write_csv(PL_a, file = 'After_COVID_PL.csv')


PL_df <- rbind(PL_b, PL_a)
PL_df %>% select(date) %>% tail(10)
length(PL_a$date_no)*0.3                 


BL_a <- read_csv('After_COVID_BL.csv')
length(BL_a$date_no) 

# 18628 1/1
PL_df %>% filter(date_no>18628) %>% select(HomeTeam,AwayTeam)


### load data 
Load_Data <- function(league_name, date_filter=FALSE) {
  Before_COVID <- read_csv(paste0('Before_COVID_', league_name, '.csv'))
  After_COVID <- read_csv(paste0('After_COVID_', league_name, '.csv'))
  After_COVID <- After_COVID %>% mutate(season = rep(paste0(league_name, '9999')))
  total <- rbind(Before_COVID, After_COVID)
  df <- total %>% rename(c('full_home_score' = 'HomeGoals', 'full_away_score' = 'AwayGoals',
                           'home' = 'HomeTeam', 'away' = 'AwayTeam', 'season' = 'Season'))
  if (date_filter != FALSE) {
    df <- df %>% filter(date_no < date_filter)
  }
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
  return(list(data_list, teams, seasons, df))
}
Load_Data2 <- function(league_name, date_filter=FALSE) {
  Before_COVID <- read_csv(paste0('Before_COVID_', league_name, '.csv'))
  After_COVID <- read_csv(paste0('After_COVID_', league_name, '.csv'))
  After_COVID <- After_COVID %>% mutate(season = rep(paste0(league_name, '9999')))
  total <- rbind(Before_COVID, After_COVID)
  df <- total %>% rename(c('full_home_score' = 'HomeGoals', 'full_away_score' = 'AwayGoals',
                           'home' = 'HomeTeam', 'away' = 'AwayTeam', 'season' = 'Season'))
  if (date_filter != FALSE) {
    df <- df %>% filter(date_no >= date_filter)
  }
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
  return(list(data_list, teams, seasons, df))
}
PL_train <- Load_Data("PL", date_filter = 18628)
BL_train <- Load_Data("BL", date_filter = 18628)
LIGA_train <- Load_Data("LIGA", date_filter = 18628)
SE_train <- Load_Data('SE', date_filter = 18628)

PL_test <- Load_Data2("PL", date_filter = 18628)
BL_test <- Load_Data2("BL", date_filter = 18628)
LIGA_test <- Load_Data2("LIGA", date_filter = 18628)
SE_test <- Load_Data2('SE', date_filter = 18628)
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

###### PL sampling
PL_train_model <- jags.model(textConnection(base_string), data = PL_train[[1]], n.chains = 3, n.adapt = 10000)
update(PL_train_model, 10000)
PL_train_sample <- coda.samples(PL_train_model, variable.names = c("home_baseline", "away_baseline",
                                                                       "skill", "season_sigma", "group_sigma", 
                                                                       "group_skill"), n.iter = 90000, thin = 8)
PL_train_sample <- as.matrix(PL_train_sample)
save(PL_train_sample, file = 'PL_train_sample.RData')


###### BL sampling
BL_train_model <- jags.model(textConnection(base_string), data = BL_train[[1]], n.chains = 3, n.adapt = 10000)
update(BL_train_model, 10000)
BL_train_sample <- coda.samples(BL_train_model, variable.names = c("home_baseline", "away_baseline",
                                                       "skill", "season_sigma", "group_sigma", 
                                                       "group_skill"), n.iter = 90000, thin = 8)
BL_train_sample <- as.matrix(BL_train_sample)
save(BL_train_sample, file = 'BL_train_sample.RData')

###### SE sampling
SE_train_model <- jags.model(textConnection(base_string), data = SE_train[[1]], n.chains = 3, n.adapt = 10000)
update(SE_train_model, 10000)
SE_train_sample <- coda.samples(SE_train_model, variable.names = c("home_baseline", "away_baseline",
                                                       "skill", "season_sigma", "group_sigma", 
                                                       "group_skill"), n.iter = 90000, thin = 8)
SE_train_sample <- as.matrix(SE_train_sample)
save(SE_train_sample, file = 'SE_train_sample.RData')


###### LIGA sampling
LIGA_train_model <- jags.model(textConnection(base_string), data = LIGA_train[[1]], n.chains = 3, n.adapt = 10000)
update(LIGA_train_model, 10000)
LIGA_train_sample <- coda.samples(LIGA_train_model, variable.names = c("home_baseline", "away_baseline",
                                                           "skill", "season_sigma", "group_sigma", 
                                                           "group_skill"), n.iter = 90000, thin = 8)
LIGA_train_sample <- as.matrix(LIGA_train_sample)
save(LIGA_train_sample, file = 'LIGA_train_sample.RData')

############## predict_match_result

Predict_Match_Result <- function(samples, train_df, test_df, draw_gap=0) {
  n <- nrow(samples)
  bases.new_pred <- sapply(1:nrow(test_df[[4]]), function(i) {
    home_team <- which(train_df[[2]] == test_df[[4]]$HomeTeam[i])
    away_team <- which(train_df[[2]] == test_df[[4]]$AwayTeam[i])
    season <- which(train_df[[3]] == test_df[[4]]$Season[i])
    home_skill <- samples[, col_name("skill", season, home_team)]
    away_skill <- samples[, col_name("skill", season, away_team)]
    home_baseline <- samples[, col_name("home_baseline", season)]
    away_baseline <- samples[, col_name("away_baseline", season)]
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
  bases.new_pred <- bases.new_pred %>% as.matrix() %>% t()
  
  Estimated <- bases.new_pred %>% as.data.frame() %>% 
    mutate(sub = mean_home_goal - mean_away_goal) %>%
    mutate(match_result = case_when(abs(sub) < draw_gap ~ 0, sub >= draw_gap ~ 1, sub <= -draw_gap ~ -1)) %>% 
    select(match_result) %>% pull()

  tmp <- sign(test_df[[4]]$HomeGoals - test_df[[4]]$AwayGoals)
  accuarcy <- sum(tmp == Estimated)/length(tmp)
  print(paste0('draw_gap = ', draw_gap, ' acc = ', round(accuarcy, 3), ' n_match: ', length(tmp)))
  print(sum(tmp == Estimated))
  return(list(Estimated, tmp))
}

load('PL_train_sample.RData')
load('BL_train_sample.RData')
load('SE_train_sample.RData')
load('LIGA_train_sample.RData')




result_PL <- Predict_Match_Result(PL_train_sample, train_df = PL_train, test_df = PL_train, 0.2)
result_BL <- Predict_Match_Result(BL_train_sample, train_df = BL_train, test_df = BL_train, 0.2)
result_SE <- Predict_Match_Result(SE_train_sample, train_df = SE_train, test_df = SE_train, 0.2)
result_LIGA <- Predict_Match_Result(LIGA_train_sample, train_df = LIGA_train, test_df = LIGA_train, 0.2)
78+65+81+71
74+60+77+67
72+51+78+66
152+117+139+125

result_PL <- Predict_Match_Result(PL_train_sample, train_df = PL_train, test_df = PL_test, 0.2)
result_BL <- Predict_Match_Result(BL_train_sample, train_df = BL_train, test_df = BL_test, 0.2)
result_SE <- Predict_Match_Result(SE_train_sample, train_df = SE_train, test_df = SE_test, 0.2)
result_LIGA <- Predict_Match_Result(LIGA_train_sample, train_df = LIGA_train, test_df = LIGA_test, 0.2)

1952 + 1478 + 1946 + 1917
3574 + 2871 + 3557 + 3575
7293/13577  
  
267/533
278/533
num[168]

tmp2 <- cbind(LIGA_test[[4]] %>% select(c('HomeGoals', 'AwayGoals', 'HomeTeam', 'AwayTeam', 'MatchResult')),
              bases.new_pred[,c('mean_home_goal', 'mean_away_goal')] %>%
                rename(c('mean_home_goal' = 'lambda_home_goal', 'mean_away_goal' = 'lambda_away_goal')))

LIGA_test

write.csv(tmp2, 'predict_example2.csv')


match_results1 <- PL_train[[1]]$HomeGoals - PL_train[[1]]$AwayGoals
match_results2 <- LIGA_train[[1]]$HomeGoals - LIGA_train[[1]]$AwayGoals
match_results3 <- BL_train[[1]]$HomeGoals - BL_train[[1]]$AwayGoals
match_results4 <- SE_train[[1]]$HomeGoals - SE_train[[1]]$AwayGoals
match_results <- c(match_results1, match_results2, match_results3, match_results4)

sum(sign(match_results) == 0)
sum(sign(match_results) == 0)/length(match_results)


result_PL <- Predict_Match_Result(PL_train_sample, train_df = PL_train, test_df = PL_train, 0)
result_BL <- Predict_Match_Result(BL_train_sample, train_df = BL_train, test_df = BL_train, 0)
result_SE <- Predict_Match_Result(SE_train_sample, train_df = SE_train, test_df = SE_train, 0)
result_LIGA <- Predict_Match_Result(LIGA_train_sample, train_df = LIGA_train, test_df = LIGA_train, 0)


hist(result_PL[[1]])
delta_lambda <- c(result_PL[[1]], result_BL[[1]], result_LIGA[[1]], result_SE[[1]])

h <- hist((c(result_PL[[1]], result_BL[[1]], result_LIGA[[1]], result_SE[[1]])))

h <- hist(delta_lambda)
lines(x, dnorm(x, mean(delta_lambda), sd(delta_lambda))*diff(h$mids[1:2])*length(delta_lambda), type = "l", col = 'red')

hist(abs(delta_lambda), main = '')

mean(delta_lambda)
sd(delta_lambda)


x <- seq(-4, 4, 0.1)
plot(x, dnorm(x, mean(delta_lambda), var(delta_lambda)), type = "l")

rnorm(10000)

hist(PL_train_sample)

num <- seq(0.1, 0.3, 0.001)

pnorm(0+num, mean(delta_lambda), var(delta_lambda)) - pnorm(0-num, mean(delta_lambda), var(delta_lambda))

hist(abs(delta_lambda))
lines(abs)

num[178]

pnorm(mean(delta_lambda), mean(delta_lambda), sd(delta_lambda)) 

r1 <- sign(PL_train[[1]]$HomeGoals - PL_train[[1]]$AwayGoals)
r2 <- sign(BL_train[[1]]$HomeGoals - BL_train[[1]]$AwayGoals)
r3 <- sign(LIGA_train[[1]]$HomeGoals - LIGA_train[[1]]$AwayGoals)
r4 <- sign(SE_train[[1]]$HomeGoals - SE_train[[1]]$AwayGoals)
r_all <- c(r1, r2, r3, r4)
r_all
delta_lambda
df <- NULL
df$a <- r_all
df$b <- delta_lambda
df <- as.data.frame(df)
df %>% filter(abs(b) < 0.13) %>% filter(a==0) %>% nrow()
df %>% filter(abs(b) < 0.13) %>% nrow()

home <- exp(0.2622+0.1428-0.2862)
away <- exp(0.1691-0.1428+0.2862)

hist(rpois(10000, home), breaks = -1:9 + 0.5)
hist(rpois(10000, away), breaks = -1:9 + 0.5)

teams <- PL_train[[2]]
seasons <- PL_train[[3]]
home_team <- which(teams == 'Leicester')
away_team <- which(teams == 'Manchester City')
season <- which(seasons == 'PL9999')
home_skill <- PL_train_sample[, col_name("skill", season, home_team)]
away_skill <- PL_train_sample[, col_name("skill", season, away_team)]
home_baseline <- PL_train_sample[, col_name("home_baseline", season)]
away_baseline <- PL_train_sample[, col_name("away_baseline", season)]
home_goals <- rpois(n, exp(home_baseline + home_skill - away_skill))
away_goals <- rpois(n, exp(away_baseline + away_skill - home_skill))


mean(home_skill)
mean(away_skill)
mean(home_baseline)
mean(away_baseline)

# The ranking of the teams for after COVID break.
team_skill <- PL_train_sample[, str_detect(string = colnames(PL_train_sample), "skill\\[10,")] + home_baseline - mean(home_baseline)
#team_skill <- (team_skill - rowMeans(team_skill)) + PL_train_sample[, "home_baseline[10]"]
#team_skill <- exp(team_skill)
colnames(team_skill) <- teams
team_skill <- team_skill[, order(colMeans(team_skill), decreasing = T)]
par(mfrow = c(1,1))
par(mar = c(2, 0.7, 0.7, 0.7), xaxs = "i")
team_skill = team_skill[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,31,32)]
caterplot(team_skill, labels.loc = "above", val.lim = c(-1, 0.5))


# The ranking of the teams for after COVID break.
team_skill <- PL_train_sample[, str_detect(string = colnames(PL_train_sample), "skill\\[8,")] + home_baseline - mean(home_baseline)
#team_skill <- (team_skill - rowMeans(team_skill)) + PL_train_sample[, "home_baseline[10]"]
#team_skill <- exp(team_skill)
colnames(team_skill) <- teams
team_skill <- team_skill[, order(colMeans(team_skill), decreasing = T)]
par(mfrow = c(1,1))
par(mar = c(2, 0.7, 0.7, 0.7), xaxs = "i")
team_skill = team_skill[,c(1,2,3,4,5,6,7,8,10,11,13,14,15,16,17,18,19,32,34,35)]
caterplot(team_skill, labels.loc = "above", val.lim = c(-1, 0.5))

?caterplot
colnames(team_skill)

length(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,31,32))


n = 10000
home_goals <- rpois(n, exp(home_baseline + home_skill - away_skill))
away_goals <- rpois(n, exp(away_baseline + away_skill - home_skill))

hist(home_goals, breaks = (-1:11) + 0.5, xlim = c(-0.5, 11),
     main = 'Distribution of the number of home goals (Leicester)')
hist(away_goals, breaks = (-1:11) + 0.5, xlim = c(-0.5, 11),
     main = 'Distribution of the number of away goals (Manchester City)')
hist(away_goals)

mean(exp(home_baseline + home_skill - away_skill))
mean(exp(away_baseline + away_skill - home_skill))


tmp <- cbind(home_goals, away_goals)
tmp <- tmp %>% as.data.frame() %>% unite(result, home_goals, away_goals, sep = ':')
tmp <- cbind(tmp, home_goals, away_goals)
tmp %>% as.data.frame() %>% count(., )
barplot(table(tmp$result))

tmp <- tmp %>% as.tibble()
aa <- tmp %>% mutate(temp = paste(home_goals,away_goals,sep=':')) %>% select(temp) %>% type.convert() %>% 
  group_by(temp) %>% dplyr::summarize(n=n()) %>% arrange(desc(n)) %>% slice(.,(1:10))
barplot(pull(aa['n']), main = '경기결과', names=pull(aa['temp']), ylim = c(0,5000))
aa
tmp$result %>% count()

