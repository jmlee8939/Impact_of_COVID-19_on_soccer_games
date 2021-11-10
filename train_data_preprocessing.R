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
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(magrittr)
output0 <- read_csv('PL_indep_samples_new.csv')
output <- read_csv('PL_fixed_samples_new.csv')
output2 <- read_csv('LIGA_indep_samples_new.csv')
output2 <- read_csv('LIGA_fixed_samples_new.csv')
output4 <- read_csv('SE_indep_samples_new.csv')
output4 <- read_csv('SE_fixed_samples_new.csv')
output6 <- read_csv('BL_indep_samples_new.csv')
output6 <- read_csv('BL_fixed_samples_new.csv')

abc[[1]]

dim(colMeans(output0) %>% as.matrix() %>% t())

abc <- list()
abc[[1]] <- colMeans(output0)
abc[[2]] <- colMeans(output)
abc[[3]] <- colMeans(output2)
abc[[4]] <- colMeans(output3)
abc[[5]] <- colMeans(output4)
abc[[6]] <- colMeans(output5)
abc[[7]] <- colMeans(output6)
abc[[8]] <- colMeans(output7)

load('data_fixed.rdata')
load('data_indep.rdata')
getwd()

data_fixed %>% filter(league=='LIGA') %>% filter(season == 1819)
data_indep %>% filter(league=='LIGA') %>% filter(season == 1920)

b_pl <- read_csv('Before_COVID_BL.csv')
b_pl <- b_pl %>% 
  select(season, home, away, full_home_score, full_away_score) %>%
  mutate(result = sign(full_home_score-full_away_score))
b_pl$season <- sapply(b_pl %>% select(season), function(x){as.character(as.numeric(substr(x,3,6)) - 101)})
b_pl$season %>% unique()

t_b_pl <- b_pl %>% filter(season != '1011')
aa <- t_b_pl[2,'home'] %>% pull()

t_final_dt <- c()
for (i in 1:nrow(t_b_pl)) {
  h_team <- t_b_pl[i,'home'] %>% pull()
  a_team <- t_b_pl[i,'away'] %>% pull()
  if (a_team == "Atletico") {
    print('skip')
  }
  else {
    try({
      t_season <- t_b_pl[i, 'season'] %>% pull() %>% c()
      home_dt_indep <- data_indep %>% 
        filter(team_name == h_team & season == t_season) %>% 
        select(team_name, P, W, D, L, GF, GA, GD, Pts, raised, home_baseline, away_baseline, skill, HA) %>% 
        set_colnames(c('home_team', 'P_h', 'W_h', 'D_h', 'L_h', 'GF_h', 'GA_h', 'GD_h', 'Pts_h', 'raised_h', 'home_bs', 'away_bs', 'h_skill', 'HA'))
      away_dt_indep <- data_indep %>% filter(team_name == a_team & season == t_season) %>% 
        select(team_name, P, W, D, L, GF, GA, GD, Pts, raised, skill) %>% 
        set_colnames(c('away_team', 'P_a', 'W_a', 'D_a', 'L_a', 'GF_a', 'GA_a', 'GD_a', 'Pts_a', 'raised_a', 'a_skill'))
      home_dt_fixed <- data_fixed %>% 
        filter(team_name == h_team & season == t_season) %>% 
        select(team_name, home_baseline, away_baseline, skill, HA) %>% 
        set_colnames(c('home_team_fixed', 'home_bs_fixed', 'away_bs_fixed', 'h_skill_fixed', 'HA_fixed'))
      away_dt_fixed <- data_fixed %>% 
        filter(team_name == a_team & season == t_season) %>% 
        select(team_name, skill) %>% 
        set_colnames(c('away_team_fixed', 'a_skill_fixed'))
      t_dt <- cbind(t_b_pl[i,], home_dt_indep, away_dt_indep, home_dt_fixed, away_dt_fixed)
      t_final_dt <- rbind(t_final_dt, t_dt)
      print(paste0(i, '/', nrow(t_b_pl)))}
    )
  }
}
write_csv(t_final_dt, 'BL_preprocessed_new.csv')
#####################################################

load('sample_mean.rdata')
load('team_name_list.rdata')
load('team_name_list_new.rdata')

################################################


pl_test_indep <- data_indep %>% filter(season=='1920' & league=='PL')
pl_test_fixed <- data_fixed %>% filter(season=='1920' & league=='PL')

pl_sam_ind <- as.data.frame(abc[[1]]) %>% t()
pl_sam_ind <- as.data.frame(pl_sam_ind)

pl_sam_fix <- as.data.frame(abc[[2]]) %>% t()
pl_sam_fix <- as.data.frame(pl_sam_fix)
team_name_list[[1]] 

temp_dd <- c()
for (i in 1:nrow(pl_test_indep)) {
  team_n <- pl_test_indep$team_name[i] %>% c() 
  if (team_n == 'Leeds') {
  } else {
  team_idx <- which(team_name_list[[1]] == team_n)
  col_n <- paste0('skill[10,',team_idx,']')
  skill <- pl_sam_ind %>% select(col_n) %>% pull() %>% c()
  home_b <- pl_sam_ind %>% select('home_baseline[10]') %>% pull() %>% c()
  away_b <- pl_sam_ind %>% select('away_baseline[10]') %>% pull() %>% c()
  pl_test_indep[i,'skill'] <- skill
  pl_test_indep[i,'home_baseline'] <- home_b
  pl_test_indep[i,'away_baseline'] <- away_b
  pl_test_indep[i,'HA'] <- exp(home_b) - exp(away_b)
  }
}


for (i in 1:nrow(pl_test_fixed)) {
  team_n <- pl_test_fixed$team_name[i] %>% c() 
  if (team_n == 'Leeds') {
  } else {
  team_idx <- which(team_name_list[[1]] == team_n)
  col_n <- paste0('skill[10,',team_idx,']')
  skill <- pl_sam_fix %>% select(col_n) %>% pull() %>% c()
  home_b <- pl_sam_fix %>% select('home_baseline[1]') %>% pull() %>% c()
  away_b <- pl_sam_fix %>% select('away_baseline[1]') %>% pull() %>% c()
  pl_test_fixed[i,'skill'] <- skill
  pl_test_fixed[i,'home_baseline'] <- home_b
  pl_test_fixed[i,'away_baseline'] <- away_b
  pl_test_fixed[i,'HA'] <- exp(home_b) - exp(away_b)
  }
}

pl_test_indep
pl_test_fixed
######################################LIGA


liga_test_indep <- data_indep %>% filter(season=='1920' & league=='LIGA')
liga_test_fixed <- data_fixed %>% filter(season=='1920' & league=='LIGA')

liga_sam_ind <- as.data.frame(abc[[3]]) %>% t()
liga_sam_ind <- as.data.frame(liga_sam_ind)

liga_sam_fix <- as.data.frame(abc[[4]]) %>% t()
liga_sam_fix <- as.data.frame(liga_sam_fix)

for (i in 1:nrow(liga_test_indep)) {
  team_n <- liga_test_indep$team_name[i] %>% c()
  if (team_n %in% team_name_list[[2]]) {
    team_idx <- which(team_name_list[[2]] == team_n)
    col_n <- paste0('skill[10,',team_idx,']')
    skill <- liga_sam_ind %>% select(col_n) %>% pull() %>% c()
    home_b <- liga_sam_ind %>% select('home_baseline[10]') %>% pull() %>% c()
    away_b <- liga_sam_ind %>% select('away_baseline[10]') %>% pull() %>% c()
    liga_test_indep[i,'skill'] <- skill
    liga_test_indep[i,'home_baseline'] <- home_b
    liga_test_indep[i,'away_baseline'] <- away_b
    liga_test_indep[i,'HA'] <- exp(home_b) - exp(away_b)
  }
}


for (i in 1:nrow(liga_test_fixed)) {
  team_n <- liga_test_fixed$team_name[i] %>% c()
  if (team_n %in% team_name_list[[2]]) {
    team_idx <- which(team_name_list[[2]] == team_n)
    col_n <- paste0('skill[10,',team_idx,']')
    skill <- liga_sam_fix %>% select(col_n) %>% pull() %>% c()
    home_b <- liga_sam_fix %>% select('home_baseline[1]') %>% pull() %>% c()
    away_b <- liga_sam_fix %>% select('away_baseline[1]') %>% pull() %>% c()
    liga_test_fixed[i,'skill'] <- skill
    liga_test_fixed[i,'home_baseline'] <- home_b
    liga_test_fixed[i,'away_baseline'] <- away_b
    liga_test_fixed[i,'HA'] <- exp(home_b) - exp(away_b)
  }
}

liga_test_indep
liga_test_fixed

######################################SE


se_test_indep <- data_indep %>% filter(season=='1920' & league=='SE')
se_test_fixed <- data_fixed %>% filter(season=='1920' & league=='SE')

se_sam_ind <- as.data.frame(abc[[5]]) %>% t()
se_sam_ind <- as.data.frame(se_sam_ind)

se_sam_fix <- as.data.frame(abc[[6]]) %>% t()
se_sam_fix <- as.data.frame(se_sam_fix)

for (i in 1:nrow(se_test_indep)) {
  team_n <- se_test_indep$team_name[i] %>% c() 
  if (team_n != 'Spezia') {
    team_idx <- which(team_name_list[[3]] == team_n)
    col_n <- paste0('skill[10,',team_idx,']')
    skill <- se_sam_ind %>% select(col_n) %>% pull() %>% c()
    home_b <- se_sam_ind %>% select('home_baseline[10]') %>% pull() %>% c()
    away_b <- se_sam_ind %>% select('away_baseline[10]') %>% pull() %>% c()
    se_test_indep[i,'skill'] <- skill
    se_test_indep[i,'home_baseline'] <- home_b
    se_test_indep[i,'away_baseline'] <- away_b
    se_test_indep[i,'HA'] <- exp(home_b) - exp(away_b)
  }
}


for (i in 1:nrow(se_test_fixed)) {
  team_n <- se_test_fixed$team_name[i] %>% c()
  if (team_n != 'Spezia') {
  team_idx <- which(team_name_list[[3]] == team_n)
  col_n <- paste0('skill[10,',team_idx,']')
  skill <- se_sam_fix %>% select(col_n) %>% pull() %>% c()
  home_b <- se_sam_fix %>% select('home_baseline[1]') %>% pull() %>% c()
  away_b <- se_sam_fix %>% select('away_baseline[1]') %>% pull() %>% c()
  se_test_fixed[i,'skill'] <- skill
  se_test_fixed[i,'home_baseline'] <- home_b
  se_test_fixed[i,'away_baseline'] <- away_b
  se_test_fixed[i,'HA'] <- exp(home_b) - exp(away_b)
  }
}

se_test_indep
se_test_fixed

################################################BL

bl_test_indep <- data_indep %>% filter(season=='1920' & league=='BL')
bl_test_fixed <- data_fixed %>% filter(season=='1920' & league=='BL')

bl_sam_ind <- as.data.frame(abc[[7]]) %>% t()
bl_sam_ind <- as.data.frame(bl_sam_ind)

bl_sam_fix <- as.data.frame(abc[[8]]) %>% t()
bl_sam_fix <- as.data.frame(bl_sam_fix)

for (i in 1:nrow(bl_test_indep)) {
  team_n <- bl_test_indep$team_name[i] %>% c() 
  if (team_n %in% team_name_list[[4]]) {
    team_idx <- which(team_name_list[[4]] == team_n)
    col_n <- paste0('skill[10,',team_idx,']')
    skill <- bl_sam_ind %>% select(col_n) %>% pull() %>% c()
    home_b <- bl_sam_ind %>% select('home_baseline[10]') %>% pull() %>% c()
    away_b <- bl_sam_ind %>% select('away_baseline[10]') %>% pull() %>% c()
    bl_test_indep[i,'skill'] <- skill
    bl_test_indep[i,'home_baseline'] <- home_b
    bl_test_indep[i,'away_baseline'] <- away_b
    bl_test_indep[i,'HA'] <- exp(home_b) - exp(away_b)
  }
}


for (i in 1:nrow(bl_test_fixed)) {
  team_n <- bl_test_fixed$team_name[i] %>% c() 
  if (team_n %in% team_name_list[[4]]) {
    team_idx <- which(team_name_list[[4]] == team_n)
    col_n <- paste0('skill[10,',team_idx,']')
    skill <- bl_sam_fix %>% select(col_n) %>% pull() %>% c()
    home_b <- bl_sam_fix %>% select('home_baseline[1]') %>% pull() %>% c()
    away_b <- bl_sam_fix %>% select('away_baseline[1]') %>% pull() %>% c()
    bl_test_fixed[i,'skill'] <- skill
    bl_test_fixed[i,'home_baseline'] <- home_b
    bl_test_fixed[i,'away_baseline'] <- away_b
    bl_test_fixed[i,'HA'] <- exp(home_b) - exp(away_b)
  }
}

bl_test_indep
bl_test_fixed

data_test_indep <- rbind(pl_test_indep, liga_test_indep, se_test_indep, bl_test_indep)
data_test_fixed <- rbind(pl_test_fixed, liga_test_fixed, se_test_fixed, bl_test_fixed)
save(data_test_indep, file='data_test_indep_new.rdata')
save(data_test_fixed, file='data_test_fixed_new.rdata')


####################################################

b_pl <- read_csv('After_COVID_BL.csv')
b_pl <- b_pl %>% 
  select(season, home, away, full_home_score, full_away_score) %>%
  mutate(result = sign(full_home_score-full_away_score))
b_pl$season <- sapply(b_pl %>% select(season), function(x){as.character(as.numeric(substr(x,3,6)) - 101)})
b_pl$season %>% unique()
b_pl <- b_pl %>% filter(season == '1920')


t_b_pl <- b_pl %>% filter(season != '1011')

t_final_dt <- c()
for (i in 1:nrow(t_b_pl)) {
  h_team <- t_b_pl[i,'home'] %>% pull()
  a_team <- t_b_pl[i,'away'] %>% pull()
  if (a_team == "Atletico") {
    print('skip')
  }
  else {
    try({
      t_season <- t_b_pl[i, 'season'] %>% pull() %>% c()
      home_dt_indep <- data_test_indep %>% 
        filter(team_name == h_team & season == t_season) %>% 
        select(team_name, P, W, D, L, GF, GA, GD, Pts, raised, home_baseline, away_baseline, skill, HA) %>% 
        set_colnames(c('home_team', 'P_h', 'W_h', 'D_h', 'L_h', 'GF_h', 'GA_h', 'GD_h', 'Pts_h', 'raised_h', 'home_bs', 'away_bs', 'h_skill', 'HA'))
      away_dt_indep <- data_test_indep %>% filter(team_name == a_team & season == t_season) %>% 
        select(team_name, P, W, D, L, GF, GA, GD, Pts, raised, skill) %>% 
        set_colnames(c('away_team', 'P_a', 'W_a', 'D_a', 'L_a', 'GF_a', 'GA_a', 'GD_a', 'Pts_a', 'raised_a', 'a_skill'))
      home_dt_fixed <- data_test_fixed %>% 
        filter(team_name == h_team & season == t_season) %>% 
        select(team_name, home_baseline, away_baseline, skill, HA) %>% 
        set_colnames(c('home_team_fixed', 'home_bs_fixed', 'away_bs_fixed', 'h_skill_fixed', 'HA_fixed'))
      away_dt_fixed <- data_test_fixed %>% 
        filter(team_name == a_team & season == t_season) %>% 
        select(team_name, skill) %>% 
        set_colnames(c('away_team_fixed', 'a_skill_fixed'))
      t_dt <- cbind(t_b_pl[i,], home_dt_indep, away_dt_indep, home_dt_fixed, away_dt_fixed)
      t_final_dt <- rbind(t_final_dt, t_dt)
      print(paste0(i, '/', nrow(t_b_pl)))}
    )
  }
}
write_csv(t_final_dt, 'BL_test_pp_new.csv')

