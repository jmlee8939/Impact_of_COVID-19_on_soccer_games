library('tidyverse')
library('readr')
library('lubridate')
library('hash')
######################

#### load data ####
.load_data <- function (path1, path2, country) {
  
  file1 <- list.files(path1, pattern='csv')
  file2 <- list.files(path2, pattern='csv')
  
  for (i in 1:length(file1)) {
    uefa_score_board <- 
      read.csv(paste(path2,file2[i],sep='/'), skip=1, header=F) %>%
      select(c(V1,V2,ncol(.))) %>% 
      .uefa_score_board(.,country)
    
    if (i == 1) {
      temp <- read.csv(paste(path1,file1[i],sep='/'))
      df <- .preprocessing(temp, file1[i], score_board = uefa_score_board)
      print(paste(i, 'season'))
    }
    
    else {
      uefa_score_board <- 
        read.csv(paste(path2,file2[i],sep='/'), skip=1, header=F) %>%
        select(c(V1,V2,ncol(.))) %>% 
        .uefa_score_board(.,country)
      
      temp <- read.csv(paste(path1, file1[i],sep='/'))
      temp <- .preprocessing(temp, file_name = file1[i], score_board = uefa_score_board)
      df <- rbind(df,temp)
      print(paste(i, 'season'))
    }
  }
  return(df)
}

#### preprocessing ####
.preprocessing <- function(df,file_name,score_board) {
  temp <- df
  temp <- temp %>% 
    select(-contains('rating')) %>%
    .null_to_NA %>%
    .matchup_stat_convert %>% 
    .fill_mean_value %>% 
    .date_preprocessing %>% 
    mutate(home_last_5_points = .last_n_stat(.,'points',5,'home'),
           away_last_5_points = .last_n_stat(.,'points',5,'away'),
           home_last_5_goals = .last_n_stat(.,'goals',5,'home'),
           away_last_5_goals = .last_n_stat(.,'goals',5,'away'),
           home_last_5_possession = .last_n_stat(.,'possession',5,'home'),
           away_last_5_possession = .last_n_stat(.,'possession',5,'away'),
           home_last_5_conceded = .last_n_stat(.,'conceded',5,'home'),
           away_last_5_conceded = .last_n_stat(.,'conceded',5,'away'),
           home_last_5_passes = .last_n_stat(.,'totalpasses',5,'home'),
           away_last_5_passes = .last_n_stat(.,'totalpasses',5,'away'),
           home_last_5_shot = .last_n_stat(.,'shot',5,'home'),
           away_last_5_shot = .last_n_stat(.,'shot',5,'away'),
           
           season = rep(str_split(file_name,'_')[[1]][1],nrow(temp))
    )
  temp['home_uefa'] <- temp %>% select(home) %>% apply(1,.uefa_score, score_board)
  temp['away_uefa'] <- temp %>% select(away) %>% apply(1,.uefa_score, score_board)
  return(temp)
}

#### null to NA
.null_to_NA <- function(df){
  df[df=='null'] <- NA
  return(df)
}

#### type.convert
.matchup_stat_convert <- function(df){
  
  df <- df %>% 
    mutate(matchup_home_goals = as.numeric(matchup_home_goals), matchup_away_goals = as.numeric(matchup_away_goals))
  
  df[c('matchup_home_wins','matchup_draw','matchup_away_wins')] <- 
    df %>%
    select(matchup_home_wins,matchup_draw,matchup_away_wins) %>% 
    lapply(extract_numeric)
  
  df[,34:ncol(df)] <- 
    df[,34:ncol(df)] %>%
    type.convert
  
  df[c('matchup_home_goals','matchup_away_goals')] <- 
    df %>%
    select(matchup_home_goals,matchup_away_goals) %>%
    replace(is.na(.),0)
  
  df[c('matchup_home_wins','matchup_draw','matchup_away_wins')] <-
    df %>% 
    select(matchup_home_wins,matchup_draw,matchup_away_wins) %>%
    replace(is.na(.),33)
  
  return(df)
}

#### fill_mean_values
.fill_mean_value <- function(df) {
  
  df[,1:19] <- df[,1:19] %>% 
    mutate_all(funs(ifelse(is.na(.), round(mean(., na.rm=TRUE)),.)))
  df[,33:ncol(df)] <- 
    df[,33:ncol(df)] %>% 
    mutate_all(funs(ifelse(is.na(.), round(mean(., na.rm=TRUE)),.)))
  
  return(df)
}

#### date_preprocessing  
.date_preprocessing <- function(df) {
  df$date <- df %>% 
    select(date) %>% 
    apply(2,as.character) %>%
    str_replace_all(c('Jpl'= 'Sun', 'Jtat' = 'Mon', 'Jnne'='Tue', 'Jtan'='Wed','Alh'='Thu', 'Ijm'='Fri', 'Jmos'='Sat')) %>%
    str_replace_all(c('Jan'= '01', 'Feb'='02','Mac'= '03', 'Apr'='04', 'Mei' = '05', 'Jun'='06', 'Jul'='07', 'Ago'='08', 'Sep'='09', 'Okt'='10','Nov'='11','Des'='12'))

  df <- df %>%
    separate(col=date, sep=', ', into=c("day", 'date')) %>%
    mutate(date_no = sapply(date,dmy)) %>%
    arrange(date_no)

  return(df)
}

#### uefa_score_bhoard
.uefa_score_board <- function(df, country){
  if (country == 'England') {
    replace_columns <- c('Bolton Wanderers'='Bolton','Fulham FC'='Fulham',
                         'Blackburn Rovers'='Blackburn','Portsmouth FC'='Portsmouth',
                         'Tottenham Hotspur'='Tottenham', 'Stoke City'='Stoke','Hull City'='Hull',
                         'Southampton FC'='Southampton', 'West Ham United'='West Ham', 'Leicester City'='Leicester',
                         'Burnley FC'='Burnley', 'Swansea City'='Swansea', 'Wigan Athletic'='Wigan')
    v1 = country
    v2 = str_sub(country,1,3)
  }
  else if (country == 'Germany') {
    replace_columns <- c("Bayern M체nchen" = "Bayern Munich", "VfL Wolfsburg"="Wolfsburg" ,
                         "Hertha BSC" ="Hertha Berlin","1.FC N체rnberg" ="Nuernberg" ,
                         "FSV Mainz 05" = "Mainz 05" , "Borussia M철nchengladbach" ="Borussia M.Gladbach",
                         "SC Freiburg" = "Freiburg", "FC Augsburg" ="Augsburg" ,
                         "1.FC K철ln" ="FC Koln", "1899 Hoffenheim" ="Hoffenheim")
    v1 = country
    v2 = str_sub(country,1,3)
  }
  else if (country == 'Spain') {
    replace_columns <- c("FC Barcelona"= "Barcelona", "Atl챕tico Madrid"="Atletico Madrid",
                         "Celta de Vigo" ="Celta Vigo","Deportivo La Coru챰a" ="Deportivo La Coruna" ,
                         "M찼laga CF" = "Malaga" , "Levante UD" = "Levante")
    v1 = country
    v2 = 'Esp'
  }
  else if (country == 'Italy') {
    replace_columns <- c("Internazionale"= "Inter", "AS Roma" ="Roma" ,
                        "Palermo" = "Palermo FC","AC Parma" = "Parma Calcio 1913" ,
                        "Chievo Verona" =  "Verona" )
    v1 = country
    v2 = 'Ita'
  }
  df_1 <- df %>% filter(V2==v2|V1==v1)
  aa <- df_1 %>% 
    select(V1) %>% 
    apply(2,as.character) %>%
    str_replace_all(replace_columns)
  df_1[,1] <- aa 
  print(df_1)
  return(df_1 %>% select(c(V1,ncol(df))))
}


#### uefa_score
.uefa_score <- function(team, scoreboard){
  uefa_team <- scoreboard %>% select(V1) %>% pull()
  if (team %in% uefa_team){
    return(scoreboard[uefa_team %in% team,2])
  }
  else {
    return(min(scoreboard[,2]))
  }
}

#### extract_data
.extract_last_n_points <- function(df, team, n) {
  tt<- df %>% 
    mutate(winning_point = full_home_score - full_away_score) %>%
    filter(home == team | away == team) %>% 
    mutate(point = case_when(home == team ~ winning_point, away == team ~ -winning_point)) %>% 
    mutate(point = case_when(point > 0 ~ 3, point == 0 ~ 1, point < 0 ~ 0)) %>%
    select(point) %>%
    pull()
  if (length(tt) <= n){
    return(NA)
  }
  else{
    return(sum(tt[(length(tt)-n):(length(tt)-1)]))
  }
}

.extract_last_n_points <- function(df, team, n) {
  tt<- df %>% 
    mutate(winning_point = full_home_score - full_away_score) %>%
    filter(home == team | away == team) %>% 
    mutate(point = case_when(home == team ~ winning_point, away == team ~ -winning_point)) %>% 
    mutate(point = case_when(point > 0 ~ 3, point == 0 ~ 1, point < 0 ~ 0)) %>%
    select(point) %>%
    pull()
  if (length(tt) <= n){
    return(NA)
  }
  else{
    return(sum(tt[(length(tt)-n):(length(tt)-1)]))
  }
}

.extract_last_n_points <- function(df, team, n) {
  tt<- df %>% 
    mutate(winning_point = full_home_score - full_away_score) %>%
    filter(home == team | away == team) %>% 
    mutate(point = case_when(home == team ~ winning_point, away == team ~ -winning_point)) %>% 
    mutate(point = case_when(point > 0 ~ 3, point == 0 ~ 1, point < 0 ~ 0)) %>%
    select(point) %>%
    pull()
  if (length(tt) <= n){
    return(NA)
  }
  else{
    return(sum(tt[(length(tt)-n):(length(tt)-1)]))
  }
}
.extract_last_n_possession <- function(df, team, n) {
  tt<- df %>% 
    filter(home == team | away == team) %>% 
    mutate(point = case_when(home == team ~ home_possession, away == team ~ away_possession)) %>% 
    select(point) %>%
    pull()
  if (length(tt) <= n){
    return(NA)
  }
  else{
    return(mean(tt[(length(tt)-n):(length(tt)-1)]))
  }
}
.extract_last_n_goals <- function(df, team, n) {
  tt<- df %>% 
    filter(home == team | away == team) %>% 
    mutate(full_away_score = as.numeric(full_away_score), full_home_score = as.numeric(full_home_score)) %>% 
    mutate(point = case_when(home == team ~ full_home_score, away == team ~ full_away_score)) %>% 
    select(point) %>%
    pull()
  if (length(tt) <= n){
    return(NA)
  }
  else{
    return(sum(tt[(length(tt)-n):(length(tt)-1)]))
  }
}
.extract_last_n_conceded <- function(df, team, n) {
  tt<- df %>% 
    filter(home == team | away == team) %>% 
    mutate(full_away_score = as.numeric(full_away_score), full_home_score = as.numeric(full_home_score)) %>% 
    mutate(point = case_when(home == team ~ full_away_score, away == team ~ full_home_score)) %>% 
    select(point) %>%
    pull()
  if (length(tt) <= n){
    return(NA)
  }
  else{
    return(sum(tt[(length(tt)-n):(length(tt)-1)]))
  }
}
.extract_last_n_totalpasses <- function(df, team, n) {
  tt<- df %>% 
    filter(home == team | away == team) %>% 
    mutate(home_total_passes = as.numeric(home_total_passes), away_total_passes = as.numeric(away_total_passes)) %>% 
    mutate(point = case_when(home == team ~ home_total_passes, away == team ~ away_total_passes)) %>% 
    select(point) %>%
    pull()
  if (length(tt) <= n){
    return(NA)
  }
  else{
    return(sum(tt[(length(tt)-n):(length(tt)-1)]))
  }
}
.extract_last_n_shot <- function(df, team, n) {
  tt<- df %>% 
    filter(home == team | away == team) %>% 
    mutate(home_shot = as.numeric(home_shot), away_shot = as.numeric(away_shot)) %>% 
    mutate(point = case_when(home == team ~ home_shot, away == team ~ away_shot)) %>% 
    select(point) %>%
    pull()
  if (length(tt) <= n){
    return(NA)
  }
  else{
    return(sum(tt[(length(tt)-n):(length(tt)-1)]))
  }
}

#### last n stat
.last_n_stat <- function(df, type, n, h_a) {
  n_stat <- c()
  if (type == 'points') {
    for (i in 1:nrow(df)){
      if (h_a == 'home'){
        team <- as.character(df$home[i])
      }
      else{
        team <- as.character(df$away[i])
      }
      temp_df <- df[1:i,]
      n_stat[i] <- .extract_last_n_points(temp_df, team, 5)
    }
    return(n_stat)
  }
  else if (type == 'goals') {
    for (i in 1:nrow(df)){
      if (h_a == 'home'){
        team <- as.character(df$home[i])
      }
      else{
        team <- as.character(df$away[i])
      }
      temp_df <- df[1:i,]
      n_stat[i] <- .extract_last_n_goals(temp_df, team, 5)
    }
    return(n_stat)
  }
  else if (type == 'possession') {
    for (i in 1:nrow(df)){
      if (h_a == 'home'){
        team <- as.character(df$home[i])
      }
      else{
        team <- as.character(df$away[i])
      } 
      temp_df <- df[1:i,]
      n_stat[i] <- .extract_last_n_possession(temp_df, team, 5)
    }
    return(n_stat)
  }
  else if (type == 'conceded') {
    for (i in 1:nrow(df)) {
      if (h_a == 'home') {
        team <- as.character(df$home[i])
      }
      else{
        team <- as.character(df$away[i])
      } 
      temp_df <- df[1:i,]
      n_stat[i] <- .extract_last_n_conceded(temp_df, team, 5)
    }
    return(n_stat)
  }
  else if (type == 'totalpasses') {
    for (i in 1:nrow(df)) {
      if (h_a == 'home') {
        team <- as.character(df$home[i])
      }
      else {
        team <- as.character(df$away[i])
      }
      temp_df <- df[1:i,]
      n_stat[i] <- .extract_last_n_totalpasses(temp_df, team, 5)
    }
    return(n_stat)
  }
  else {
    for (i in 1:nrow(df)) {
      if (h_a == 'home') {
        team <- as.character(df$home[i])
      }
      else {
        team <- as.character(df$away[i])
      }
      temp_df <- df[1:i,]
      n_stat[i] <- .extract_last_n_shot(temp_df, team, 5)
    }
    return(n_stat)
  }
}






