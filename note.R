library('tidyverse')
library('readr')
library('lubridate')
library('hash')

########### PL data
getwd()
path <-'C:/Users/Infosci_center/Documents/GitHub/soccer/soccer/data/LIGA' 
file <- list.files(path, pattern='csv')

########### UEFA data
path2 <-'C:/Users/Infosci_center/Documents/GitHub/soccer/soccer/data/UEFA' 
file2 <- list.files(path2, pattern='csv')

setwd(path)

#### load data PL ####
.load_data <- function (path1, path2, country) {
  
  file1 <- list.files(path1, pattern='csv')
  file2 <- list.files(path2, pattern='csv')
  
  for (i in 1:length(file1)) {
    uefa_score_board <- 
      read.csv(paste(path2,file2[i],sep='/'), skip=1, header=F) %>%
      select(c(V1,V2,ncol(.))) %>% 
      .uefa_score_board(.,country)
    
    if (i == 1) {
      temp <- read.csv(file1[i])
      df <- .preprocessing(temp)
      print(paste(i, 'season'))
    }
    
    else {
      uefa_score_board <- 
        read.csv(paste(path2,file2[i],sep='/'), skip=1, header=F) %>%
        select(c(V1,V2,ncol(.))) %>% 
        .uefa_score_board(.,country)
      
      temp <- read.csv(file1[i])
      temp <- .preprocessing(temp)
      df <- rbind(df,temp)
      print(paste(i, 'season'))
    }
  }
  return(df)
}

for (i in 1:length(file)) {
  uefa_score_board <- 
    read.csv(paste(path2,file2[i],sep='/'), skip=1, header=F) %>%
    select(c(V1,V2,ncol(.))) %>% 
    .uefa_score_board(.,'Spain')
  
  if (i == 1) {
    temp <- read.csv(file[i])
##    df_PL <- .preprocessing(temp,file[i])
    
    df_PL <-temp
  }
  
  else {
    uefa_score_board <- 
      read.csv(paste(path2,file2[i],sep='/'), skip=1, header=F) %>%
      select(c(V1,V2,ncol(.))) %>% 
      .uefa_score_board(.,'Spain')
    temp <- read.csv(file[i])
 ##   temp <- .preprocessing(temp,file[i])
    df_PL <- rbind(df_PL,temp)
  }
}
df_PL
d

df_PL %>% summary
save(df_PL, file = 'C:/Users/Infosci_center/Documents/GitHub/soccer/soccer/PL.RData')
write.csv(df_PL, file = 'C:/Users/Infosci_center/Documents/GitHub/soccer/soccer/PL.csv')
tt <- read.csv(file[2])
tt2 <- read.csv(paste(path2,file2[2],sep='/'), skip=1, header=F) %>%
  select(c(V1,V2,ncol(.))) %>% 
  .uefa_score_board(.,'Spain')
tt2

tt <-tt %>% 
  select(-contains('rating')) %>%
  .null_to_NA %>%
  .matchup_stat_convert %>% 
  .fill_mean_value %>% 
  .date_preprocessing

tt %>% mutate(home_last_5 =.last_n_stat(.,'goals',5,'home'))
unique(tt$home)

########## 정리...

.preprocessing <- function(df) {
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
           
           season = rep(str_split(file[i],'_')[[1]][1],nrow(temp))
    )
  temp['home_uefa'] <- temp %>% select(home) %>% apply(1,.uefa_score,uefa_score_board)
  temp['away_uefa'] <- temp %>% select(away) %>% apply(1,.uefa_score,uefa_score_board)
  return(temp)
}



.last_n_stat(tt,'points',5,'home')
tt %>% summary
.extract_last_n_points(tt[1:1,], as.character(tt$home[5]), 5)
as.character(tt$home[5])

######### null to NA
.null_to_NA <- function(df){
  df[df=='null'] <- NA
  return(df)
}
#df_PL[df_PL=='null'] <- NA

########### type.convert
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

####### fill_mean_values
.fill_mean_value <- function(df) {
  
  df[,1:19] <- df[,1:19] %>% 
    mutate_all(funs(ifelse(is.na(.), round(mean(., na.rm=TRUE)),.)))
  df[,33:ncol(df)] <- 
    df[,33:ncol(df)] %>% 
    mutate_all(funs(ifelse(is.na(.), round(mean(., na.rm=TRUE)),.)))
  
  return(df)
}


df_PL[,33:ncol(df_PL)] <- df_PL[,33:ncol(df_PL)] %>% mutate_all(funs(ifelse(is.na(.), round(mean(., na.rm=TRUE)),.)))
file
df_PL %>% mutate(away_last_5_conceded = ifelse(is.na(away_last_5_conceded), 0, away_last_5_conceded))

df_PL %>% is.na %>% colSums

#df_PL %>% summary
#df_PL %>% is.na %>% colSums 
#df_PL <- df_PL %>% mutate(matchup_home_goals = as.numeric(matchup_home_goals), matchup_away_goals = as.numeric(matchup_away_goals))
#df_PL[c('matchup_home_wins','matchup_draw','matchup_away_wins')] <- df_PL %>% select(matchup_home_wins,matchup_draw,matchup_away_wins) %>% lapply(extract_numeric)
#df_PL[,34:ncol(df_PL)-1] <- df_PL[,34:ncol(df_PL)-1] %>% type.convert

################ fill NA
### rating remove
df_PL <- df_PL %>% select(-contains('rating'))

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


#df_PL$date <- df_PL %>% select(date) %>% apply(2,as.character) %>%
#  str_replace_all(c('Jpl'= 'Sun', 'Jtat' = 'Mon', 'Jnne'='Tue', 'Jtan'='Wed','Alh'='Thu', 'Ijm'='Fri', 'Jmos'='Sat')) %>%
#  str_replace_all(c('Jan'= '01', 'Feb'='02','Mac'= '03', 'Apr'='04', 'Mei' = '05', 'Jun'='06', 'Jul'='07', 'Ago'='08', 'Sep'='09', 'Okt'='10','Nov'='11','Des'='12'))

#### day change
## Jpl, Jtat, Jnne, Jtan, Alh,Ijm, Jmos
## Jan, Fep, Mac, Apr, mei, Jun , Jul , Ago, Sep, Okt, Nov, Des,  

######### arrange with match date

##df_PL <- df_PL %>% separate(col=date, sep=', ', into=c("day", 'date')) %>%
#  mutate(date_no = sapply(date,dmy)) %>%
#  arrange(date_no)

########### fill zeros in matchup...
df_PL[c('matchup_home_goals','matchup_away_goals','matchup_home_wins','matchup_draw','matchup_away_wins')] <-
  df_PL %>% select(contains('matchup')) %>% replace(is.na(.),0)

############ fill mean values

df_PL[,33:ncol(df_PL)] <- df_PL[,33:ncol(df_PL)] %>% mutate_all(funs(ifelse(is.na(.), round(mean(., na.rm=TRUE)),.)))
file
############ 
path2 <-'C:/Users/Infosci_center/Documents/GitHub/soccer/soccer/data/UEFA' 
file2 <- list.files(path2, pattern='csv')
temp <- read.csv(paste(path2,file2[1],sep='/'), skip=1, header=F) %>% select(c(V1,V2,ncol(.)))
for (i in 1:length(file2)){
  tmp <- read.csv(paste(path2,file2[i],sep='/'), skip=1, header=F) %>% select(c(V1,V2,ncol(.)))
  temp <- rbind(temp, setNames(tmp,names(temp)))
}
temp
a<-'abcdef'
str_sub(a,1,3)
###################
### Eng, Esp, Ger, Ita, Spain, England, Germany, Italy
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
    replace_columns <- c("Bayern M체nchen"= "Bayern Munich", "VfL Wolfsburg"="Wolfsburg" ,
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
  df <- df %>% filter(V2==v2|V1==v1)
  df$V1 <- df %>% 
    select(V1) %>% apply(2,as.character) %>%
    str_replace_all(replace_columns)
  print(df[1:5,])
  return(df %>% select(c(V1,ncol(df))))
}

temp <-.uefa_score_board(temp,'Spain')

temp <- temp %>% filter(V2=='Eng'| V1=='England') 
temp$V1 <- temp %>% 
  select(V1) %>% apply(2,as.character) %>%
  str_replace_all(c('Bolton Wanderers'='Bolton','Fulham FC'='Fulham',
                    'Blackburn Rovers'='Blackburn','Portsmouth FC'='Portsmouth',
                    'Tottenham Hotspur'='Tottenham', 'Stoke City'='Stoke','Hull City'='Hull',
                    'Southampton FC'='Southampton', 'West Ham United'='West Ham', 'Leicester City'='Leicester',
                    'Burnley FC'='Burnley', 'Swansea City'='Swansea', 'Wigan Athletic'='Wigan'))
temp <- temp %>% select(c(V1,ncol(temp)))
.uefa_score('Everton',temp)
temp %>% select(V1) %>% pull() %in% 'Everton'

.uefa_score <- function(team, scoreboard){
  uefa_team <- scoreboard %>% select(V1) %>% pull()
  if (team %in% uefa_team){
    return(scoreboard[uefa_team %in% team,2])
  }
  else {
    return(min(scoreboard[,2]))
  }
}

df_PL %>% mutate(home_uefa = apply(home,1,.uefa_score(.,temp)))
df_PL['home_uefa']<- df_PL %>% select(home) %>% apply(1,.uefa_score,temp)
df_PL %>% select(c(home_uefa,home))
df_PL %>% arrange(date_no) %>% group_by(home) %>% summary
unique(df_PL$home)
df_PL %>% mutate(home_points = full_home_score-full_away_score) 


tt2 <- unique(df_PL$home)
tt2 %>% sort()
intersect(tt,tt2)

h <- hash(unique(df_PL$home), rep(0,length(unique(df_PL$home))))
h$Arsenal
######## 직전 5경기 득점, 실점, 승점, possession, 
.last_n_stat <- function(df, type, n, h_a) {
  n_stat <- c()
  if (type == 'points') {
    for (i in 1:nrow(df)){
      if (h_a == 'home'){
        team <- df$home[i]
      }
      else{
        team <- df$away[i]
      }
      temp_df <- df[1:i,]
      n_stat[i] <- .extract_last_n_points(temp_df, team, 5)
    }
    return(n_stat)
  }
  else if (type == 'goals') {
    for (i in 1:nrow(df)){
      if (h_a == 'home'){
        team <- df$home[i]
      }
      else{
        team <- df$away[i]
      }
      temp_df <- df[1:i,]
      n_stat[i] <- .extract_last_n_goals(temp_df, team, 5)
    }
    return(n_stat)
  }
  else if (type == 'possession') {
    for (i in 1:nrow(df)){
      if (h_a == 'home'){
        team <- df$home[i]
      }
      else{
          team <- df$away[i]
      } 
      temp_df <- df[1:i,]
      n_stat[i] <- .extract_last_n_possession(temp_df, team, 5)
    }
    return(n_stat)
  }
  else if (type == 'conceded') {
    for (i in 1:nrow(df)) {
      if (h_a == 'home') {
        team <- df$home[i]
      }
      else{
        team <- df$away[i]
      } 
      temp_df <- df[1:i,]
      n_stat[i] <- .extract_last_n_conceded(temp_df, team, 5)
    }
    return(n_stat)
  }
  else if (type == 'totalpasses') {
    for (i in 1:nrow(df)) {
      if (h_a == 'home') {
        team <- df$home[i]
      }
      else {
        team <- df$away[i]
      }
      temp_df <- df[1:i,]
      n_stat[i] <- .extract_last_n_totalpasses(temp_df, team, 5)
    }
    return(n_stat)
  }
  else {
    for (i in 1:nrow(df)) {
      if (h_a == 'home') {
        team <- df$home[i]
      }
      else {
        team <- df$away[i]
      }
      temp_df <- df[1:i,]
      n_stat[i] <- .extract_last_n_shot(temp_df, team, 5)
    }
  }
}



temp <- df_PL %>% filter(season =='PL1112')
temp %>% mutate(last_5_points = .last_n_stat(.,'points',5,'home')) %>% select(last_5_points)
temp %>% mutate(last_5_points = .last_n_stat(.,'possession',5,'home')) %>% select(last_5_points)
temp %>% mutate(last_5_points = .last_n_stat(.,'goals',5,'home')) %>% select(last_5_points)
temp %>% mutate(last_5_points = .last_n_stat(.,'conceded',5,'home')) %>% select(last_5_points)


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
df_PL

df_PL$home_total_passes
tt<- df_PL %>% 
  mutate(winning_point = full_home_score - full_away_score) %>%
  filter(home=='Arsenal'|away=='Arsenal') %>% 
  mutate(point = case_when(home=='Arsenal' ~ winning_point,away=='Arsenal' ~ -winning_point)) %>% 
  mutate(point = case_when(point>0 ~ 3, point==0 ~ 1, point<0 ~ 0)) %>%
  select(point) %>%
  pull()
sum(tt[(length(tt)-5):(length(tt)-1)])

######### baseline
rep(0,34)
######### 

######### Recursive least squares filter()
path2
BL
path <-'C:/Users/Infosci_center/Documents/GitHub/soccer/soccer/data/LIGA'
file <- list.files(path,pattern='csv')

for (i in 1:length(file)) {
  if (i == 1) {
    tt <- read.csv(paste(path,file[i],sep='/')) %>% select(home) %>% type.convert
  }
  else {
    temp <- read.csv(paste(path,file[i],sep='/')) %>% select(home) %>% type.convert
    tt <- rbind(tt,temp)
  }
}
tt

file2
for (i in 1:length(file2)) {
  if (i == 1) {
    tt2 <- read.csv(paste(path2,file2[i], sep='/'), skip = 1, header = F) %>% 
      filter(V1 == 'Germany' | V2 == 'Ger') %>% 
      select(V1) %>% type.convert
  }
  else {
    temp <- read.csv(paste(path2,file2[i], sep='/'), skip = 1, header = F) %>% 
      filter(V1 == 'Germany' | V2 == 'Ger') %>%
      select(V1) %>% type.convert
    
    tt2 <- rbind(tt2,temp)
    }
}



1512/380

unique(tt2)
unique(tt)
unique
tt2[1]
tt[1]

setdiff(pull(unique(tt)), pull(unique(tt2)))
setdiff(pull(unique(tt2)), pull(unique(tt)))
intersect(pull(unique(tt)), pull(unique(tt2)))

intersect(c('1','2','5'),c('1','3','5'))
as.character()

str_replace_all(c("FC Barcelona"= "Barcelona", "Atl챕tico Madrid"="Atletico Madrid",
                  "Celta de Vigo" ="Celta Vigo","Deportivo La Coru챰a" ="Deportivo La Coruna" ,
                  "M찼laga CF" = "Malaga" , "Levante UD" = "Levante"))


path2

path5 <- 'C:/Users/Infosci_center/Documents/GitHub/soccer/soccer/data/SE'
lis <- list.files(path5)
setwd(path5)
for (i in 1:length(lis)) {
  if (i == 1) {
    df <- read.csv(lis[i]) %>% select(home)
  }
  else {
    temp <- read.csv(lis[i]) %>% select(home)
    df <- rbind(df,temp)
  }
}
setdiff(unique(pull(df)),unique(pull(tt2)))
setdiff(unique(pull(tt2)),unique(pull(df)))
str_replace_all(c("Internazionale"= "Inter", "AS Roma" ="Roma" ,
                  "Palermo" = "Palermo FC","AC Parma" ="Parma Calcio 1913" ,
                  "Chievo Verona" =  "Verona" ))

tt2 %>% 
  select(V1) %>% apply(2,as.character) %>%
  str_replace_all(replace_columns)


unique(pull(df))
unique(pull(tt2))
unique(df)


paste(path,file[1],sep='/')

############# feature distribution