library('tidyverse')

PL %>% colnames

setwd('C:/Users/Infosci_center/Documents/GitHub/soccer/soccer')
PL <- read.csv('Before_COVID_PL.csv')
PL <- PL[,3:ncol(PL)]

LIGA <- read.csv('Before_COVID_LIGA.csv')
LIGA <- LIGA[,3:ncol(LIGA)]

SE <- read.csv('Before_COVID_SE.csv')
SE <- SE[,3:ncol(SE)]

BL <- read.csv('Before_COVID_BL.csv')
BL <- BL[,3:ncol(BL)]

all_data <- rbind(PL,LIGA,SE,BL)

re <- all_data %>% 
  filter(home_missing_player!=away_missing_player) %>% 
  mutate(missing_player = home_missing_player - away_missing_player, win = full_home_score - full_away_score) %>% 
  mutate(result = win*missing_player)

seasons <- levels(all_data$season) 


df <- c(0,0)
for (i in seasons) {
  aa <- re %>% filter(win>0, season %in% i) %>% nrow
  bb <- re %>% filter(win==0, season %in% i) %>% nrow
  cc <- re %>% filter(win<0, season %in% i) %>% nrow
  tmp <- c(i,round((3*aa+bb)/(aa+bb+cc),3))
  df <- rbind(df,tmp)
}


dt <- c(0,0)
for (i in 1:4) {
  sea <- seasons[((i*2)-1):(i*2)]
  aa <- re %>% filter(win>0, season %in% sea) %>% nrow
  bb <- re %>% filter(win==0, season %in% sea) %>% nrow
  cc <- re %>% filter(win<0, season %in% sea) %>% nrow
  tmp <- c(i,round((aa*3+bb)/(aa+bb+cc),3))
  dt <- rbind(dt,tmp)
}

write.csv(df,'home_win_before.csv')
# home_win_after(0.403,0.42,0.421,0.379)

# PL LIGA SE BL
#dt     0 0.000
#tmp    1 0.367
#tmp    2 0.300
#tmp    3 0.334
#tmp    4 0.365
#tmp    1 1.439
#tmp    2 1.539
#tmp    3 1.507
#tmp    4 1.394

# tmp "PL1112"   "1.576"
# tmp "PL1213"   "1.593"
# tmp "PL1314"   "1.593"
# tmp "PL1415"   "1.628"
# tmp "PL1516"   "1.519"
# tmp "PL1617"   "1.672"
# tmp "PL1718"   "1.664"
# tmp "PL1819"   "1.622"
# tmp "PL1920"   "1.591"
# tmp "LIGA1112" "1.723"
# tmp "LIGA1213" "1.751"
# tmp "LIGA1314" "1.597"
# tmp "LIGA1415" "1.584"
# tmp "LIGA1516" "1.746"
# tmp "LIGA1617" "1.7"  
# tmp "LIGA1718" "1.671"
# tmp "LIGA1819" "1.657"
# tmp "LIGA1920" "1.677"
# tmp "SE1112"   "1.632"
# tmp "SE1213"   "1.639"
# tmp "SE1314"   "1.672"
# tmp "SE1415"   "1.672"
# tmp "SE1516"   "1.506"
# tmp "SE1617"   "1.563"
# tmp "SE1718"   "1.554"
# tmp "SE1819"   "1.641"
# tmp "SE1920"   "1.403"
# tmp "BL1112"   "1.614"
# tmp "BL1213"   "1.571"
# tmp "BL1314"   "1.671"
# tmp "BL1415"   "1.64" 
# tmp "BL1516"   "1.581"
# tmp "BL1617"   "1.714"
# tmp "BL1718"   "1.616"
# tmp "BL1819"   "1.632"
# tmp "BL1920"   "1.484"

win_rate <- df[,2] %>% as.numeric()
win_rate <- win_rate[-1]
win_rate <- matrix(win_rate, nrow = 4, ncol = 9, byrow=TRUE)
win_rate <- cbind(win_rate, c(0.403,0.42,0.421,0.379))
write.csv(win_rate, 'win_rate.csv')


win_score <- df[,2] %>% as.numeric()
win_score <- win_score[-1]
win_score <- matrix(win_score, nrow = 4, ncol = 9, byrow=TRUE)
win_score <- cbind(win_score, c(1.439, 1.539, 1.507, 1.394))
write.csv(win_score, 'win_score.csv')
win_score

#tmp    1 1.439
#tmp    2 1.539
#tmp    3 1.507
#tmp    4 1.394

qqnorm(c(win_score))
qqline(c(win_score))

qqnorm(c(win_rate))
qqline(c(win_rate))


shapiro.test(c(win_score))
t.test(c(win_score[,1:9]), c(win_score[,10]))

shapiro.test(c(win_rate))
t.test(c(win_rate[,1:9]), c(win_rate[,10]))

