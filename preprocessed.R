setwd('C:/Users/jaemini_man/Desktop/GitHub/soccer/soccer')
source('preprocessing_utils.R')

path1 <- 'C:/Users/jaemini_man/Desktop/GitHub/soccer/soccer/data/PL'
path2 <- 'C:/Users/jaemini_man/Desktop/GitHub/soccer/soccer/data/UEFA'
path3 <- 'C:/Users/jaemini_man/Desktop/GitHub/soccer/soccer/data/BL'
path4 <- 'C:/Users/jaemini_man/Desktop/GitHub/soccer/soccer/data/LIGA'
path5 <- 'C:/Users/jaemini_man/Desktop/GitHub/soccer/soccer/data/SE'
###184136

PL <- .load_data(path1,path2,'England')
BL <- .load_data(path3,path2,'Germany')
LIGA <- .load_data(path4,path2,'Spain')
SE <- .load_data(path5,path2,'Italy')

save(PL, file = 'PL.RData')
write.csv(PL, file = 'PL.csv')

save(BL, file = 'BL.RData')
write.csv(BL, file = 'BL.csv')

save(LIGA, file = 'LIGA.RData')
write.csv(LIGA, file = 'LIGA.csv')

save(SE, file = 'SE.RData')
write.csv(SE, file = 'SE.csv')

After_COVID_PL <- PL %>% filter(date_no > 18340)
After_COVID_BL <- BL %>% filter(date_no > 18340)
After_COVID_SE <- SE %>% filter(date_no > 18340)
After_COVID_LIGA <- LIGA %>% filter(date_no > 18340)

Before_COVID_PL <- PL %>% filter(date_no < 18340) 
Before_COVID_BL <- BL %>% filter(date_no < 18340)
Before_COVID_SE <- SE %>% filter(date_no < 18340)
Before_COVID_LIGA <- LIGA %>% filter(date_no < 18340) 

write.csv(After_COVID_PL, file = 'After_COVID_PL.csv')
write.csv(After_COVID_BL, file = 'After_COVID_BL.csv')
write.csv(After_COVID_SE, file = 'After_COVID_SE.csv')
write.csv(After_COVID_LIGA, file = 'After_COVID_LIGA.csv')

write.csv(Before_COVID_PL, file = 'Before_COVID_PL.csv')
write.csv(Before_COVID_BL, file = 'Before_COVID_BL.csv')
write.csv(Before_COVID_SE, file = 'Before_COVID_SE.csv')
write.csv(Before_COVID_LIGA, file = 'Before_COVID_LIGA.csv')



tail(A_PL)
