###plot

getwd()
setwd('C:/Users/jaemini_man/Desktop/GitHub/soccer/soccer')

install.packages("ggpubr")

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
library(ggplot2)
library(gridExtra)
library(ggpubr)

load('PL_sampling_indep.rdata')
load('LIGA_sampling_fixed.rdata')
load('Liga')
output0 <- read_csv('PL_indep_samples.csv')
output2 <- read_csv('LIGA_indep_samples.csv')
output4 <- read_csv('SE_indep_samples.csv')
output6 <- read_csv('BL_indep_samples.csv')

output0 <- bases3
output2 <- bases
output4 <- bases
output6 <- bases

rm(bases)

team_label <- function(path, league, mode=1){
  df <- read_csv(paste0('Before_COVID_', league, '.csv'))
  df <- df %>% select(season) %>% unique() 
  output <- df %>% pull() %>% c()
  output <- c(output, 'After COVID-19')
  return(list(output, output))
}


plot_data <- function(league, samples) {
  seasons <- team_label(path, league, mode=1)[[2]]
  home_base <- samples[, str_detect(string = colnames(samples), "home_baseline\\[")] 
  away_base <- samples[, str_detect(string = colnames(samples), "away_baseline\\[")] 
  home_advantage <- exp(home_base) - exp(away_base)
  colnames(home_advantage) <- seasons 
  aa <- as.data.frame(colMeans(as.matrix(home_advantage)))
  colnames(aa) <- 'HA'
  aa$label <- rev(c(1:10))
  std <- apply(t(as.matrix(home_advantage)), 1, sd)*1.69
  HA <- aa$HA
  aa$std <- std
  aa$COVID19 <- c(rep("Before",9), "After") 
  return(aa)
} 

PL_seasons <- team_label(path, 'PL', mode=1)[[2]]
PL_seasons[10] <- 'After COVID-19'
LIGA_seasons <- team_label(path, 'LIGA', mode=1)[[2]]
LIGA_seasons[10] <- 'After COVID-19'
LIGA_seasons <- team_label(path, 'LIGA', mode=1)[[2]]
LIGA_seasons[10] <- 'After COVID-19'
SE_seasons <- team_label(path, 'SE', mode=1)[[2]]
SE_seasons[10] <- 'After COVID-19'
BL_seasons <- team_label(path, 'BL', mode=1)[[2]]
BL_seasons[10] <- 'After COVID-19'


colnames(PL_dt)[4] <- 'COVID19'
colnames(LIGA_dt)[4] <- 'COVID19'
colnames(SE_dt)[4] <- 'COVID19'
colnames(BL_dt)[4] <- 'COVID19'


PL_dt <- plot_data('PL', output0)
mean(PL_dt$HA[1:9])
LIGA_dt <- plot_data('LIGA', output2)
mean(LIGA_dt$HA[1:9])
SE_dt <- plot_data('SE', output4)
mean(SE_dt$HA[1:9])
BL_dt <- plot_data('BL', output6)  
mean(SE_dt$HA[1:9])
PL_dt <- transform(PL_dt, type = factor(type, levels = c("Before", "After")))


(380*(mean(PL_dt$HA[1:9]) + mean(LIGA_dt$HA[1:9]) + mean(SE_dt$HA[1:9])) + 306*mean(BL_dt$HA[1:9]))/(380*3 + 306)

(380*(mean(PL_dt$HA[10]) + mean(LIGA_dt$HA[10]) + mean(SE_dt$HA[10])) + 306*mean(BL_dt$HA[10]))/(380*3 + 306)
0.35-0.17

PL_plot <- ggplot(data=PL_dt, aes(x=HA, y=label, xmin=HA-std, xmax=HA+std)) + 
  geom_pointrange(aes(color=COVID19), shape=20, size = 0.8, alpha = 1) +
  scale_fill_brewer(palette="Oranges") +
  annotate("text", x=PL_dt$HA , y=PL_dt$label+0.4, label=PL_seasons, size=2.5) +
  theme(axis.title.y=element_text(size=7), ) +
  ylab(' ') +
  geom_vline(xintercept = mean(PL_dt$HA[1:9]), linetype="dashed", color = 'blue', size =0.8, alpha = 0.3) +
  scale_y_continuous(breaks=NULL) +
  scale_x_continuous(breaks=seq(-1, 1, 0.1)) + 
  theme(axis.text.y=element_blank()) +
  labs(title='B') +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=10)) +
  coord_cartesian(xlim = c(-0.1, 0.7)) +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = -0.1, vjust = 0, size=20, face='bold')) +
  scale_color_discrete(breaks=c("Before","After")) +
  labs(colour = "COVID-19") +
  annotate('text', x=0.5, y=1, label = 'Premier League', fontface = 'italic', size = 4)

PL_plot


LIGA_plot <- ggplot(data=LIGA_dt, aes(x=HA, y=label, xmin=HA-std, xmax=HA+std)) + 
  geom_pointrange(aes(color=COVID19), shape=20, size = 0.8, alpha = 1) +
  scale_fill_brewer(palette="Oranges") +
  annotate("text", x=LIGA_dt$HA , y=LIGA_dt$label+0.4, label=LIGA_seasons, size=2.5) +
  theme(axis.title.y=element_text(size=7)) +
  geom_vline(xintercept = mean(LIGA_dt$HA[1:9]), linetype="dashed", color = 'blue', size =0.8, alpha = 0.3) +
  scale_y_continuous(breaks=NULL) +
  scale_x_continuous(breaks=seq(-1, 1, 0.1)) +
  theme(axis.text.y=element_blank(), axis.text.x=element_text(size=10)) +
  labs(title='C') +
  ylab(' ') +
  coord_cartesian(xlim = c(-0.1, 0.7)) +
  theme(axis.title.x=element_blank()) +
  theme(plot.title = element_text(hjust = -0.1, vjust = 0, size=20, face='bold')) +
  scale_color_discrete(breaks=c("Before","After")) +
  labs(colour = "COVID-19") +
  annotate('text', x=0.6, y=1, label = 'LaLiga', fontface = 'italic', size = 4) +
  theme(legend.position="none")

LIGA_plot

SE_plot <- ggplot(data=SE_dt, aes(x=HA, y=label, xmin=HA-std, xmax=HA+std)) + 
  geom_pointrange(aes(color=COVID19), shape=20, size = 0.8, alpha = 1) +
  scale_fill_brewer(palette="Oranges") +
  annotate("text", x=SE_dt$HA , y=SE_dt$label+0.4, label=SE_seasons, size=2.5) +
  theme(axis.title.y=element_text(size=7)) +
  geom_vline(xintercept = mean(SE_dt$HA[1:9]), linetype="dashed", color = 'blue', size =0.8, alpha = 0.3) +
  scale_y_continuous(breaks=NULL) +
  scale_x_continuous(breaks=seq(-1, 1, 0.1)) +
  theme(axis.text.y=element_blank()) +
  labs(title='D') +
  ylab(' ') +
  coord_cartesian(xlim = c(-0.1, 0.7)) +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=10)) +
  theme(plot.title = element_text(hjust = -0.1, vjust = 0, size=20, face='bold')) +
  scale_color_discrete(breaks=c("Before","After")) +
  labs(colour = "COVID-19") +
  annotate('text', x=0.6, y=1, label = 'Serie A', fontface = 'italic', size = 4) +
  theme(legend.position="none")

SE_plot


BL_plot <- ggplot(data=BL_dt, aes(x=HA, y=label, xmin=HA-std, xmax=HA+std)) + 
  geom_pointrange(aes(color=COVID19), shape=20, size = 0.8, alpha =1) +
  scale_fill_brewer(palette="Oranges") +
  annotate("text", x=BL_dt$HA , y=BL_dt$label+0.4, label=BL_seasons, size=2.5) +
  theme(axis.title.y=element_text(size=7)) +
  scale_x_continuous(breaks=seq(-1, 1, 0.1)) +
  geom_vline(xintercept = mean(BL_dt$HA[1:9]), linetype="dashed", color = 'blue', size =0.8, alpha = 0.3) +
  scale_y_continuous(breaks=NULL) +
  theme(axis.text.y=element_blank()) +
  labs(title='E') +
  ylab(' ') +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=10)) +
  coord_cartesian(xlim = c(-0.1, 0.7)) +
  theme(plot.title = element_text(hjust = -0.1, vjust = 0, size=20, face='bold')) +
  scale_color_discrete(breaks=c("Before","After")) +
  labs(colour = "COVID-19") +
  annotate('text', x=0.55, y=1, label = 'Bundesliga', fontface = 'italic', size = 4) +
  theme(legend.position="none")

BL_plot


get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

pl_legend <- get_legend(PL_plot)
PL_plot
pplot <- ggarrange(PL_plot, LIGA_plot, SE_plot, BL_plot, ncol=2, nrow=2, common.legend = FALSE, legend="none")

ppplot <- annotate_figure(pplot, bottom =text_grob("HA", x=unit(0.51, "npc"),y=unit(0.7, 'npc'),  size=20))
grid.arrange(PL2_plot, ppplot, nrow=1)

all_samples <- rbind(output0[,1:23], output2[,1:23], output4[,1:23], output6[,1:23])
colnames(away_b)
colnames(home_b)
home_b <- all_samples[, str_detect(string = colnames(all_samples), "home_baseline\\[")] 
away_b <- all_samples[, str_detect(string = colnames(all_samples), "away_baseline\\[")] 
HA <- exp(home_b) - exp(away_b)
HA_before <- HA[,1:9] %>% pull() %>% c()
HA_after <- HA[,10] %>% c() 

exp(away_b)
exp(home_b)-exp(away_b)

mean(HA_before)
mean(HA_after)

dim(HA_before)
dim(HA_after)
HA_before <- HA_before %>% as.data.frame() %>% mutate('COVID' = 'Before')
HA_after <- HA_after %>% as.data.frame() %>% mutate('COVID' = 'After')
ab <- rbind(HA_before, HA_after) %>% as.data.frame()
colnames(ab) <- c('HA', 'COVID')


ab$COVID <- factor(ab$COVID, levels = c("Before", "After"))
be_m <- mean(HA_before$.)
af_m <- mean(HA_after$.)

ab
tt <- ggplot(ab, aes(x = HA, fill=COVID)) +
  geom_density(alpha=0.3, outline.type = "upper" , size=1) +
  geom_vline(xintercept = be_m, lty = 'dashed', color = 'blue') +
  geom_vline(xintercept = af_m, lty = 'dashed', color = 'red') +
  annotate("text", x= be_m+0.014 , y= 1, label='mean_before', size=5,
           fontface = 2, angle = 90, color = 'blue') +
  annotate("text", x= af_m+0.014 , y= 1, label='mean_after', size=5, 
           fontface = 2, angle = 90, color = 'red') +
  labs(x='Home advantage', y='Density') +
  theme(legend.position = "right") +
  guides(fill = guide_legend(title = 'COVID-19', reverse = TRUE))
  
tt

############################ plot 3 skill

plot2_data <- function(league, samples) {
  skills <- samples[, str_detect(string = colnames(samples), "skill\\[10")] %>% as.matrix()
  home_base <- samples[, "home_baseline[10]"] %>% pull()
  away_base <- samples[, "away_baseline[10]"] %>% pull()
  skills <- skills + home_base + away_base
  skills <- skills - mean(skills)
  
  aa <- as.data.frame(colMeans(as.matrix(skills)))
  colnames(aa) <- 'Skill'
  std <- apply(t(as.matrix(skills)), 1, sd)*1.69
  HA <- aa$HA
  aa$std <- std
  return(aa)
}
output0[, str_detect(string = colnames(output0), "skill\\[10")] %>% as.matrix() - output0[, "home_baseline[10]"] %>% pull()

team_name_list
load(file='team_name_list.rdata')

class(bases3) %>% pulls

bases3[, "away_baseline[10]"] 
abc[[1]]
pl2_dt <- plot2_data("PL", output0)
pl2_dt$team_name <- team_name_list[[1]]
exception <- c('Huddersfield', 'Cardiff', 'Hull', 
               'Queens Park Rangers', 'Bolton', 'Blackburn', 'Norwich',
               'Sunderland', 'Reading', 'Watford', 'Wigan', 'Middlesbrough', 'Stoke',
               'Swansea', 'Bouremouth')

'%ni%' <- Negate('%in%')
pl2_dt <- pl2_dt %>% filter(team_name %ni% exception)
pl2_dt <- pl2_dt %>% arrange(., Skill) %>% mutate('label' = c(1:nrow(pl2_dt)))
pl2_dt$Skill <- pl2_dt$Skill - mean(pl2_dt$Skill)

PL2_plot<- ggplot(data=pl2_dt, aes(x=Skill, y=label, xmin=Skill-std, xmax=Skill+std)) + 
  geom_pointrange(shape=20, size = 0.8, alpha = 0.5, color ='blue') +
  annotate("text", x=pl2_dt$Skill , y=pl2_dt$label+0.5, label=pl2_dt$team_name, size=2.5) +
  theme(axis.title.y=element_text(size=10)) +
  geom_vline(xintercept = mean(pl2_dt$Skill), linetype="dashed", color = 'red', size =0.8, alpha = 0.3) +
  scale_y_continuous(breaks=NULL) +
  scale_x_continuous(breaks=seq(-1, 1, 0.1)) + 
  theme(axis.text.y=element_blank()) + 
  coord_cartesian(xlim = c(-0.5, 0.7)) +
  labs(title='A') +
  xlab('SKILL') +
  ylab('     ') +
  theme(axis.text.x=element_text(size = 14), axis.title.x=element_text(size=20), 
        plot.title=element_text(size=20, hjust = -0.05, face='bold'),
        axis.title.y=element_text(size=18))

PL2_plot


grid.arrange(PL2_plot, ppplot, nrow=1)
last_plot()
ggsave('test.tiff', plot = grid.arrange(PL2_plot, ppplot, nrow=1),
       units='in', width = 10, height =5, dpi = 200)

team_name_list[[1]]

pl2_dt_temp <- plot2_data("PL", output0)
pl2_dt_temp



 PPP1 <- read_csv('After_COVID_PL.csv')
PPP2 <- read_csv('After_COVID_LIGA.csv')
PPP3 <- read_csv('After_COVID_SE.csv')
PPP4 <- read_csv('After_COVID_BL.csv')

PPP1 %>% filter(season=='PL1920') %>% nrow()
PPP2 %>% filter(season=='LIGA1920') %>% nrow()
PPP3 %>% filter(season=='SE1920') %>% nrow()
PPP4 %>% filter(season=='BL1920') %>% nrow()

test_PL = read_csv('PL_test_pp.csv')
test_PL %>% filter(home == 'Liverpool', away == 'Tottenham') %>% select(h_skill, a_skill, home_bs, away_bs)

exp(0.272 + 0.772)

########

bl2_dt <- plot2_data("BL", output6)
bl2_dt$team_name <- team_name_list[[4]]
exception_bl <- c('Hannover 96', 'Werder Bremen', 'Hamburger SV', 
               'Greuther Fuerth', 'Fortuna Duesseltorf', 'Eintracht Braunschweig',
               'Paderborn', 'Darmstadt', 'Ingolstadt', 'Union Berlin',
               'Arminia Bielefeld')


bl2_dt <- bl2_dt %>% filter(team_name %ni% exception_bl)
bl2_dt <- bl2_dt %>% arrange(., Skill) %>% mutate('label' = c(1:nrow(bl2_dt)))
bl2_dt$Skill <- bl2_dt$Skill - mean(bl2_dt$Skill)

BL2_plot<- ggplot(data=bl2_dt, aes(x=Skill, y=label, xmin=Skill-std, xmax=Skill+std)) + 
  geom_pointrange(shape=20, size = 0.8, alpha = 0.5, color ='blue') +
  annotate("text", x=bl2_dt$Skill , y=bl2_dt$label+0.5, label=bl2_dt$team_name, size=3) +
  theme(axis.title.y=element_text(size=10)) +
  geom_vline(xintercept = mean(pl2_dt$Skill), linetype="dashed", color = 'red', size =0.8, alpha = 0.3) +
  scale_y_continuous(breaks=NULL) +
  scale_x_continuous(breaks=seq(-1, 1, 0.1)) + 
  theme(axis.text.y=element_blank()) + 
  coord_cartesian(xlim = c(-0.5, 0.7)) +
  labs(title='A') +
  xlab('SKILL') +
  ylab('     ') +
  theme(axis.text.x=element_text(size = 14), axis.title.x=element_text(size=20), 
        plot.title=element_text(size=20, hjust = -0.05, face='bold'),
        axis.title.y=element_text(size=18))

BL2_plot

bl2_dt
pl2_dt
0.5613 
0.3007

0.3007-0.077
0.561 -0.391
+ 0.242 + 0.633

0.242+0.633

0.561 - 0.242

-0.633 + 0.319
exp(-0.561 -0.314 + 0.424)

exp(0.077 + 0.272)


