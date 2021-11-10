library(mltools)
library(data.table)


data_test_indep
tppl <- rbind(read_csv('PL_test_pp_new.csv'), read_csv('LIGA_test_pp_new.csv'), 
             read_csv('SE_test_pp_new.csv'), read_csv('BL_test_pp_new.csv'))

ppl <- tppl %>% 
  mutate(lambda_h = home_bs + h_skill - a_skill, 
               lambda_a = away_bs + a_skill - h_skill) %>% 
  select(home, away, full_home_score, full_away_score, result, lambda_h, lambda_a)

ppl <- tppl %>% 
  mutate(lambda_h = home_bs_fixed + h_skill_fixed - a_skill_fixed, 
         lambda_a = away_bs_fixed + a_skill_fixed - h_skill_fixed) %>% 
  select(home, away, full_home_score, full_away_score, result, lambda_h, lambda_a)



score_prediction <- function(lambda_h, lambda_a) {
  h_sample = rpois(10000, exp(lambda_h))
  a_sample = rpois(10000, exp(lambda_a))
  sample_dt <- rbind(h_sample, a_sample)
  win_rate <- sum(h_sample > a_sample)/10000
  draw_rate <- sum(h_sample == a_sample)/10000
  lose_rate <- sum(h_sample < a_sample)/10000
  return(list(win_rate, draw_rate, lose_rate))
}

score <- c()
for (i in 1:nrow(ppl)) {
  lambda_h <- ppl[i,'lambda_h'] %>% pull()
  lambda_a <- ppl[i,'lambda_a'] %>% pull()
  output <- score_prediction(lambda_h, lambda_a)
  score <- rbind(score, c(output[[1]], output[[2]], output[[3]]))
}

#apply(score, 1, which.max) * -1 +2
#sum(ppl$result == apply(score, 1, which.max) * -1 +2)/nrow(ppl)

one_h = one_hot(as.data.table(ppl$result %>% as.factor()))
score_ = score[,order(ncol(score):1)]
sum((one_h - score_)**2)/(nrow(one_h)-1)/2

RPS(score_, one_h)



RPS <- function(probability, answer){
  output <- ((probability - answer)**2)[,3] 
  output2 <- (rowSums((probability-answer)[,2:3]))**2
  output <- output + output2
  output <- output/2
  output <- mean(output %>% pull() %>% c())
  return(output)
}

RPS(score_, one_h)

sum(c(0.2011 + 0.2003 + 0.))

((score_ - one_h)**2)[,3] + rowSums((score_ - one_h)[,2:3])**2

## examples
tppl %>% filter(home=='Liverpool', away=='Tottenham') %>% select(h_skill, a_skill, home_bs, away_bs)

ppl %>% filter(home=='Liverpool', away=='Tottenham')
h_sample <- rpois(10000, exp(0.349))
a_sample <- rpois(10000, exp(0.135))
t_score <- score_prediction(0.349, 0.135)
sample_dt <- cbind(h_sample, a_sample) %>% as.data.frame()
table(h_sample, a_sample)


rnorm(1000, 0, 1)
s_plot1 <- ggplot(sample_dt, aes(h_sample+rnorm(1000, 0, 0.2),
                      a_sample+rnorm(1000, 0, 0.2))) +
  stat_density_2d(geom = "raster", aes(fill = after_stat(density)), contour = FALSE) + 
  scale_fill_viridis_c(option = "mako") +
  geom_abline(aes(intercept=0, slope=1), linetype='dashed', color='red', alpha=0.3, size=2) +
  scale_x_continuous(limits = c(-0.5, 5)) +
  scale_y_continuous(limits = c(-0.5, 5)) + 
  xlab('Home team goals') +
  ylab('Away team goals') +
  labs(title = 'Liverpool FC(Home) vs Tottenham Hotspur(Away)') +
  annotate("point", x=2, y=1, color = 'red', alpha = 0.8, size =4) +
  annotate("text", x=2, y=0.75, label='actual result', color = 'red', size=6) +
  theme(legend.position = "none")  +
  theme(axis.title = element_text(size = 15), plot.title = element_text(size=13))

s_plot1

?viridisLite::viridis()
magma
inferno
plasma
cividis
rocket
mako
turbo

tppl %>% filter(home=="Schalke 04" , away=="Bayern Munich") %>% select(h_skill, a_skill, home_bs, away_bs)
ppl %>% filter(home=="Schalke 04" , away=="Bayern Munich")
h_sample <- rpois(10000, exp(-0.443))
a_sample <- rpois(10000, exp(1.17))
t_score <- score_prediction(-0.443, 1.17)
sample_dt <- cbind(h_sample, a_sample) %>% as.data.frame()
table(h_sample, a_sample)

s_plot2 <- ggplot(sample_dt, aes(h_sample+rnorm(1000, 0, 0.2),
                                 a_sample+rnorm(1000, 0, 0.2))) +
  stat_density_2d(geom = "raster", aes(fill = after_stat(density)), contour = FALSE) + 
  scale_fill_viridis_c(option = "mako") +
  geom_abline(aes(intercept=0, slope=1), linetype='dashed', color='red', alpha=0.3, size=2) +
  scale_x_continuous(limits = c(-0.5, 5)) +
  scale_y_continuous(limits = c(-0.5, 5)) + 
  xlab('Home team goals') +
  ylab('Away team goals') +
  labs(title = 'Schalke 04(Home) vs Bayern Munich(Away)') +
  annotate("point", x=0, y=4, color = 'red', alpha = 0.8, size =4) +
  annotate("text", x=0.6, y=3.75, label='actual result', color = 'red', size=6) +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 15), plot.title = element_text(size=13))


ggarrange(s_plot1, s_plot2, labels= c('A','B'))


ggsave('test.tiff', plot = ggarrange(s_plot1, s_plot2, labels= c('A','B')),
       units='in', width = 10, height =5, dpi = 200)

-1/1.37**2


200/10000
s_plot1
s_plot2

exp(-0.633-0.242+0.432)



aa <- read_csv('Before_COVID_BL.csv')
bb <- read_csv('After_COVID_BL.csv')
rbind(aa, bb) %>% nrow()
3060/10

18*17

package_version('rjags')
library('utils')
packageVersion('rjags')
R.version

(1-0.73)*0.73
