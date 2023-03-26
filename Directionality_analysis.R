require(tidyverse)
require(dplyr)
require(lmerTest)
require(rstatix)
require(lm.beta)

# load data
data <- read_csv('data_IMAGEN_ctqcutoff.csv') %>%
  convert_as_factor(id, sex, hand, site)

####################################
# Cross-prediction model
####################################

# The rate of change in the factor weight between ages 14 and 19 years
data$delta_an1 <- (data$an1_time19 - data$an1_time14) / (data$an1_time14 + 1)
data$delta_an2 <- (data$an2_time19 - data$an2_time14) / (data$an2_time14 + 1)
data$delta_an3 <- (data$an3_time19 - data$an3_time14) / (data$an3_time14 + 1)
data$delta_ap1 <- (data$ap1_time19 - data$ap1_time14) / (data$ap1_time14 + 1)
data$delta_ap2 <- (data$ap2_time19 - data$ap2_time14) / (data$ap2_time14 + 1)
# The rate of change in the emotional symptom score between ages 14 and 19 years
data$delta_emotion <- (data$semotion_19 -data$semotion_14)  /  (data$semotion_14 + 1)

data_girl <- data[data$sex==0,]

data_girl <- as.data.frame(data_girl)

##### The factor weights at age 14 years -> The rate of change in the emotional symptom score ######
index_h <- as.numeric(which(colnames(data) %in% c('an1_time14', 'an2_time14', 'an3_time14',
                                                  'ap1_time14', 'ap2_time14')))
index_delta <- as.numeric(which(colnames(data) %in% c('delta_an1', 'delta_an2', 'delta_an3',
                                                      'delta_ap1', 'delta_ap2')))
index_all <- list(index_h=index_h, index_delta=index_delta)

for (i in 1:length(index_h)){
  i_h <- index_all$index_h[i]
  i_delta <- index_all$index_delta[i]
  
  fit.lm <- lm(delta_emotion ~ data_girl[, i_h] + data_girl[, i_delta] * ctqabuse * mdd_mean + ses + site  + hand + bmi14  + puberty+
                 + semotion_14 + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data_girl)
  
  fit.beta <- lm.beta(fit.lm)
  
  a <- as.data.frame(coef(summary(fit.beta))[1:2, ])
  rownames(a) <- c(paste0("intercept_", colnames(data_girl)[i_h]), 
                   paste0(colnames(data_girl)[i_h]))
  
  if (colnames(data_girl)[i_h] == 'an1_time14'){
    result_delta <- a
  }
  else{
    result_delta <- rbind(result_delta, a)
  }
  
}

# five latent factors
row_odd <- seq_len(nrow(result_delta)) %% 2
result_delta <- result_delta[row_odd==0, ]

# Three factors that modulate the the interaction between PRS_MDD and childhood abuse
result_delta <- result_delta[c(1, 4, 5), ]

# FDR-p 
result_delta$FDR_p <- p.adjust(result_delta$`Pr(>|t|)`, method='fdr')


##### The baseline emotional symptom score -> The rate of change in the factor weight ######
fit.lm <- lm(delta_ap1 ~ semotion_14 + delta_emotion * ctqabuse * mdd_mean + ses + site  + hand + bmi14 + 
             puberty + ap1_time14 + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data_girl)
fit.beta <- lm.beta(fit.lm)
summary(fit.beta)

########  permutation testing ###########
set.seed(1234)
permutation <-  1000
allobservations <-  data_girl$delta_emotion
t_permute <- rep(0, permutation)
for (i in 1:permutation){
  # selecting either next combination, or random permutation
  permutation_index <-  sample(1:length(allobservations), length(allobservations))
  # creating random sample based on permutation index
  randomSample <- allobservations[permutation_index]
  # running the LM for this permutation
  fit.lm <- lm(randomSample ~ ap1_time14 + delta_ap1 * ctqabuse * mdd_mean + ses + site  + hand + bmi14 + puberty+
                 semotion_14 + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data_girl)
  t_permute[i] <- coefficients(summary(fit.lm))[2, 3]
}

fit.lm <- lm(delta_emotion ~ ap1_time14 + delta_ap1 * ctqabuse * mdd_mean + ses + site  + hand + bmi14 + puberty+
               semotion_14 + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data_girl)
t0 <- coefficients(summary(fit.lm))[2, 3]

p_permute <- sum(abs(t0) < abs(t_permute)) / permutation


####################################
# Cross-lagged panel model
####################################

if (!require(lavaan)) install.packages('lavaan')
if (!require(tidySEM)) install.packages('tidySEM')
if (!require(semPlot)) install.packages('semPlot')
if (!require(tidyverse)) install.packages('tidyverse')
library(lavaan)
library(tidyverse)
library(ggplot2)
library(semPlot)
library(tidySEM)
library(rstatix)
options(warn=-1)

clpmModel <- 
  '
  #Note, the data contain x1-2 and y1-2
  #regressions
  x2 ~ alpha2*x1 + beta2*y1 + bmi19 + hand + ses + s1+s2+s3+s4+s5+s6+s7 + ctqabuse + mdd_mean + PC1 +
  PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8
  y2 ~ delta2*y1 + gamma2*x1+ bmi19 + hand + ses + s1+s2+s3+s4+s5+s6+s7 +ctqabuse + mdd_mean + PC1 +
  PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8
  x1 ~ bmi14 + hand + ses + puberty + s1+s2+s3+s4+s5+s6+s7 +ctqabuse + mdd_mean + PC1 +
  PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8
  y1 ~ bmi14 + hand + ses + puberty + s1+s2+s3+s4+s5+s6+s7 +ctqabuse + mdd_mean + PC1 +
  PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8
  
  x1 ~~ x1 #variance
  x2 ~~ u2*x2
  y1 ~~ y1 #variance
  y2 ~~ v2*y2
  
  x1 ~~ y1 #x1 and y1 covariance
  x2 ~~ y2
'

# model ap1
colnames(data_girl)[c(5,10,37,38)] <- c("x1", "x2", "y1", "y2")  # x -- ap1; y -- emotion
fit.clpmModel <- lavaan(clpmModel, data = data_girl,
                        missing = 'ML', #for the missing data!
                        int.ov.free = F,
                        int.lv.free = F,
                        auto.fix.first = F,
                        auto.fix.single = F,
                        auto.cov.lv.x = F,
                        auto.cov.y = F,
                        auto.var = F)
result <- summary(fit.clpmModel, standardized = TRUE, fit.measures = TRUE)
result$pe[which(result$pe$label == 'gamma2'), 'est'] # gamma2

########### bootstrap (stratified random sampling: site) ##############
# divides a population into groups and selects a random number of people 
# from each site to be included in the sample.
library(boot)

foo <- function(data, indices, clpm){
  dt <- data[indices, ]
  fit.clpmModel <- lavaan(clpm, data = dt,
                          missing = 'ML', #for the missing data!
                          int.ov.free = F,
                          int.lv.free = F,
                          auto.fix.first = F,
                          auto.fix.single = F,
                          auto.cov.lv.x = F,
                          auto.cov.y = F,
                          auto.var = F)
  result <- summary(fit.clpmModel, standardized = TRUE, fit.measures = TRUE)
  result$pe[which(result$pe$label == 'gamma2'), 'est']
  
}

set.seed(1234)
# parallel
system.time(bootstrap <- boot(data_girl, foo, R=1000, strata=data_girl$site, parallel = "multicore", 
                              ncpus=6, clpm=clpmModel))

saveRDS(bootstrap, file='clpm_bootstrap.Rds')


bootstrap <- readRDS('clpm_bootstrap.Rds')
plot(bootstrap)
# 95%CI
boot.ci(bootstrap, type='perc')

# test: stratified random sampling: site
table(data_girl$site) # 0: 73, 1: 55, 2: 50, 3: 27, 4: 50, 5: 56, 6: 60, 7: 59
tableOfIndices<-boot.array(bootstrap, indices=F)
tableOfIndices <- t(tableOfIndices)
tableOfIndices <- data.frame(tableOfIndices)
tableOfIndices$id <- data_girl$id
tableOfIndices$site <- data_girl$site
tableOfIndices <- tableOfIndices[tableOfIndices$X1!=0, ]

result_tot <- tableOfIndices
for (i in 1:nrow(tableOfIndices)){
  if (tableOfIndices[i, 1] > 1){
    re <- tableOfIndices[rep(i, tableOfIndices[i, 1] - 1), ]
    result_tot <- rbind(result_tot, re)
  }
}
table(result_tot$site)
