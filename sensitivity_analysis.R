require(tidyverse)
require(dplyr)
require(lmerTest)
require(rstatix)
require(lm.beta)

# load data
data <- read_csv('data_IMAGEN.csv') %>%
  convert_as_factor(id, sex, hand, site)

index_modulate <- as.numeric(which(colnames(data) %in% c('an1_time19', 'ap1_time19', 'ap2_time19')))

####################################
# Sensitivity analyses
####################################

########## childhood abuse score was binarized by the cut-offs: ###########
# 8 for emotional abuse, 7 for physical abuse and 5 for sexual abuse
data$ctqcutoff <- ifelse(data$emotion_abuse <= 8 & data$physical_abuse <= 7 &
                           data$sex_abuse <= 5, 0, 1)
table(data$ctqcutoff) # 0: 634, 1:175
data$ctqcutoff <- as.factor(data$ctqcutoff)

data_girl <- data[data$sex == 0, ] # 430
data_boy <- data[data$sex == 1, ] # 379

fit.lm <- lm(semotion_19 ~ an1_time19 * ctqcutoff * mdd_mean + ses + site  + hand + bmi19 +
               PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data_girl)
a <- coef(summary(fit.lm))[26,]

fit.lm <- lm(semotion_19 ~ ap1_time19 * ctqcutoff * mdd_mean + ses + site  + hand + bmi19 + 
               PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data_girl)
result_cutoff <- rbind(a, coef(summary(fit.lm))[26,])

fit.lm <- lm(semotion_19 ~ ap2_time19 * ctqcutoff * mdd_mean + ses + site  + hand + bmi19 + 
               PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data_girl)

result_cutoff <- as.data.frame(rbind(result_cutoff, coef(summary(fit.lm))[26,]))
rownames(result_cutoff) <- c('an1_time19_3way', 'ap1_time19_3way', 'ap2_time19_3way')

########## age, IQ or substance use ###########
# age
fit.lm <- lm(semotion_19 ~ an1_time19 * ctqabuse * mdd_mean + ses + site  + hand + bmi19 + age19 +
               PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data_girl)
a <- coef(summary(fit.lm))[27,]

fit.lm <- lm(semotion_19 ~ ap1_time19 * ctqabuse * mdd_mean + ses + site  + hand + bmi19 + age19 +
               PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data_girl)
result_age <- rbind(a, coef(summary(fit.lm))[27,])

fit.lm <- lm(semotion_19 ~ ap2_time19 * ctqabuse * mdd_mean + ses + site  + hand + bmi19 + age19 +
               PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data_girl)
result_age <- as.data.frame(rbind(result_age, coef(summary(fit.lm))[27,]))
rownames(result_age) <- c('an1_time19_3way', 'ap1_time19_3way', 'ap2_time19_3way')

# IQ
iq_csv <- read_csv('IQ_sen.csv')

data_girl_confounding <- data_girl %>%
  merge(iq_csv, by='id')


for (i in index_modulate){
  fit.lm <- lm(semotion_19 ~ data_girl_confounding[, i] * ctqabuse * mdd_mean + ses + 
                 site + hand+ bmi19 + IQ + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                 PC7 + PC8, data=data_girl_confounding)
  
  a <- coef(summary(fit.lm))[27,]
  
  if (colnames(data_girl_confounding)[i] == "an1_time19"){
    result_IQ <- a
  } else{
    result_IQ <- rbind(result_IQ, a)
  }
}

# substance use
substance <- read_csv('substance_sen.csv')
data_girl_confounding <- data_girl %>%
  merge(substance, by='id')

for (i in 44:46){
  for (j in index_modulate){
    # for girls
    fit.lm <- lm(semotion_19 ~ data_girl_confounding[, j] * ctqabuse * mdd_mean + ses + site + hand +
                   bmi19 + data_girl_confounding[, i] + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +
                   PC7 + PC8, data=data_girl_confounding)
    
    a <- as.data.frame(coef(summary(fit.lm))[c(1, 27), ])
    rownames(a) <- c(paste0("intercept_", colnames(data_girl_confounding)[j]), 
                     paste0(colnames(data_girl_confounding)[i],"_", colnames(data_girl_confounding)[j]))
    
    if (colnames(data_girl_confounding)[i] == "smoking" && 
        colnames(data_girl_confounding)[j] == 'an1_time19'){
      result_substance <- a
    } else{
      result_substance <- rbind(result_substance, a)}
  }
}

result_substance <- result_substance[seq(0, nrow(result_substance), 2), ]


######  replacing the PRS_MDD with the PRS_ADHD or the PRS_SCZ #########  
prs <- read_csv('prs_scz_adhd.csv')
data_girl_prs <- data_girl %>%
  merge(prs, by='id')

for (i in 44:45){
  for (j in index_modulate){
    # for girls
    fit.lm <- lm(semotion_19 ~ data_girl_prs[, j] * ctqabuse * data_girl_prs[, i] + ses + 
                   site + hand + bmi19 + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 
                 + PC7 + PC8, data=data_girl_prs)
    
    a <- as.data.frame(coef(summary(fit.lm))[c(1, 26), ])
    rownames(a) <- c(paste0("intercept_", colnames(data_girl_prs)[j]), 
                     paste0(colnames(data_girl_prs)[j],"_abuse_", 
                            colnames(data_girl_prs)[i]))
    
    if (colnames(data_girl_prs)[i] == "scz_mean" && 
        colnames(data_girl_prs)[j] == 'an1_time19'){
      result_prs <- a
    } else{
      result_prs <- rbind(result_prs, a)}
  }
}

result_prs <- result_prs[seq(0, nrow(result_prs), 2), ]


######### replacing the emotional symptom score with the conduct symptom score or 
#               hyperactivity/inattention symptom score  #############
sdq <- read_csv('conduct_hyper.csv')
data_girl_sdq <- data_girl %>%
  merge(sdq, by='id')

for (i in 44:45){
  for (j in index_modulate){
    # for girls
    fit.lm <- lm(data_girl_sdq[, i] ~ data_girl_sdq[, j] * ctqabuse * mdd_mean + ses + 
                   site + hand + bmi19 + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 
                 + PC7 + PC8, data=data_girl_sdq)
    
    a <- as.data.frame(coef(summary(fit.lm))[c(1, 26), ])
    rownames(a) <- c(paste0("intercept_", colnames(data_girl_sdq)[j]), 
                     paste0(colnames(data_girl_sdq)[i],"_",
                            colnames(data_girl_sdq[j])))
    
    if (colnames(data_girl_sdq)[i] == "sconduct_19" && 
        colnames(data_girl_sdq)[j] == 'an1_time19'){
      result_sdq <- a
    } else{
      result_sdq <- rbind(result_sdq, a)}
  }
}

result_sdq <- result_sdq[seq(0, nrow(result_sdq), 2), ]


##### replacing the emotional symptom score with either the depression 
#              or the generalized anxiety score   #############
dawba <- read_csv('anxiety_depression.csv')
data_girl_dawba <- data_girl %>%
  merge(dawba, by='id')

for (i in 44:45){
  for (j in index_modulate){
    # for girls
    fit.lm <- lm(data_girl_dawba[, i] ~ data_girl_dawba[, j] * ctqabuse * mdd_mean + ses + 
                   site + hand + bmi19 + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 
                 + PC7 + PC8, data=data_girl_dawba)
    
    a <- as.data.frame(coef(summary(fit.lm))[c(1, 26), ])
    rownames(a) <- c(paste0("intercept_", colnames(data_girl_dawba)[j]), 
                     paste0(colnames(data_girl_dawba)[i],"_",
                            colnames(data_girl_dawba[j])))
    
    if (colnames(data_girl_dawba)[i] == "anxiety_19" && 
        colnames(data_girl_dawba)[j] == 'an1_time19'){
      result_dawba <- a
    } else{
      result_dawba <- rbind(result_dawba, a)}
  }
}

result_dawba <- result_dawba[seq(0, nrow(result_dawba), 2), ]



