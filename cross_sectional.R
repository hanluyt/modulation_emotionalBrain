require(tidyverse)
require(dplyr)
require(lmerTest)
require(rstatix)
require(lm.beta)

# load data
data <- read_csv('data_IMAGEN.csv') %>%
  convert_as_factor(id, sex, hand, site)

# descriptive statistics
table(data$sex) # girl: 430, boy: 379
table(data$hand) # left: 113, right: 696

stat <- data %>%
  summarise_if(
    is.numeric, 
    list(avg=~mean(.), std=~sd(.)),
    na.rm=TRUE)


data <- as.data.frame(data)
data_girl <- data[data$sex == 0, ] # 430
data_boy <- data[data$sex == 1, ] # 379

####################################
# Association analysis 1
# emotional symptom score at age 19 ~ childhood abuse * PRS_MDD + covariates
####################################

# all participants
fit.lm <- lm(semotion_19 ~ ctqabuse * mdd_mean + sex + ses + site + hand + bmi19 + 
               PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data)
summary(fit.lm)


# for girls and boys separately
fit.girl <- lm(semotion_19 ~ ctqabuse * mdd_mean + ses + site + hand + bmi19 + 
               PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data_girl)
summary(fit.girl)

fit.boy <- lm(semotion_19 ~ ctqabuse * mdd_mean + ses + site + hand + bmi19 + 
                 PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data_boy)
summary(fit.boy)


####################################
# Association analysis 2
# the weights of the latent factors at age 19  ~ childhood abuse * PRS_MDD + covariates
####################################

index_h <- as.numeric(which(colnames(data) %in% c('an1_time19', 'an2_time19', 'an3_time19',
                                                  'ap1_time19', 'ap2_time19')))

# run 5 regressions and extract coefficients
association_weight <- function(data, index_h){
  for (i in index_h){
    if (nrow(data) == 809){
      # all participants
      fit.lm <- lm(data[, i] ~ ctqabuse * mdd_mean + sex + ses + site  + hand + bmi19 + 
                     semotion_19 + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data)
      
      # extract coefficients for childhood abuse (CA), PRS_MDD, sex, emotion_19, CA-by-PRS_MDD
      a <- coef(summary(fit.lm))[c(2, 3, 4, 15, 24), ] 
    }
    else{
      # for girls and boys separately
      fit.lm <- lm(data[, i] ~ ctqabuse * mdd_mean + ses + site  + hand + bmi19 + 
                     semotion_19 + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data)
      # extract coefficients for childhood abuse (CA), PRS_MDD, emotion_19, CA-by-PRS_MDD
      a <- coef(summary(fit.lm))[c(2, 3, 14, 23), ] 
    }
    rownames(a) <- paste0(rownames(a), '_', colnames(data)[i])
    
    if (colnames(data)[i] == "an1_time19"){
      result <- a
    } else{
      result <- rbind(result, a)
    }
  }
  return(result)
}


result_weight_all <- association_weight(data, index_h)
result_weight_gril <- association_weight(data_girl, index_h)
result_weight_boy <- association_weight(data_boy, index_h)


####################################
# Modulation analysis
# emotional symptom score at age 19  ~ the weight of factor at age 19 * childhood abuse * PRS_MDD
# + covariates
####################################

modulate <- function(data, index_h){
  for (i in index_h){
    # for girls and boys separately
    fit.lm <- lm(semotion_19 ~ data[, i] * ctqabuse * mdd_mean + ses + site  + hand + bmi19 + 
                   PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data)
    
    # extract coefficients for weight, CA, PRS_MDD, weight-by-CA-by-PRS_MDD
    a <- coef(summary(fit.lm))[c(2, 3, 4, 26), ] 
    
    rownames(a)[c(1, 4)] <- c(colnames(data)[i], paste0(colnames(data)[i], ":", "ctq:mdd"))
    
    if (colnames(data)[i] == "an1_time19"){
      result <- a
    } else{
      result <- rbind(result, a)
    }
  }
  return(result)
}


result_modulate_boy <- modulate(data_boy, index_h)
three_way_boy <- as.data.frame(result_modulate_boy[c(4, 8, 12, 16, 20), ])
# FDR-p
three_way_boy$FDR_p <- p.adjust(three_way_boy$`Pr(>|t|)`, method='fdr')


result_modulate_girl <- modulate(data_girl, index_h)
three_way_girl <- as.data.frame(result_modulate_girl[c(4, 8, 12, 16, 20), ])
# FDR-p
# Angry activated factor 1 and 2, neutral activated factor 1 may be relevant to resilience
three_way_girl$FDR_p <- p.adjust(three_way_girl$`Pr(>|t|)`, method='fdr')

# post doc analysis to see sex effect
index_modulate <- as.numeric(which(colnames(data) %in% c('an1_time19', 'ap1_time19', 'ap2_time19')))

for (i in index_modulate){
  fit.lm <- lm(semotion_19 ~ data[, i] * ctqabuse * mdd_mean * sex + ses + site  + hand + bmi19 + 
                 PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data)
  
  a <- coef(summary(fit.lm))[34,]
  
  if (colnames(data)[i] == "an1_time19"){
    result_sex <- a
  } else{
    result_sex <- rbind(result_sex,a)
    }
}


result_sex <- as.data.frame(result_sex)
rownames(result_sex) <- c('an1_time19_4way', 'ap1_time19_4way', 'ap2_time19_4way')
result_sex$FDR_p <- p.adjust(result_sex$`Pr(>|t|)`, method='fdr')
  





