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
# the weights of the latent factors at age 19  ~ SDQ symptoms at age 19 +
#                                                childhood abuse * PRS_MDD + other covariates
####################################
sdq <- read_csv('sdq_19_0411.csv') 
sdq <- sdq[complete.cases(sdq), ]

data <- merge(data, sdq, by='id')

# normalization
i <- c(seq(2, 13), 22, 23, 25, seq(27, 47))
data[, i] <- apply(data[, i], 2, function(x) scale(x))


index_h <- as.numeric(which(colnames(data) %in% c('an1_time19', 'an2_time19', 'an3_time19',
                                                  'ap1_time19', 'ap2_time19')))

index_sdq <- as.numeric(which(colnames(data) %in% c('semotion', 'sconduct', 'shyper',
                                                  'speer', 'sprosoc')))


# Behavioral associations of the latent foctors with various symptoms in the SDQ at age 19
association_weight <- function(data, index_h, index_sdq){
  for (i in index_h){
    for (j in index_sdq){
      if (nrow(data) == 809){
        # all participants
        fit.lm <- lm(data[, i] ~ data[, j] + ctqabuse * mdd_mean + sex + ses + site  + hand + bmi19 + 
                       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data)
        
        # extract coefficients for symptoms, childhood abuse (CA), PRS_MDD, sex, CA-by-PRS_MDD
        if (colnames(data[j]) == 'semotion'){
          a <- coef(summary(fit.lm))[c(2, 3, 4, 5, 24), ] 
        }
        else {
          a <- coef(summary(fit.lm))[c(2, 5), ] 
        }
      }
      else{
        # for girls and boys separately
        fit.lm <- lm(data[, i] ~ data[, j] + ctqabuse * mdd_mean + ses + site  + hand + bmi19 + 
                       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data)
        # extract coefficients for symptoms, childhood abuse (CA), PRS_MDD, CA-by-PRS_MDD
        if (colnames(data[j]) == 'semotion'){
          a <- coef(summary(fit.lm))[c(2, 3, 4, 23), ] 
        }
        else {
          a <- coef(summary(fit.lm))[c(2, 3), ] 
        }
      }
      rownames(a)[1] <- colnames(data)[j]
      rownames(a) <- paste0(rownames(a), '_', substr(colnames(data)[i], 1, 3))
      
      if (colnames(data)[i] == "an1_time19" & colnames(data)[j] == 'semotion'){
        result <- a
      } else{
        result <- rbind(result, a)
      }
    }
    
  }
  return (result)
}


############## for all participants ################
result_weight_all <- association_weight(data, index_h, index_sdq)
result_weight_all <- result_weight_all[-c(7, 9, 11, 13, 20, 22, 24, 26, 33, 35, 37, 39,
                                          46, 48, 50, 52, 59, 61, 63, 65), ]
result_weight_all <- as.data.frame(result_weight_all)
a <- row.names(result_weight_all)
result_weight_all <- as_tibble(result_weight_all)
result_weight_all <- result_weight_all %>% 
  mutate(variables=a) %>% 
  select(variables, everything())


# CA
data_ca_all <- result_weight_all %>% 
  filter(grepl('ctqabuse_a', variables)) %>% 
  mutate(FDR_p=p.adjust(`Pr(>|t|)`, method='fdr'))

# PRS_MDD
data_mdd_inter_all <- result_weight_all %>% 
  filter(grepl('mdd_mean_a', variables)) 
data_mdd_all <- data_mdd_inter_all[-c(2, 4, 6, 8, 10),]
data_mdd_all$FDR_p <- p.adjust(data_mdd_all$`Pr(>|t|)`, method='fdr')

# CA-by-PRD_MDD
data_inter_all <- data_mdd_inter_all[c(2, 4, 6, 8, 10),]
data_inter_all$FDR_p <- p.adjust(data_inter_all$`Pr(>|t|)`, method='fdr')

# sex
data_sex_all <- result_weight_all %>% 
  filter(grepl('sex', variables)) %>% 
  mutate(FDR_p=p.adjust(`Pr(>|t|)`, method='fdr'))

sdq_5 <- c('semotion', 'sconduct', 'shyper','speer', 'sprosoc')
for (symp in sdq_5){
  data_symp_all <- result_weight_all %>% 
    filter(grepl(symp, variables)) %>% 
    mutate(FDR_p=p.adjust(`Pr(>|t|)`, method='fdr'))
  
  if (symp == 'semotion'){
    data_sdq_all <- data_symp_all
  }
  else {
    data_sdq_all <- rbind(data_sdq_all, data_symp_all)
  }
  
}


############## for boys ################
data_boy <- data[data$sex==1, ]
result_weight_boy <- association_weight(data_boy, index_h, index_sdq)
result_weight_boy <- result_weight_boy[-c(6, 8, 10, 12, 18, 20, 22, 24, 30, 32, 34, 36, 
                                          42, 44, 46, 48, 54, 56, 58, 60), ]
result_weight_boy <- as.data.frame(result_weight_boy)
a <- row.names(result_weight_boy)
result_weight_boy <- as_tibble(result_weight_boy)
result_weight_boy <- result_weight_boy %>% 
  mutate(variables=a) %>% 
  select(variables, everything())

# CA
data_ca_boy <- result_weight_boy %>% 
  filter(grepl('ctqabuse_a', variables)) %>% 
  mutate(FDR_p=p.adjust(`Pr(>|t|)`, method='fdr'))

# PRS_MDD
data_mdd_inter_boy <- result_weight_boy %>% 
  filter(grepl('mdd_mean_a', variables)) 
data_mdd_boy <- data_mdd_inter_boy[-c(2, 4, 6, 8, 10),]
data_mdd_boy$FDR_p <- p.adjust(data_mdd_boy$`Pr(>|t|)`, method='fdr')

# CA-by-PRD_MDD
data_inter_boy <- data_mdd_inter_boy[c(2, 4, 6, 8, 10),]
data_inter_boy$FDR_p <- p.adjust(data_inter_boy$`Pr(>|t|)`, method='fdr')

for (symp in sdq_5){
  data_symp_boy <- result_weight_boy %>% 
    filter(grepl(symp, variables)) %>% 
    mutate(FDR_p=p.adjust(`Pr(>|t|)`, method='fdr'))
  
  if (symp == 'semotion'){
    data_sdq_boy <- data_symp_boy
  }
  else {
    data_sdq_boy <- rbind(data_sdq_boy, data_symp_boy)
  }
  
}


############## for girls ################
data_girl <- data[data$sex==0, ]
result_weight_girl <- association_weight(data_girl, index_h, index_sdq)
result_weight_girl <- result_weight_girl[-c(6, 8, 10, 12, 18, 20, 22, 24, 30, 32, 34, 36, 
                                          42, 44, 46, 48, 54, 56, 58, 60), ]
result_weight_girl <- as.data.frame(result_weight_girl)
a <- row.names(result_weight_girl)
result_weight_girl <- as_tibble(result_weight_girl)
result_weight_girl <- result_weight_girl %>% 
  mutate(variables=a) %>% 
  select(variables, everything())

# CA
data_ca_girl <- result_weight_girl %>% 
  filter(grepl('ctqabuse_a', variables)) %>% 
  mutate(FDR_p=p.adjust(`Pr(>|t|)`, method='fdr'))

# PRS_MDD
data_mdd_inter_girl <- result_weight_girl %>% 
  filter(grepl('mdd_mean_a', variables)) 
data_mdd_girl <- data_mdd_inter_girl[-c(2, 4, 6, 8, 10),]
data_mdd_girl$FDR_p <- p.adjust(data_mdd_girl$`Pr(>|t|)`, method='fdr')

# CA-by-PRD_MDD
data_inter_girl <- data_mdd_inter_girl[c(2, 4, 6, 8, 10),]
data_inter_girl$FDR_p <- p.adjust(data_inter_girl$`Pr(>|t|)`, method='fdr')

for (symp in sdq_5){
  data_symp_girl <- result_weight_girl %>% 
    filter(grepl(symp, variables)) %>% 
    mutate(FDR_p=p.adjust(`Pr(>|t|)`, method='fdr'))
  
  if (symp == 'semotion'){
    data_sdq_girl <- data_symp_girl
  }
  else {
    data_sdq_girl <- rbind(data_sdq_girl, data_symp_girl)
  }
  
}


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
# index_modulate <- as.numeric(which(colnames(data) %in% c('an1_time19', 'ap1_time19', 'ap2_time19')))
# 
# for (i in index_modulate){
#   fit.lm <- lm(semotion_19 ~ data[, i] * ctqabuse * mdd_mean * sex + ses + site  + hand + bmi19 + 
#                  PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data=data)
#   
#   a <- coef(summary(fit.lm))[34,]
#   
#   if (colnames(data)[i] == "an1_time19"){
#     result_sex <- a
#   } else{
#     result_sex <- rbind(result_sex,a)
#     }
# }
# 
# 
# result_sex <- as.data.frame(result_sex)
# rownames(result_sex) <- c('an1_time19_4way', 'ap1_time19_4way', 'ap2_time19_4way')
# result_sex$FDR_p <- p.adjust(result_sex$`Pr(>|t|)`, method='fdr')
  





