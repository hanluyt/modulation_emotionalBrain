require(tidyverse)
require(dplyr)
require(lmerTest)
require(rstatix)

# load data
data <- read_csv('data_IMAGEN_ctqcutoff.csv') %>%
  convert_as_factor(id, sex, hand, site)

data_girl <- data[data$sex == 0, ]
data_girl <- data_girl[, -c(12:21, 37:40)]

# pivoting
data_girl <- data_girl %>%
  pivot_longer(cols=starts_with("a"),
               names_to=c('.value', 'time'),
               names_sep='_',
               values_drop_na=TRUE) %>%
  mutate(time=str_remove(time, "time") %>% as.double()) 

################## Repeated measures analyses of variance ##################
res.aov <- aov(data=data_girl, an1 ~ time + Error(id/time) + site + puberty + ses + hand + bmi14 +
                 PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + mdd_mean + ctqabuse)
summary.anova <- summary(res.aov)


################## Visualization ##################
param <- list(
  y.label    = "Weights of neutral activated factor 1",
  Fc.label  = "Age",
  cat.color  = "#A2A0C3",
  errorbar   = "sd"  # can be either sd, se, or ci
)

# main effect
res.time.pval <- summary.anova[["Error: id:time"]][[1]][["Pr(>F)"]][1]
# sign.time <-ifelse(res.time.pval < 0.05, "Significant", "No significant")

library(gtools)
p.time <- ifelse(res.time.pval < 0.05, stars.pval(res.time.pval), "n.s.")

Fc.title <- sprintf("%s %s", param$Fc.label, p.time)


stat_girl <- data_girl %>%
  group_by(time) %>%
  get_summary_stats(an1, type = "mean_sd")

# mean developmental trajectory
mean_data <- data.frame(x=stat_girl$time,  y =stat_girl$mean)
mean_data$x <- as.factor(mean_data$x)
mean_data$id <- 'id'
mean_data$id <- as.factor(mean_data$id)

# plot
data_girl$time <- as.factor(data_girl$time)
pd <- position_dodge(width=0.35)
ggplot() + 
  geom_point(data=data_girl, aes(x=time, y=an1, group=id), color=param$cat.color,alpha = .55, 
             size = 3.3, position=pd) +
  geom_line(data=data_girl, aes(x=time, y=an1, group=id), linewidth=0.38, 
            color=param$cat.color,alpha=0.45, position=pd) +
  geom_point(data=mean_data, aes(x, y), color = '#7C7C7C', size=8) +
  geom_line(data=mean_data, aes(x, y, group=id),  color = '#7C7C7C', size=5.5) +
  scale_x_discrete(name=Fc.title) +
  scale_y_continuous(name=param$y.label, limits=c(0, 40), breaks=seq(0, 40, 5)) + theme_bw() +
  theme(text=element_text(family="Arial", size=18),panel.border = element_blank(),
        axis.text = element_text(size=18),
        legend.text=element_text(size=18), plot.title=element_text(hjust=0.5))
