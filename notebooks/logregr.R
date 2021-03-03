setwd("C:/Users/Karol/Desktop/R")
df <- read.csv('uni.csv', header = T)
head(df)
summary(df)

# Dealing with NAs
df <- na.omit(df)

# Encode as a factor
df$admit <- as.factor(df$admit)
df$rank <- as.factor(df$rank)

# Table describing the average number of points in the matriculation examination,
# depending on whether a given person was admitted to studies and ranking.

library(dplyr)
df %>%
  group_by(admit, rank) %>%
  summarise(mean = mean(gre))

# Regression models with hypotheses

#H0 : Exam results do not affect admission to studies
#H1 : Exam results affect admission to studies

model1 <- glm(admit~gre, family="binomial", data=df)
summary(model1)

# P-value < 0.05, assume H1, Exam results affect admission to studies 

#H0: Number of points does not affect admission to studies
#H1: Number of points affect admission to studies

model2 <- glm(admit~gpa, family="binomial", data=df)
summary(model2)

# P-value < 0.05, assume H1, The number of points affect admission to studies

#H0: Ranking place does not affect admission to studies
#H1: Ranking place affect admission to studies

model3 <- glm(admit~rank, family="binomial", data=df)
summary(model3)

# P-value < 0.05, assume H1, Ranking place affect admission to studies

#H0: Exam results, number of points, ranking place do not affect admission to studies
#H1: Exam results, number of points, ranking place affect admission to studies

model4 <- glm(admit~gre+gpa+rank, family="binomial", data=df)
summary(model4)

# P-value < 0.05, assume H1.
# Exam results, number of points, ranking place affect admission to studies

# Model 4 is the best fit because for it the AIC value is the lowest

# Graph generated on the basis of the model 4

library(ggplot2)

predicted.data<-data.frame(
  probability.of.acceptance=model4$fitted.values,
  admit=df$admit)

predicted.data<-predicted.data[
  order(predicted.data$probability.of.acceptance, decreasing=FALSE),]

predicted.data$rank<-1:nrow(predicted.data)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.acceptance))+
  geom_point(aes(color=admit), alpha=1, shape=4, stroke=2)+
  xlab('Index')+
  ylab('Predicted probability of accepted students')