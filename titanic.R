#package for data wrangling
library(dplyr)
#package for visualization
library(ggplot2)
library(randomForest)
set.seed(678)
path <- 'https://raw.githubusercontent.com/thomaspernet/data_csv_r/master/data/titanic_csv.csv'
#load data into R
titanic <-read.csv(path) %>%
  select(-name)


#totalsex
ggplot(titanic, aes(sex)) +
  geom_bar()

#survived count
ggplot(titanic, aes(survived)) +
  geom_bar()

#males survived count
males <- filter(titanic, sex == "male")
ggplot(males, aes(survived)) +
  geom_bar()

#females survived count
females <- filter(titanic, sex == "female")
ggplot(females, aes(survived)) +
  geom_bar()

#count survived vs. age for each class and sex
ggplot(titanic,aes(x = age, fill = factor(survived))) +
  facet_wrap(~sex + pclass) +
  geom_histogram(binwidth = 10)

#count fare vs survived
ggplot(titanic,aes(x = fare, fill = factor(survived))) +
  geom_histogram(binwidth = 10)

#extract first cabin character
titanic$cabin[1:100]
titanic$cabin <- as.character(titanic$cabin)
titanic[which(titanic$cabin == ""),"cabin"] <- "U"
first_cabin_char <- as.factor(substr(titanic$cabin,1,1))
titanic$first_cabin_char <- first_cabin_char

ggplot(titanic, aes(x=first_cabin_char,fill=factor(survived))) +
  geom_bar()


shuffle_index <- sample(nrow(titanic), nrow(titanic)*.7)
#split data into training and test data
train <- titanic[shuffle_index, ]
test <- titanic[-shuffle_index, ]
#gender seems to be predictive for survival, so
#train a logistic regression model on the training data to predict survived subject to gender


logistic_model_1 = glm(survived ~ sex ,
                             family = binomial(logit), data = train)
summary(logistic_model_1)
prob_1 <- predict(logistic_model_1,test)
pred_1 <- ifelse(prob_1>0.5,1,0)
table(pred_1,test$survived)
mean(pred_1==test$survived)


logistic_model_2 = glm(survived ~ fare,
                       family = binomial(logit), data = train)
summary(logistic_model_2)
prob_2 <- predict(logistic_model_2,test)
pred_2 <- ifelse(prob_2>0.5,1,0)
table(pred_2,test$survived)
mean(pred_2==test$survived,na.rm=TRUE)


logistic_model_3 = glm(survived ~sex + fare,
                       family = binomial(logit), data = train)
summary(logistic_model_3)
prob_3 <- predict(logistic_model_3,test)
pred_3 <- ifelse(prob_3>0.5,1,0)
table(pred_3,test$survived)
mean(pred_3==test$survived,na.rm=TRUE)


logistic_model_4 = glm(survived ~sex + fare+pclass,
                       family = binomial(logit), data = train)
summary(logistic_model_4)
prob_4 <- predict(logistic_model_4,test)
pred_4 <- ifelse(prob_4>0.5,1,0)
table(pred_4,test$survived)
mean(pred_4==test$survived,na.rm=TRUE)



#random forest models
rf_model_1 = randomForest(survived ~ sex, data=train, importance=TRUE)
rf_prob_1 <- predict(rf_model_1,test)
rf_pred_1 <- ifelse(rf_prob_1>0.5,1,0)
table(rf_pred_1,test$survived)
mean(rf_pred_1==test$survived)


rf_model_3 = randomForest(survived ~ sex+fare, data=train, importance=TRUE)
rf_prob_3 <- predict(rf_model_3,test)
rf_pred_3 <- ifelse(rf_prob_3>0.5,1,0)
table(rf_pred_3,test$survived)
mean(rf_pred_3==test$survived,na.rm=TRUE)


rf_model_4 = randomForest(survived ~ sex+fare+pclass, data=train, importance=TRUE)
rf_prob_4 <- predict(rf_model_4,test)
rf_pred_4 <- ifelse(rf_prob_4>0.5,1,0)
table(rf_pred_4,test$survived)
mean(rf_pred_4==test$survived,na.rm=TRUE)


