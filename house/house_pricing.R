library(ggplot2)
library(dplyr)
library(lattice)
library(randomForest)
library(caret)
library
set.seed(1234)
train <- read.csv("/home/julius/Rdata/train.csv")
split_index <- sample(nrow(train),0.7*nrow(train))
train <- train[split_index,]
test <- train[-split_index,]
#test <- read.csv("/home/julius/Rdata/test.csv")

ggplot(train,aes(SalePrice))+
  geom_histogram()

ggplot(train,aes(x=Id, y=SalePrice)) +
  facet_wrap(~MSZoning) +
  geom_bar(stat="identity")

ggplot(train,aes(x=LotFrontage,y=SalePrice)) +
  geom_line()

train_lot <- filter(train, LotArea<=50000)
ggplot(train_lot,aes(x=LotArea,y=SalePrice)) +
  geom_point()

ggplot(train,aes(x=Id, y=SalePrice)) +
  facet_wrap(~Neighborhood) +
  geom_bar(stat="identity")
#neighborhood might be predicitive

ggplot(train,aes(x=Id, y=SalePrice)) +
  facet_wrap(~Condition1) +
  geom_bar(stat="identity")
#condition 1 definitely looks predictive

ggplot(train,aes(x=Id, y=SalePrice)) +
  facet_wrap(~Condition2) +
  geom_bar(stat="identity")
both_cond <- filter(train,Condition1!="Norm"& Condition2 != "Norm")
nrow(both_cond)
#only 15 obervations with both conditions not equal to "Norm", unclear if Condition2 is predictive

ggplot(train,aes(x=Id, y=SalePrice)) +
  facet_wrap(~BldgType) +
  geom_bar(stat="identity")

#random forests
rf_1 <- randomForest(SalePrice ~LotArea+Neighborhood+Condition1,train,importance=TRUE,na.rm=TRUE)
rf_1
pred_1<- predict(rf_1,test)
#root mean squared error of predictions
error_1 <- sqrt(mean((pred_1-test$SalePrice)**2))

rf_2 <- randomForest(SalePrice ~LotArea+Condition1,train,importance=TRUE,na.rm=TRUE)
rf_2
pred_2 <- predict(rf_2,test)
error_2 <- sqrt(mean((pred_2-test$SalePrice)**2))

rf_3 <- randomForest(SalePrice~LotArea+Neighborhood+Condition1+Condition2,train,importance=TRUE,na.rm=TRUE)
rf_3
pred_3 <- predict(rf_3,test)
error_3 <- sqrt(mean((pred_3-test$SalePrice)**2))
importance <- varImp(rf_3)

#!!!CAUTION!!! TIME-CONSUMING
#cross validation
set.seed(1234)
cv_fold_10 <- createMultiFolds(train,10,10)
ctrl <- trainControl(method="repeatedcv",number=10, repeats=10,index=cv_fold_10)
rf_cv_1 <- train(SalePrice ~ LotArea+Neighborhood+Condition1+OverallQual+YearBuilt+GarageCars+MSSubClass+OverallCond+X1stFlrSF+X2ndFlrSF+BedroomAbvGr+BsmtFullBath,train,method="rf",tuneLength=100,ntree=1000,trControl=ctrl)
pred_4 <- predict(rf_cv_1,test)
error_4 <- sqrt(mean((pred_4-test$SalePrice)**2))

#sdivide dataset by data structure
is.num <- sapply(train,is.factor)
num_train <- train[,!is.num]
fact_train <- train[,is.num]

#find correlation between numeric variables and SalePrice
cor(num_train,num_train$SalePrice)


#find linear relationships between numeric variables and SalePrice
lin_mod <- lm(SalePrice ~. , num_train)
summary(lin_mod)

#train random forest with these dependent variables
rf_4 <- randomForest(SalePrice ~LotArea+Neighborhood+Condition1+OverallQual+YearBuilt+GarageCars+MSSubClass+OverallCond+X1stFlrSF+X2ndFlrSF+BedroomAbvGr+BsmtFullBath,train,importance=TRUE,na.rm=TRUE)
rf_4
pred_4 <- predict(rf_4,test)
error_5 <- sqrt(mean((pred_4-test$SalePrice)**2))








#doing some nonsense to explore which features cause problems in training
rf_fact <- randomForest(SalePrice ~ .,fact_train,importance=TRUE)

fact_train <- train[,is.num]
fact_train <- select(fact_train,-Alley)
fact_train <- select(fact_train,-MasVnrType)
fact_train <- select(fact_train,-ExterQual)
fact_train <- select(fact_train,-ExterCond)
fact_train <- select(fact_train,-BsmtQual)
fact_train <- select(fact_train,-BsmtCond)
fact_train <- select(fact_train,-Foundation)
fact_train <- select(fact_train,-BsmtExposure)
fact_train <- fact_train[,1:12]
fact_train <- mutate(fact_train,SalePrice=train$SalePrice)

f <- function(x){
  rf_fact <- randomForest(as.formula(paste("SalePrice ~",paste(names(x),collapse="+"))),x,importance=TRUE)
  return(rf_fact)
}


