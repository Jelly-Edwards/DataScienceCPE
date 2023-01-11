install.packages("corrplot")
install.packages("e1071")
library(corrplot)
library(caret)
library(readr)

existProd <- read.csv("existingproductattributes2017.csv")
newProd <- read.csv("newproductattributes2017.csv")


attributes(existProd)
str(existProd)

## The ProductType attribute is a character data type. Need to convert to binary for processing
## Using caret library to dummify the data

DFExistProd <- dummyVars(" ~ .", data = existProd)

convExistProd <- data.frame(predict(DFExistProd, newdata = existProd))

str(convExistProd)
## create new columns for each categorical value in previous ProductType column
## Converted everything to Num data type

summary(convExistProd)
## Checking for NA or missing data
## BestSellersRank has NA's, dropping this attribute due to missing data

convExistProd$BestSellersRank <- NULL

corrData <- cor(convExistProd)
corrData
## Correlation matrix

corrplot(corrData)
## Strongest correlation for Volume is a positive correlation with x5StarReviews
## then followed by 4StarReviews and so on and so forth

convExistProd$ProductNum <- NULL
## Removing Product Number since this isn't needed for model since it is essentially product name.
convExistProd$ProfitMargin <- NULL


## Linear Model
set.seed(123)

trainSize <- round(nrow(convExistProd)*0.7)
testSize <- nrow(convExistProd)-trainSize

training_indices <- sample(seq_len(nrow(convExistProd)), size = trainSize)
trainSet <- convExistProd[training_indices,]
testSet <- convExistProd[-training_indices,]

linModel <- lm(Volume~ x5StarReviews + x4StarReviews + x3StarReviews + PositiveServiceReview, trainSet)

summary(linModel)
## Resulted with a Perfect R^2, which seems incorrect. RMSE is very small. It's overfitting

## Retrying w/ Caret
inTraining <- createDataPartition(convExistProd$Volume, p = 0.75, list = FALSE)
training <- convExistProd[inTraining,]
testing <- convExistProd[-inTraining,]

fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)

LMFit1 <- train(Volume~., data = training, method = "lm", trControl = fitControl)

LMFit1
## Trying Var Importance 

varImp(LMFit1)

featSelect <- training[, c("Price", "x5StarReviews","x4StarReviews","x3StarReviews","PositiveServiceReview", "Volume")]

LMFit2 <- train(Volume~., data = featSelect, method = "lm", trControl = fitControl)

LMFit2
## Resulted with a Perfect R^2, which seems incorrect

## Diving into non-parametric machine learning models
## Use models on  New Products Dataset

SVMFit1 <- train(Volume~., data = featSelect, method = "svmLinear2", trControl = fitControl)
SVMFit1

RMFit1 <- train(Volume~., data = featSelect, method = "rf", trControl = fitControl)
RMFit1

RMFit2 <- train(Volume~., data = featSelect, method = "rf", trControl = fitControl, tuneLength = 3)
RMFit2

GBMFit1 <- train(Volume~., data = training, method = "gbm", trControl = fitControl)

## Best Model was SVM. GBM fails due to small data set size.

## Predictions

SVMPredict <- predict(SVMFit1, testing)
SVMPredict

postResample(SVMPredict, testing$Volume)
## RMSE as ~71, R^2 is around ~0.99. Very close to perfect fit, I'm concerned for overfit

RMPredict <- predict(RMFit1, testing)
RMPredict

postResample(RMPredict, testing$Volume)
## RMSE as ~891, R^2 is around ~0.70. Very high R^2 so it is successful, but RMSE value is very high. 

write.csv(SVMPredict, "C:\\Users\\jelly\\Documents\\DataAnalytics\\SVMPredict.csv", row.names=FALSE)

write.csv(RMPredict, "C:\\Users\\jelly\\Documents\\DataAnalytics\\RMPredict.csv", row.names=FALSE)

## The best model is the Random Forest, because SVM might be over fitting as it keeps getting really high and almost
## perfect RMSE and R^2 values. Also, the model provided negative values for the predict Volumes, which is not possible
## since it isn't possible to have LOSE product during sales. RF does not do this.

attributes(newProd)
str(newProd)

DFNewProd <- dummyVars(" ~ .", data = newProd)

convNewProd <- data.frame(predict(DFNewProd, newdata = newProd))

str(convNewProd)

convNewProd$BestSellersRank <- NULL
convNewProd$ProductNum <- NULL
convNewProd$ProfitMargin <- NULL

finalPred <- predict(RMFit2, convNewProd)
finalPred

output <- newProd
output$Predictions <- finalPred

write.csv(output, file="C2.t3output.csv", row.names = TRUE)
