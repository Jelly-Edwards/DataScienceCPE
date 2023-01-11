library(readr)
library(tidyr)
library(tidyverse)
library(dplyr)
library(caret)
library(SmartEDA)
library(gbm)
library(C50)
library(doParallel)

## Find how many cores are on my machine
detectCores()
## 12 Cores

## Create Cluster w/ desired number of cores. Don't use them all! 
cl <- makeCluster(6)

## Register Cluster
registerDoParallel(cl)

## Confirm how many cores are now "assigned" to R and Rstudio
getDoParWorkers()
## 6

trainDF <- read.csv("trainingData.csv")

summary(trainDF)
attributes(trainDF)
str(trainDF)

## Data is too large and needs to be sampled to ease the prediction models
## Will split by Building

build0 <- trainDF[trainDF$BUILDINGID == "0",]
str(build0)
head(build0)
tail(build0)
count(build0)
## 5249 Rows

build1 <- trainDF[trainDF$BUILDINGID == "1",]
str(build1)
head(build1)
tail(build1)
count(build1)
## 5196 Rows

build2 <- trainDF[trainDF$BUILDINGID == "2",]
str(build2)
head(build2)
tail(build2)
count(build2)
## 9492 Rows

## In case this is needed in the future, I am also creating a randomized sample
rndSample <- trainDF[sample(1:nrow(trainDF), 5000, replace = FALSE),]
str(rndSample)
head(rndSample)
tail(rndSample)
count(rndSample)
## 5000 observations randomly selected from original DF

## We Need to combine Floor, BuildingID, SpaceID, and RelativePosition into one combined feature
## This feature will be a factor data type

build0DF <- cbind(build0, paste(build0$FLOOR,"-",build0$BUILDINGID,"-",build0$SPACEID,"-",build0$RELATIVEPOSITION), stringsAsFactors=FALSE) 

str(build0DF)
attributes(build0DF)

## Rename the new column
colnames(build0DF)[530] <- "USERLOCATION"

## Move new USERLOCATION column to front
build0DF <- build0DF %>% relocate(USERLOCATION, .before = USERID)
str(build0DF)
attributes(build0DF)

## Convert USERLOCATION, FLOOR, BUILDINGID, SPACEID, RELATIVEPOSITION, USERID, PHONEID to Factor
build0DF$USERLOCATION <- as.factor(build0DF$USERLOCATION)
build0DF$FLOOR <- as.factor(build0DF$FLOOR)
build0DF$BUILDINGID <- as.factor(build0DF$BUILDINGID)
build0DF$SPACEID <- as.factor(build0DF$SPACEID)
build0DF$RELATIVEPOSITION <- as.factor(build0DF$RELATIVEPOSITION)
build0DF$USERID <- as.factor(build0DF$USERID)
build0DF$PHONEID <- as.factor(build0DF$PHONEID)
str(build0DF)

## Check for missing values
summary(build0DF)
## No Missing Values

##Remove the extra location columns since we have USER LOCATION.
## We want to predict location via WAP
build0DF <- build0DF[,!names(build0DF) %in% c("FLOOR", "BUILDINGID", "SPACEID", "RELATIVEPOSITION", "LONGITUDE", "LATITUDE")]
str(build0DF)

## Building 1
build1DF <- cbind(build1, paste(build1$FLOOR,"-",build1$BUILDINGID,"-",build1$SPACEID,"-",build1$RELATIVEPOSITION), stringsAsFactors=FALSE) 

str(build1DF)
attributes(build1DF)

## Rename the new column
colnames(build1DF)[530] <- "USERLOCATION"

## Move new USERLOCATION column to front
build1DF <- build1DF %>% relocate(USERLOCATION, .before = USERID)
str(build1DF)
attributes(build1DF)

## Convert USERLOCATION, FLOOR, BUILDINGID, SPACEID, RELATIVEPOSITION, USERID, PHONEID to Factor
build1DF$USERLOCATION <- as.factor(build1DF$USERLOCATION)
build1DF$FLOOR <- as.factor(build1DF$FLOOR)
build1DF$BUILDINGID <- as.factor(build1DF$BUILDINGID)
build1DF$SPACEID <- as.factor(build1DF$SPACEID)
build1DF$RELATIVEPOSITION <- as.factor(build1DF$RELATIVEPOSITION)
build1DF$USERID <- as.factor(build1DF$USERID)
build1DF$PHONEID <- as.factor(build1DF$PHONEID)
str(build1DF)

## Check for missing values
summary(build1DF)
## No Missing Values

##Remove the extra location columns since we have USER LOCATION.
## We want to predict location via WAP
build1DF <- build1DF[,!names(build1DF) %in% c("FLOOR", "BUILDINGID", "SPACEID", "RELATIVEPOSITION", "LONGITUDE", "LATITUDE")]
str(build1DF)

## Building 2
build2DF <- cbind(build2, paste(build2$FLOOR,"-",build2$BUILDINGID,"-",build2$SPACEID,"-",build2$RELATIVEPOSITION), stringsAsFactors=FALSE) 

str(build2DF)
attributes(build2DF)

## Rename the new column
colnames(build2DF)[530] <- "USERLOCATION"

## Move new USERLOCATION column to front
build2DF <- build2DF %>% relocate(USERLOCATION, .before = USERID)
str(build2DF)
attributes(build2DF)

## Convert USERLOCATION, FLOOR, BUILDINGID, SPACEID, RELATIVEPOSITION, USERID, PHONEID to Factor
build2DF$USERLOCATION <- as.factor(build2DF$USERLOCATION)
build2DF$FLOOR <- as.factor(build2DF$FLOOR)
build2DF$BUILDINGID <- as.factor(build2DF$BUILDINGID)
build2DF$SPACEID <- as.factor(build2DF$SPACEID)
build2DF$RELATIVEPOSITION <- as.factor(build2DF$RELATIVEPOSITION)
build2DF$USERID <- as.factor(build2DF$USERID)
build2DF$PHONEID <- as.factor(build2DF$PHONEID)
str(build2DF)

## Check for missing values
summary(build2DF)
## No Missing Values

##Remove the extra location columns since we have USER LOCATION.
## We want to predict location via WAP
build2DF <- build2DF[,!names(build2DF) %in% c("FLOOR", "BUILDINGID", "SPACEID", "RELATIVEPOSITION", "LONGITUDE", "LATITUDE")]
str(build2DF)

## Repeat w/ RandomSample
rndSampleDF <- cbind(rndSample, paste(rndSample$FLOOR,"-",rndSample$BUILDINGID,"-",rndSample$SPACEID,"-",rndSample$RELATIVEPOSITION), stringsAsFactors=FALSE) 

str(rndSampleDF)
attributes(rndSampleDF)

## Rename the new column
colnames(rndSampleDF)[530] <- "USERLOCATION"

## Move new USERLOCATION column to front
rndSampleDF <- rndSampleDF %>% relocate(USERLOCATION, .before = USERID)
str(rndSampleDF)
attributes(rndSampleDF)

## Convert USERLOCATION, FLOOR, BUILDINGID, SPACEID, RELATIVEPOSITION, USERID, PHONEID to Factor
rndSampleDF$USERLOCATION <- as.factor(rndSampleDF$USERLOCATION)
rndSampleDF$FLOOR <- as.factor(rndSampleDF$FLOOR)
rndSampleDF$BUILDINGID <- as.factor(rndSampleDF$BUILDINGID)
rndSampleDF$SPACEID <- as.factor(rndSampleDF$SPACEID)
rndSampleDF$RELATIVEPOSITION <- as.factor(rndSampleDF$RELATIVEPOSITION)
rndSampleDF$USERID <- as.factor(rndSampleDF$USERID)
rndSampleDF$PHONEID <- as.factor(rndSampleDF$PHONEID)
str(rndSampleDF)

## Check for missing values
summary(rndSampleDF)
## No Missing Values

##Remove the extra location columns since we have USER LOCATION.
## We want to predict location via WAP
rndSampleDF <- rndSampleDF[,!names(rndSampleDF) %in% c("FLOOR", "BUILDINGID", "SPACEID", "RELATIVEPOSITION", "LONGITUDE", "LATITUDE")]
str(rndSampleDF)

## EDA Report
ExpReport(
  build0DF,
  Target = "USERLOCATION",
  label=NULL,
  op_file = "Report.html",
  op_dir = getwd(),
  Rc = "Yes",
)
## EDA report fails to create.
## Quitting from lines 124-125 (report_tmp_2.Rmd) 
## Error in ExpNumViz(data, target = Target, Page = c(2, 1), theme = theme,  : 
## If scatter option is TRUE then 'target should be categorical' else 'change scatter = FALSE' 

## Set Seed
set.seed(998)

## Define 75%/25% train/test split for Build0DF
inTraining <- createDataPartition(build0DF$USERLOCATION, p = 0.75, list = FALSE)
training <- build0DF[inTraining,]
testing <- build0DF[-inTraining,]
nrow(training)
nrow(testing)

#10 fold cross validation
fitControl <- trainControl(method ="repeatedcv", number =10, repeats = 1)

## Chose a classification method since I have made the USERLOCATION variable a factor
## Location is more of a label/category rather than a continuous value like income
RMFit1 <- train(USERLOCATION~., data = training, method = "rf", trControl = fitControl)
RMFit1
## Mtry =523, with accuracy of 0.85 and a Kappa of 0.85

varImp(RMFit1)
## WAP051, WAP052 highest importance, followed b WAP155 and WAP156

## C5.0 is another classification method
C50Fit1 <- train(USERLOCATION~., data = training, method = "C5.0", trControl=fitControl)
C50Fit1

## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were trials = 20, model = rules and winnow = TRUE.
## Accuracy 0.79 and Kappa at 0.79

## KNN is another classification method
KNNFit1 <- train(USERLOCATION~., data = training, method = "knn", trControl=fitControl)
KNNFit1
## Accuracy at 0.65 and Kappa at 0.65

## Define 75%/25% train/test split for rndSampleDF
inTraining <- createDataPartition(rndSampleDF$USERLOCATION, p = 0.75, list = FALSE)
training <- rndSampleDF[inTraining,]
testing <- rndSampleDF[-inTraining,]
nrow(training)
nrow(testing)

#10 fold cross validation
fitControl <- trainControl(method ="repeatedcv", number =10, repeats = 1)

## Chose a classification method since I have made the USERLOCATION variable a factor
## Location is more of a label/category rather than a continuous value like income
RMFit2 <- train(USERLOCATION~., data = training, method = "rf", trControl = fitControl)
RMFit2

varImp(RMFit2)

## C5.0 is another classification method
C50Fit2 <- train(USERLOCATION~., data = training, method = "C5.0", trControl=fitControl)
C50Fit2

KNNFit2 <- train(USERLOCATION~., data = training, method = "knn", trControl=fitControl)
KNNFit2

## Doesn't work for randomSample because the randomized Factored Location would have some records w/ a single record

## Define 75%/25% train/test split for Build1DF
inTraining <- createDataPartition(build1DF$USERLOCATION, p = 0.75, list = FALSE)
training <- build1DF[inTraining,]
testing <- build1DF[-inTraining,]
nrow(training)
nrow(testing)

#10 fold cross validation
fitControl <- trainControl(method ="repeatedcv", number =10, repeats = 1)

## Chose a classification method since I have made the USERLOCATION variable a factor
## Location is more of a label/category rather than a continuous value like income
RMFit3 <- train(USERLOCATION~., data = training, method = "rf", trControl = fitControl)
RMFit3
## Mtry =541, with accuracy of 0.95 and a Kappa of 0.95

varImp(RMFit3)
## WAP008, WAP108 highest importance, followed b WAP114 and WAP222

## C5.0 is another classification method
C50Fit3 <- train(USERLOCATION~., data = training, method = "C5.0", trControl=fitControl)
C50Fit3

## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were trials = 20, model = rules and winnow = TRUE.
## Accuracy 0.88 and Kappa at 0.88

## KNN is another classification method
KNNFit3 <- train(USERLOCATION~., data = training, method = "knn", trControl=fitControl)
KNNFit3
## Accuracy at 0.80 and Kappa at 0.80

## Define 75%/25% train/test split for Build2DF
inTraining <- createDataPartition(build2DF$USERLOCATION, p = 0.75, list = FALSE)
training <- build2DF[inTraining,]
testing <- build2DF[-inTraining,]
nrow(training)
nrow(testing)

#10 fold cross validation
fitControl <- trainControl(method ="repeatedcv", number =10, repeats = 1)

## Chose a classification method since I have made the USERLOCATION variable a factor
## Location is more of a label/category rather than a continuous value like income
RMFit4 <- train(USERLOCATION~., data = training, method = "rf", trControl = fitControl)
RMFit4
## Mtry =549, with accuracy of 0.88 and a Kappa of 0.88

varImp(RMFit4)
## WAP496, WAP012 highest importance, followed b WAP501 and WAP118

## C5.0 is another classification method
C50Fit4 <- train(USERLOCATION~., data = training, method = "C5.0", trControl=fitControl)
C50Fit4

## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were trials = 20, model = rules and winnow = TRUE.
## Accuracy 0.80 and Kappa at 0.80

## KNN is another classification method
KNNFit4 <- train(USERLOCATION~., data = training, method = "knn", trControl=fitControl)
KNNFit4
## Accuracy at 0.76 and Kappa at 0.76





## Prediction with TESTING Data. The cross validation tests showed RF was the best model in accuracy/Kappa
inTraining <- createDataPartition(build0DF$USERLOCATION, p = 0.75, list = FALSE)
training <- build0DF[inTraining,]
testing <- build0DF[-inTraining,]
nrow(training)
nrow(testing)

rfPredictTesting <- predict(RMFit1, testing)
rfPredictTesting

postResample(rfPredictTesting, testing$USERLOCATION)
#Acc at 97%, Kappa at 97%

confusionMatrix(rfPredictTesting, testing$USERLOCATION)
#Accuracy at 97%, Kappa at 97%

C50PredictTesting <- predict(C50Fit1, testing)
C50PredictTesting

postResample(C50PredictTesting, testing$USERLOCATION)
#Acc at 96%, Kappa at 96%

confusionMatrix(C50PredictTesting, testing$USERLOCATION)
#Accuracy at 96%, Kappa at 96%

KNNPredictTesting <- predict(KNNFit1, testing)
KNNPredictTesting

postResample(KNNPredictTesting, testing$USERLOCATION)
#Acc at 74%, Kappa at 74%
confusionMatrix(KNNPredictTesting, testing$USERLOCATION)
#Accuracy at 75%, Kappa at 75%

## Prediction with TESTING Data. The cross validation tests showed RF was the best model in accuracy/Kappa
inTraining <- createDataPartition(build1DF$USERLOCATION, p = 0.75, list = FALSE)
training <- build1DF[inTraining,]
testing <- build1DF[-inTraining,]
nrow(training)
nrow(testing)

rfPredictTesting2 <- predict(RMFit3, testing)
rfPredictTesting2

postResample(rfPredictTesting2, testing$USERLOCATION)
#Acc at 99%, Kappa at 99%

C50PredictTesting2 <- predict(C50Fit3, testing)
C50PredictTesting2

postResample(C50PredictTesting2, testing$USERLOCATION)
#Acc at 96%, Kappa at 96%

KNNPredictTesting2 <- predict(KNNFit3, testing)
KNNPredictTesting2

postResample(KNNPredictTesting2, testing$USERLOCATION)
#Acc at 87%, Kappa at 87%

## Prediction with TESTING Data. The cross validation tests showed RF was the best model in accuracy/Kappa
inTraining <- createDataPartition(build2DF$USERLOCATION, p = 0.75, list = FALSE)
training <- build2DF[inTraining,]
testing <- build2DF[-inTraining,]
nrow(training)
nrow(testing)

rfPredictTesting3 <- predict(RMFit4, testing)
rfPredictTesting3

postResample(rfPredictTesting3, testing$USERLOCATION)
#Acc at 98%, Kappa at 98%

C50PredictTesting3 <- predict(C50Fit4, testing)
C50PredictTesting3

postResample(C50PredictTesting3, testing$USERLOCATION)
#Acc at 93%, Kappa at 93%

KNNPredictTesting3 <- predict(KNNFit4, testing)
KNNPredictTesting3

postResample(KNNPredictTesting3, testing$USERLOCATION)
#Acc at 85%, Kappa at 85%

ModelData0 <- resamples(list(RF = RMFit1, C50 = C50Fit1, KNN = KNNFit1))
ModelData0
summary(ModelData0)

ModelData1 <- resamples(list(RF = RMFit3, C50 = C50Fit3, KNN = KNNFit3))
ModelData1
summary(ModelData1)

ModelData2 <- resamples(list(RF = RMFit4, C50 = C50Fit4, KNN = KNNFit4))
ModelData2
summary(ModelData2)

write.csv(rfPredictTesting, "C:\\Users\\jelly\\Documents\\DataAnalytics\\rfPredictTesting0.csv", row.names=FALSE)
write.csv(C50PredictTesting, "C:\\Users\\jelly\\Documents\\DataAnalytics\\C50PredictTesting0.csv", row.names=FALSE)
write.csv(KNNPredictTesting, "C:\\Users\\jelly\\Documents\\DataAnalytics\\KNNPredictTesting0.csv", row.names=FALSE)

write.csv(rfPredictTesting2, "C:\\Users\\jelly\\Documents\\DataAnalytics\\rfPredictTesting1.csv", row.names=FALSE)
write.csv(C50PredictTesting2, "C:\\Users\\jelly\\Documents\\DataAnalytics\\C50PredictTesting1.csv", row.names=FALSE)
write.csv(KNNPredictTesting2, "C:\\Users\\jelly\\Documents\\DataAnalytics\\KNNPredictTesting1.csv", row.names=FALSE)

write.csv(rfPredictTesting3, "C:\\Users\\jelly\\Documents\\DataAnalytics\\rfPredictTesting2.csv", row.names=FALSE)
write.csv(C50PredictTesting3, "C:\\Users\\jelly\\Documents\\DataAnalytics\\C50PredictTesting2.csv", row.names=FALSE)
write.csv(KNNPredictTesting3, "C:\\Users\\jelly\\Documents\\DataAnalytics\\KNNPredictTesting2.csv", row.names=FALSE)
