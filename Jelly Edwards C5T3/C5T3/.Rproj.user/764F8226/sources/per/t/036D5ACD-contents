library(doParallel)
library(readr)
library(plotly)
library(corrplot)
library(caret)
library(e1071)
library(kknn)

options(max.print=1000000)

# Find how many cores are on your machine
detectCores() # Result = Typically 4 to 6

# Create Cluster with desired number of cores. Don't use them all! Your computer is running other processes. 
cl <- makeCluster(6)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2 

## 0: Sentiment Unclear
## 1: very negative
## 2: somewhat negative
## 3: neutral
## 4: somewhat positive
## 5: very positive

iphoneSM <- read.csv("iphone_smallmatrix_labeled_8d.csv")

str(iphoneSM)
str(galaxySM)

summary(iphoneSM)
is.na(iphoneSM)
## No missing data

plot_ly(iphoneSM, x = ~iphoneSM$iphonesentiment, type='histogram')




## Correlation
corrData <- cor(iphoneSM)
corrData
corrplot(corrData)
## The correlations for iphonesentiment are very weak for all features - needs more trimming via feature variance

nzvMetrics <- nearZeroVar(iphoneSM, saveMetrics = TRUE)
nzvMetrics

nzv <- nearZeroVar(iphoneSM, saveMetrics = FALSE) 
nzv

# create a new data set and remove near zero variance features
iphoneNZV <- iphoneSM[,-nzv]
str(iphoneNZV)

corrData2 <- cor(iphoneNZV)
corrData2
corrplot(corrData2)
## largest Correlation between IphoneSentiment and SamsungGalaxy. It's negative, but still pretty weak

## Let's sample the data before using RFE
set.seed(123)

iphoneSample <- iphoneSM[sample(1:nrow(iphoneSM), 1000, replace=FALSE),]

## Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 5, verbose = FALSE)

## Use rfe and omit the response variable (attribute 59 iphonesentiment)
rfeResults <- rfe(iphoneSample[,1:58], iphoneSample$iphonesentiment, sizes=(1:58), rfeControl=ctrl)

rfeResults

plot(rfeResults, type=c("g", "o"))

## create new data set with rfe recommended reatures
iphoneRFE <- iphoneSM[,predictors(rfeResults)]

## add the dependent variable to iphoneRFE
iphoneRFE$iphonesentiment <- iphoneSM$iphonesentiment

## review outcome
str(iphoneRFE)

## I think iphonesentiment needs to be a factor - it is a categorical value
iphoneSM$iphonesentiment <- as.factor(iphoneSM$iphonesentiment)
iphoneRFE$iphonesentiment <- as.factor(iphoneRFE$iphonesentiment)
iphoneNZV$iphonesentiment <- as.factor(iphoneNZV$iphonesentiment)

str(iphoneSM)
str(iphoneRFE)
str(iphoneNZV)
## I like the iphoneNZV the most, with iphoneRFE second




## Modeling w/ All Features first for OotB Accuracy and Kappa
set.seed(998)

#define 75%/25% train/test split
inTraining <- createDataPartition(iphoneSM$iphonesentiment, p = 0.70, list = FALSE)
training <- iphoneSM[inTraining,]
testing <- iphoneSM[-inTraining,]
nrow(training)
nrow(testing)

#10 fold cross val
fitControl <- trainControl(method ="repeatedcv", number =10, repeats = 1)

## C5.0 is a classification method
C50Fit1 <- train(iphonesentiment~., data = training, method = "C5.0", trControl=fitControl)
C50Fit1
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were trials = 1, model = rules and winnow = TRUE.
## Accuracy: 0.77, Kappa: 0.55

## SVM
SVMFit1 <- train(iphonesentiment~., data = training, method = "svmLinear2", trControl=fitControl)
SVMFit1
## Accuracy: 0.70, Kappa: 0.40

## kknn
kknnFit1 <- train(iphonesentiment~., data = training, method = "kknn", trControl=fitControl)
kknnFit1
## Accuracy: 0.32, Kappa: 0.15

## RM
RMFit1 <- train(iphonesentiment~., data = training, method = "rf", trControl = fitControl)
RMFit1
## Accuracy: 0.76, Kappa: 0.54

## C50 and RMFit preformed in a similar manner, and were both the best performing

C50PredictTest1 <- predict(C50Fit1, testing)
C50PredictTest1

postResample(C50PredictTest1, testing$iphonesentiment)
confusionMatrix(C50PredictTest1, testing$iphonesentiment)
#Acc at 78%, Kappa at 57%
#Acc at 47%, Kappa at 0.3%

SVMPredictTest1 <- predict(SVMFit1, testing)
SVMPredictTest1

postResample(SVMPredictTest1, testing$iphonesentiment)
confusionMatrix(SVMPredictTest1, testing$iphonesentiment)
#Acc at 70%, Kappa at 41%
#Acc at 51%, Kappa at 1%

kknnPredictTest1 <- predict(kknnFit1, testing)
kknnPredictTest1

postResample(kknnPredictTest1, testing$iphonesentiment)
confusionMatrix(kknnPredictTest1, testing$iphonesentiment)
#Acc at 34%, Kappa at 18%
#Acc at 20%, Kappa at -0.1%

RMPredictTest1 <- predict(RMFit1, testing)
RMPredictTest1

postResample(RMPredictTest1, testing$iphonesentiment)
confusionMatrix(RMPredictTest1, testing$iphonesentiment)
#Acc at 78%, Kappa at 57%
#Acc at 48%, Kappa at < 0%




## Now using data sets that were from feature selection
set.seed(998)

#define 75%/25% train/test split
inTraining <- createDataPartition(iphoneNZV$iphonesentiment, p = 0.70, list = FALSE)
training <- iphoneNZV[inTraining,]
testing <- iphoneNZV[-inTraining,]
nrow(training)
nrow(testing)

#10 fold cross val
fitControl <- trainControl(method ="repeatedcv", number =10, repeats = 1)

## C5.0 is a classification method
C50Fit2 <- train(iphonesentiment~., data = training, method = "C5.0", trControl=fitControl)
C50Fit2
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were trials = 1, model = tree and winnow = FALSE
## Accuracy: 0.76, Kappa: 0.52

## RM
RMFit2 <- train(iphonesentiment~., data = training, method = "rf", trControl = fitControl)
RMFit2
## Accuracy: 0.76, Kappa: 0.53

C50PredictTest2 <- predict(C50Fit2, testing)
C50PredictTest2

postResample(C50PredictTest2, testing$iphonesentiment)
confusionMatrix(C50PredictTest2, testing$iphonesentiment)
#Acc at 76%, Kappa at 52%
#Acc at 76%, Kappa at 52%

RMPredictTest2 <- predict(RMFit2, testing)
RMPredictTest2

postResample(RMPredictTest2, testing$iphonesentiment)
confusionMatrix(RMPredictTest2, testing$iphonesentiment)
#Acc at 76%, Kappa at 53%
#Acc at 76%, Kappa at 53%




set.seed(998)

#define 75%/25% train/test split
inTraining <- createDataPartition(iphoneRFE$iphonesentiment, p = 0.70, list = FALSE)
training <- iphoneRFE[inTraining,]
testing <- iphoneRFE[-inTraining,]
nrow(training)
nrow(testing)

#10 fold cross val
fitControl <- trainControl(method ="repeatedcv", number =10, repeats = 1)

## C5.0 is a classification method
C50Fit3 <- train(iphonesentiment~., data = training, method = "C5.0", trControl=fitControl)
C50Fit3
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were trials = 1, model = tree and winnow = FALSE
## Accuracy: 0.77, Kappa: 0.56

## RM
RMFit3 <- train(iphonesentiment~., data = training, method = "rf", trControl = fitControl)
RMFit3
## Accuracy: 0.77, Kappa: 0.57

C50PredictTest3 <- predict(C50Fit3, testing)
C50PredictTest3

postResample(C50PredictTest3, testing$iphonesentiment)
confusionMatrix(C50PredictTest3, testing$iphonesentiment)
#Acc at 77%, Kappa at 55%
#Acc at 77%, Kappa at 55%

RMPredictTest3 <- predict(RMFit3, testing)
RMPredictTest3

postResample(RMPredictTest3, testing$iphonesentiment)
confusionMatrix(RMPredictTest3, testing$iphonesentiment)
#Acc at 81%, Kappa at 65%
#Acc at 81%, Kappa at 65%




## While C50 and RM are pretty similar, RM typically beats the Kappa score by the slightest amount
## I shall use RM for the LargeMatrix

iphoneLM <- read.csv("iphoneLargeMatrix.csv")

str(iphoneLM)

summary(iphoneLM)
is.na(iphoneLM)

## I liked the RFE feature seleciton last time. Unfortuantely since sentiment is empty here, I cannot perform the
## same function. I will instead select the same features used last time to this one

iphoneLMFS <- iphoneLM[,names(iphoneLM) %in% c("iphone", "googleandroid", "iphonedispos", "iphonedisneg", "samsunggalaxy", "htcphone",
                                               "iphonedisunc", "iphoneperpos", "ios", "iphoneperneg", "sonyxperia", "iphoneperunc",
                                               "iphonecampos", "iphonecamneg", "iphonecamunc", "htcdisunc", "htccampos", "htcperpos", "htccamneg", "iphonesentiment")]
str(iphoneLMFS)

iphoneLMFS$iphonesentiment <- as.factor(iphoneLMFS$iphonesentiment)

str(iphoneLMFS)

RMPredictLM <- predict(RMFit3, iphoneLMFS)
RMPredictLM

summary(RMPredictLM)

## 0: Sentiment Unclear - 20911
## 1: very negative - 0
## 2: somewhat negative - 1236
## 3: neutral - 861
## 4: somewhat positive - 1
## 5: very positive - 5349




## Repeat with Galaxy Small Matrix
galaxySM <- read.csv("galaxy_smallmatrix_labeled_9d.csv")
str(galaxySM)

summary(galaxySM)

plot_ly(galaxySM, x = ~galaxySM$galaxysentiment, type='histogram')





## Correlation
corrData <- cor(galaxySM)
corrData
corrplot(corrData)
## The correlations for galaxy are very weak for all features - needs more trimming via feature variance

nzvMetricsgal <- nearZeroVar(galaxySM, saveMetrics = TRUE)
nzvMetricsgal

nzvgal <- nearZeroVar(galaxySM, saveMetrics = FALSE) 
nzvgal

# create a new data set and remove near zero variance features
galaxyNZV <- galaxySM[,-nzvgal]
str(galaxyNZV)

## Let's sample the data before using RFE
set.seed(123)

galaxySample <- galaxySM[sample(1:nrow(galaxySM), 1000, replace=FALSE),]

## Set up rfeControl with randomforest, repeated cross validation and no updates
ctrlgal <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 5, verbose = FALSE)

## Use rfe and omit the response variable (attribute 59 galaxysentiment)
rfeResultsgal <- rfe(galaxySample[,1:58], galaxySample$galaxysentiment, sizes=(1:58), rfeControl=ctrlgal)

rfeResultsgal

plot(rfeResultsgal, type=c("g", "o"))

## create new data set with rfe recommended reatures
galRFE <- galaxySM[,predictors(rfeResultsgal)]

## add the dependent variable to galRFE
galRFE$galaxysentiment <- galaxySM$galaxysentiment

## review outcome
str(galRFE)

## I think galaxysentiment needs to be a factor - it is a categorical value
galaxySM$galaxysentiment <- as.factor(galaxySM$galaxysentiment)
galRFE$galaxysentiment <- as.factor(galRFE$galaxysentiment)
galaxyNZV$galaxysentiment <- as.factor(galaxyNZV$galaxysentiment)

str(galaxySM)
str(galRFE)
str(galaxyNZV)
## I like the galaxyNZV the most, with galRFE second




## Modeling w/ All Features first for OotB Accuracy and Kappa
set.seed(998)

#define 75%/25% train/test split
inTraining <- createDataPartition(galaxySM$galaxysentiment, p = 0.70, list = FALSE)
training <- galaxySM[inTraining,]
testing <- galaxySM[-inTraining,]
nrow(training)
nrow(testing)

#10 fold cross val
fitControl <- trainControl(method ="repeatedcv", number =10, repeats = 1)

## C5.0 is a classification method
C50FitGal1 <- train(galaxysentiment~., data = training, method = "C5.0", trControl=fitControl)
C50FitGal1
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were trials = 1, model = rules and winnow = FALSE
## Accuracy: 0.77, Kappa: 0.54

## SVM
SVMFitGal1 <- train(galaxysentiment~., data = training, method = "svmLinear2", trControl=fitControl)
SVMFitGal1
## Accuracy: 0.70, Kappa: 0.37

## kknn
kknnFitGal1 <- train(galaxysentiment~., data = training, method = "kknn", trControl=fitControl)
kknnFitGal1
## Accuracy: 0.74, Kappa: 0.50

## RM
RMFitGal1 <- train(galaxysentiment~., data = training, method = "rf", trControl = fitControl)
RMFitGal1
## Accuracy: 0.77, Kappa: 0.54

## C50 and RMFit preformed in a similar manner, and were both the best performing

C50PredictTestGal1 <- predict(C50FitGal1, testing)
C50PredictTestGal1

postResample(C50PredictTestGal1, testing$galaxysentiment)
confusionMatrix(C50PredictTestGal1, testing$galaxysentiment)
#Acc at 76%, Kappa at 52%
#Acc at 76%, Kappa at 52%

SVMPredictTestGal1 <- predict(SVMFitGal1, testing)
SVMPredictTestGal1

postResample(SVMPredictTestGal1, testing$galaxysentiment)
confusionMatrix(SVMPredictTestGal1, testing$galaxysentiment)
#Acc at 69%, Kappa at 35%
#Acc at 69%, Kappa at 35%

kknnPredictTestGal1 <- predict(kknnFitGal1, testing)
kknnPredictTestGal1

postResample(kknnPredictTestGal1, testing$galaxysentiment)
confusionMatrix(kknnPredictTestGal1, testing$galaxysentiment)
#Acc at 72%, Kappa at 47%
#Acc at 72%, Kappa at 47%

RMPredictTestGal1 <- predict(RMFitGal1, testing)
RMPredictTestGal1

postResample(RMPredictTestGal1, testing$galaxysentiment)
confusionMatrix(RMPredictTestGal1, testing$galaxysentiment)
#Acc at 76%, Kappa at 52%
#Acc at 76%, Kappa at 52%



## Now using data sets that were from feature selection
set.seed(998)

#define 75%/25% train/test split
inTraining <- createDataPartition(galRFE$galaxysentiment, p = 0.70, list = FALSE)
training <- galRFE[inTraining,]
testing <- galRFE[-inTraining,]
nrow(training)
nrow(testing)

#10 fold cross val
fitControl <- trainControl(method ="repeatedcv", number =10, repeats = 1)

## C5.0 is a classification method
C50FitGal2 <- train(galaxysentiment~., data = training, method = "C5.0", trControl=fitControl)
C50FitGal2
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were trials = 1, model = tree and winnow = FALSE
## Accuracy: 0.76, Kappa: 0.52

## RM
RMFitGal2 <- train(galaxysentiment~., data = training, method = "rf", trControl = fitControl)
RMFitGal2
## Accuracy: 0.77, Kappa: 0.53

C50PredictTestGal2 <- predict(C50FitGal2, testing)
C50PredictTestGal2

postResample(C50PredictTestGal2, testing$galaxysentiment)
confusionMatrix(C50PredictTestGal2, testing$galaxysentiment)
#Acc at 76%, Kappa at 52%
#Acc at 76%, Kappa at 52%

RMPredictTestGal2 <- predict(RMFitGal2, testing)
RMPredictTestGal2

postResample(RMPredictTestGal2, testing$galaxysentiment)
confusionMatrix(RMPredictTestGal2, testing$galaxysentiment)
#Acc at 76%, Kappa at 52%
#Acc at 76%, Kappa at 52%




set.seed(998)

#define 75%/25% train/test split
inTraining <- createDataPartition(galaxyNZV$galaxysentiment, p = 0.70, list = FALSE)
training <- galaxyNZV[inTraining,]
testing <- galaxyNZV[-inTraining,]
nrow(training)
nrow(testing)

#10 fold cross val
fitControl <- trainControl(method ="repeatedcv", number =10, repeats = 1)

## C5.0 is a classification method
C50FitGal3 <- train(galaxysentiment~., data = training, method = "C5.0", trControl=fitControl)
C50FitGal3
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were trials = 1, model = rules and winnow = TRUE
## Accuracy: 0.76, Kappa: 0.50

## RM
RMFitGal3 <- train(galaxysentiment~., data = training, method = "rf", trControl = fitControl)
RMFitGal3
## Accuracy: 0.76, Kappa: 0.50

C50PredictTestGal3 <- predict(C50FitGal3, testing)
C50PredictTestGal3

postResample(C50PredictTestGal3, testing$galaxysentiment)
confusionMatrix(C50PredictTestGal3, testing$galaxysentiment)
#Acc at 75%, Kappa at 49%
#Acc at 75%, Kappa at 49%

RMPredictTestGal3 <- predict(RMFitGal3, testing)
RMPredictTestGal3

postResample(RMPredictTestGal3, testing$galaxysentiment)
confusionMatrix(RMPredictTestGal3, testing$galaxysentiment)
#Acc at 75%, Kappa at 49%
#Acc at 75%, Kappa at 49%



## While C50 and RM are pretty similar, RM typically beats the Kappa score by the slightest amount
## I shall use RM for the LargeMatrix

galLM <- read.csv("galaxyLargeMatrix.csv")

str(galLM)

summary(galLM)

## I liked the RFE feature seleciton last time. Unfortuantely since sentiment is empty here, I cannot perform the
## same function. I will instead select the same features used last time to this one

galLMFS <- galLM[,!names(galLM) %in% c("id", "nokiadisneg", "sonycamneg")]
str(galLMFS)

galLMFS$galaxysentiment <- as.factor(galLMFS$galaxysentiment)

str(galLMFS)

RMPredictGLM <- predict(RMFitGal2, galLMFS)
RMPredictGLM

summary(RMPredictGLM)

## 0: Sentiment Unclear - 20706
## 1: very negative - 3
## 2: somewhat negative - 1198
## 3: neutral - 872
## 4: somewhat positive - 0
## 5: very positive - 5579