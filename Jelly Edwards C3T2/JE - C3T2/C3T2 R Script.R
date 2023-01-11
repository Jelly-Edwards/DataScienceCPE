
library(caret)
library(readr)
library(SmartEDA)
library(gbm)

surveyData <- read.csv("CompleteResponses.csv")

#Looking at the data
attributes(surveyData)
str(surveyData)

#The data types for several attributes are incorrect, so I will update them.
#elevel, car, zipcode, and brand are factor types rather than int

surveyData$elevel <- as.factor(surveyData$elevel)
surveyData$car <- as.factor(surveyData$car)
surveyData$zipcode <- as.factor(surveyData$zipcode)
surveyData$brand <- as.factor(surveyData$brand)

str(surveyData)

#Loading Incomplete Data as Well

incomSurvey <- read.csv("SurveyIncomplete.csv")

incomSurvey$elevel <- as.factor(incomSurvey$elevel)
incomSurvey$car <- as.factor(incomSurvey$car)
incomSurvey$zipcode <- as.factor(incomSurvey$zipcode)
incomSurvey$brand <- as.factor(incomSurvey$brand)

str(incomSurvey)

# Check if there are any missing values
summary(surveyData)

is.na(surveyData)

# There are no missing values

# EDA

ExpReport(
  surveyData,
  Target="brand",
  label=NULL,
  op_file="Report.html",
  op_dir=getwd(),
  Rc="Yes")

# Created Report.html, with EDA of data. Brand set as dependent variable. 

# Set Seed
set.seed(998)

#define 75%/25% train/test split
inTraining <- createDataPartition(surveyData$brand, p = 0.75, list = FALSE)
training <- surveyData[inTraining,]
testing <- surveyData[-inTraining,]
nrow(training)
nrow(testing)

#10 fold cross validation
fitControl <- trainControl(method ="repeatedcv", number =10, repeats = 1)

#Feature Selection
#Selected these since I prev did a model with all features, and the variable importance
#showed that Salary, Age, and Credit typically were important in the models
featSelect <- training[, c("salary","age","credit","brand")]
featSelect

#train stochastic gradient boosting model (GBM)
GBMFit1 <- train(brand~., data = featSelect, method ="gbm", trControl=fitControl)

#training results for GBM
GBMFit1

# Variable importance
varImp(GBMFit1, numTrees = 150)

#Two Variables show the most importance: Salary and Age

rfGrid <- expand.grid(mtry=c(1:5))

# Random Forest model
rfFit1 <- train(brand~., data=featSelect, method = "rf", trControl=fitControl, tuneGrid= rfGrid, tuneLength = 5)

rfFit1

varImp(rfFit1)
#Salary, Age, very high. Credit at 10

#Predictions Using Testing of SurveyData
rfPredictTesting <- predict(rfFit1, testing)
rfPredictTesting

postResample(rfPredictTesting, testing$brand)
#Acc at ~ 92%, Kappa at ~83%
confusionMatrix(rfPredictTesting, testing$brand)

#Predictions using Incomplete Survey Test Set
surveyIncomPredict <- predict(rfFit1, incomSurvey)
surveyIncomPredict

postResample(surveyIncomPredict, incomSurvey$brand)
#Acc at ~39%, Kappa at ~1%
confusionMatrix(surveyIncomPredict, incomSurvey$brand)

summary(rfPredictTesting)
#0 ~ 963, 1 ~ 1511
summary(surveyIncomPredict)
#0 ~1911, 1 ~ 3089

write.csv(GBMFit1, "C:\\Users\\jelly\\Documents\\DataAnalytics\\GBMFit1.csv", row.names=FALSE)
