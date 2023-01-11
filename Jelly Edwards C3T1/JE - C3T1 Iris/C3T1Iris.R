install.packages(readr)
library("readr")
IrisDataset<-read.csv("iris.csv")
#Error: Missing "" in ()

attributes(IrisDataset)
summary(IrisDataset)
#Error: Missing I from dataset name

str(IrisDataset)
#Error: added an s to dataset name

names(IrisDataset)
hist(IrisDataset$Sepal.Length)
#Error: can't use txt string attribute, must be numeric. Chose Sepal.Length rather than Species

plot(IrisDataset$Sepal.Length)
#Error: Missing )

qqnorm(IrisDataset$Sepal.Length)
#Error: Can only be used with data frame w/ all numeric alike variables. Will specify a numeric attribute instead
#IrisDataset$Species<- as.numeric(IrisDataset$Species) 
#Error: This is a complete character vector. Converting to Numeric will change all values to NA. Commented out


set.seed(123)

trainSize <- round(nrow(IrisDataset)*0.7)
#Error: We shouldn't put train size at 0.2. Its too small, but should rather be 0.7 or 0.8

testSize <- nrow(IrisDataset)-trainSize
#Error: Its called trainSize not trainSet

trainSize
#Error: Doesn't need S

testSize

training_indices<-sample(seq_len(nrow(IrisDataset)),size =trainSize)

trainSet<- IrisDataset[training_indices, ]
#Error: Missing code line that defines training_indices

testSet<- IrisDataset[-training_indices, ]

#Trying to add new seed and training/testing is not necessary

LinearModel<- lm(Petal.Width~Petal.Length, trainSet)
#Error: lm function is incorrectly written. Rewritten to be correct

summary(LinearModel)
# R^2 close to 1. p value is close to 0. Very close fit, and X affects Y

prediction <- predict(LinearModel, testSet)
#Error: Written incorrectly

prediction
prediction