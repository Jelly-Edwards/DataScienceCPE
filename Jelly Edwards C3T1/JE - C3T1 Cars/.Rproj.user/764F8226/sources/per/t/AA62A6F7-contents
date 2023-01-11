install.packages("readr")
library(readr)
CarsDF<- read.csv("cars.csv")
attributes(CarsDF)#List your attributes within your data set.
summary(CarsDF) #Prints the min, max, mean, median, and quartiles of each attribute.
str(CarsDF) #Displays the structure of your data set.
names(CarsDF) #Names your attributes within your data set.
CarsDF$name.of.car #Will print out the instances within that particular column in your data set.
hist(CarsDF$speed.of.car) #Create Histogram
plot(CarsDF$speed.of.car,CarsDF$distance.of.car) #Create Scatter Plot
qqnorm(CarsDF$speed.of.car) #create normal Quantile plot
CarsDF$distance.of.car<-as.numeric(CarsDF$distance.of.car) #changing data type of column
str(CarsDF) #checking all data types
CarsDF$distance.of.car<-as.integer(CarsDF$distance.of.car)
str(CarsDF) #checking all data types
names(CarsDF)<-c("name","speed","dist") #updating column names
names(CarsDF) #checking column names
summary(CarsDF) #Will count how many NA's you have
is.na(CarsDF) #Will show your NA's through logical data

#Test and Training Sets
#Create a seed, which is a starting point used to create a sequence of random numbers
set.seed(123)

#Creating testing and training sets
trainSize<-round(nrow(CarsDF)*0.7)
testSize<-nrow(CarsDF)-trainSize

trainSize
testSize

training_indices<-sample(seq_len(nrow(CarsDF)),size =trainSize)
trainSet<-CarsDF[training_indices,]
testSet<-CarsDF[-training_indices,] 

DistModel<-lm(dist~speed, trainSet)
summary(DistModel)
#Multiple R^2 is how well the regression line fits data, with 1 being perfect fit
#p-value tells how much the X variable affects the Y variable. more than 0.05 means 
#independent variable has no effect on the dependent variable, while less than 0.05 means its significant

DistPredictions<-predict(DistModel, testSet)
DistPredictions
