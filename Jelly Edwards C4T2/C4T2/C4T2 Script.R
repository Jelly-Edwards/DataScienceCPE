install.packages("RMariaDB")
install.packages("backports")
install.packages("googledrive")
install.packages("googlesheets4")
install.packages("lubridate")
install.packages("plotly")

library(RMariaDB)
library(tidyverse)
library(lubridate)
library(plotly)

con = dbConnect(MariaDB(), user='deepAnalytics',password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# List the tables contained in the database
dbListTables(con)

## Lists attributes contained in a table
dbListFields(con,'iris')

irisALL <- dbGetQuery(con, "SELECT * FROM iris")
irisALL

irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")
irisSELECT

dbListFields(con, 'yr_2006')
dbListFields(con, 'yr_2007')
dbListFields(con, 'yr_2008')
dbListFields(con, 'yr_2009')
dbListFields(con, 'yr_2010')


##  Visualizations and time series analysis of sub-meters. Pull each year the Date, Time, & 3 sub meter attributes

yr06 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
yr07 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr08 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr09 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr10 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")

str(yr06)
str(yr07)
str(yr08)
str(yr09)
str(yr10)
## Date & Time are characters, Rest are Nums

summary(yr06)
summary(yr07)
summary(yr08)
summary(yr09)
summary(yr10)

head(yr06)
## Begins at 06-12-16
head(yr07)
## Begins at 07-01-01
head(yr08)
## Begins at 08-01-01
head(yr09)
## Begins at 09-01-01
head(yr10)
## Begins at 10-01-01

tail(yr06)
## Ends at 06-12-31
tail(yr07)
## Ends at 07-12-31
tail(yr08)
## Ends at 08-12-31
tail(yr09)
## Ends at 09-12-31
tail(yr10)
## Ends at 10-11-26

## Both 06 and 10 do not cover a full year. 06 covers roughly 2 weeks, while 10 covers 11 months.

smDF <- bind_rows(yr07, yr08, yr09)

str(smDF)
summary(smDF)
head(smDF)
tail(smDF)

## Combine Date and Time attribute values in a new attribute column
smDF2 <-cbind(smDF,paste(smDF$Date,smDF$Time), stringsAsFactors=FALSE)
str(smDF2)

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(smDF2)[6] <-"DateTime"

## Move the DateTime attribute within the dataset
smDF2 <- smDF2[,c(ncol(smDF2), 1:(ncol(smDF2)-1))]
head(smDF2)


## Convert DateTime from character to POSIXct 
smDF2$DateTime <- as.POSIXct(smDF2$DateTime, "%Y/%m/%d %H:%M:%S")

str(smDF2)
summary(smDF2)
head(smDF2)
tail(smDF2)

## Add the time zone
attr(smDF2$DateTime, "tzone") <- "UTC"
## Using Coordinated Universal Time instead of EU/Paris because the time originally given is in UTC anyways. UTC is better for coding purposes as well

## Inspect the data types 
str(smDF2)
# Date is POSIXct data type. Values is a combination between Date and Time

## Create "year" attribute with lubridate
smDF2$year <- year(smDF2$DateTime)
str(smDF2)
# created year, has num format and uses YEAR from the the DateTime attribute

## Create "month" attribute with lubridate
smDF2$month <- month(smDF2$DateTime)
str(smDF2)
# created month, has num format 

## Create "quarter" attribute with lubridate
smDF2$quarter <- quarter(smDF2$DateTime)
str(smDF2)
# created quarter, has int format 

## Create "week" attribute with lubridate
smDF2$week <- week(smDF2$DateTime)
str(smDF2)
# created week, has num format 

## Create "weekday" attribute with lubridate
smDF2$weekday <- wday(smDF2$DateTime)
str(smDF2)
# created weekday, has num format 

## Create "day" attribute with lubridate
smDF2$day <- day(smDF2$DateTime)
str(smDF2)
# created day, has int format 

## Create "hour" attribute with lubridate
smDF2$hour <- hour(smDF2$DateTime)
str(smDF2)
# created hour, has int format 

## Create "minute" attribute with lubridate
smDF2$minute <- minute(smDF2$DateTime)
str(smDF2)
# created minute, has int format 

summary(smDF2)
# The mean shows what the avg value is. This would show which meter uses the most power. 
# SubMeter 3 uses the most, with the largest mean. SubMeter 2 uses the least


## Plot all of Sub-Meter 1
plot(smDF2$Sub_metering_1)
# the plot is too cluttered and difficult to read. We need to subset

## Subsete the second week of 2008 - All Observations
houseWeek <- filter(smDF2, year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)
# Still very cluttered, needs to adjust granularity

##Subset the 9th day of January 2008 - All Observations
houseDay <- filter(smDF2, year == 2008 & month == 1 & day == 9)

## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay $Sub_metering_1, type = 'scatter', mode = 'lines')
# Increased usage of power in Sub Meter 1 around 5 - 6 PM, which makes sense because this is the kitchen and this is around dinner time

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(smDF2, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

##Subset the 2nd Week of January 2008 - All Observations
houseWeek <- filter(smDF2, year == 2008 & month == 1 & week == 2 & (hour == 0 | hour == 1 | hour == 2 | hour == 3 | hour == 4 | hour == 5| 
                                                                      hour == 6 | hour == 7 | hour == 8 | hour == 9 | hour == 10| 
                                                                      hour == 11 | hour == 12 | hour == 13 | hour == 14 | hour == 15|
                                                                      hour == 16 | hour == 17 | hour == 18 | hour == 19 | hour == 20|
                                                                      hour == 21 | hour == 22 | hour == 23))

## Plot Sub-Meter 1, 2, and 3 with Title, Legend, and Labels for Week
plot_ly(houseWeek, x = ~houseWeek$DateTime, y = ~houseWeek$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 2nd Week of January, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

##Subset the 2nd Week of January 2008 - All Observations
houseWeekhr3 <- filter(smDF2, year == 2008 & month == 1 & week == 2 & (hour == 0 | hour == 3| hour == 6 | hour == 9 | hour == 12 | hour == 15 | hour == 18 |
                                                                         hour == 21 | hour == 24))

## Plot Sub-Meter 1, 2, and 3 with Title, Legend, and Labels for Week
plot_ly(houseWeekhr3, x = ~houseWeekhr3$DateTime, y = ~houseWeekhr3$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeekhr3$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeekhr3$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 2nd Week of January, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset of January 2008 - All Observations
houseMonth <- filter(smDF2, year == 2008 & month == 1 & (week == 1 | week == 2 | week == 3 | week == 4)
                     & (hour == 0 | hour == 3| hour == 6 | hour == 9 | hour == 12 | hour == 15 | hour == 18 |
                                                           hour == 21 | hour == 24))

plot_ly(houseMonth, x = ~houseMonth$DateTime, y = ~houseMonth$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseMonth$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseMonth$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption of January, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset of 2008 - All Observations
houseYear <- filter(smDF2, year == 2008 & (hour == 0 | hour == 6| hour == 12 | hour == 18 | hour == 24))

plot_ly(houseYear, x = ~houseYear$DateTime, y = ~houseYear$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseYear$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseYear$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption of 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

houseYear2 <- filter(smDF2, year == 2008 & (hour == 0 | hour == 8| hour == 16 | hour == 24))

plot_ly(houseYear2, x = ~houseYear2$DateTime, y = ~houseYear2$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseYear2$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseYear2$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption of 2008 2",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# I prefer the one w/ every 6 hours

## Subset to one observation per week on Mondays at 8 PM for 2007, 2008, 2009
house070809weekly <- filter(smDF2, weekday == 2 & hour == 20 & minute == 1)

##Create TS object w/ SubMeter3
tmSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency = 52, start=c(2007,1))

## Plot sub-meter 3 w/ autoplot
library(ggplot2)
library(ggfortify)
autoplot(tmSM3_070809weekly)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tmSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")

## Plot sub-meter 3 with plot.ts
plot.ts(tmSM3_070809weekly)

# My final adjustments for this plot
autoplot(tmSM3_070809weekly, xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3 - Mondays at 8 PM for 07/08/09")

## Making Sub Meter 1
tsSM1_070809weekly <- ts(house070809weekly$Sub_metering_1, frequency = 52, start=c(2007,1))
autoplot(tsSM1_070809weekly, ts.colour = 'black', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1")

## Subset per week, for four hours around usual dinner time on Saturday
house070809weekly2 <- filter(smDF2, weekday == 1 & (hour == 15 | hour == 16 | hour == 17 | hour == 18)
                             & minute == 1)
summary(smDF2)

weekdays060709 <- ts(house070809weekly2$Sub_metering_1, frequency = 208, start=c(2007,1))
autoplot(weekdays060709, xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1 - Sundays From 15 - 18 PM for 07/08/09")

plot.ts(weekdays060709)

## Subset for SM2 for Sundays every 6 hrs for 07, 08, 09
houseforSM2Tuesday<- filter(smDF2, (weekday == 1) & (hour == 0 | hour == 6| hour == 12 | hour == 18 | hour == 24) & minute == 1)

tuesdaySM2 <- ts(houseforSM2Tuesday$Sub_metering_2, frequency = 208, start=
                c(2007,1))
autoplot(tuesdaySM2, xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2 - Sundays for 07/08/09")

plot.ts(tuesdaySM2)

## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built

install.packages("forecast")
library(forecast)

fitSM3 <- tslm(tmSM3_070809weekly ~ trend + season) 
summary(fitSM3)
## RMSE is 9.046 and R^2 0.263

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3
plot(forecastfitSM3)

## Grey areas are confidence bands. Dark grey and light grey are possible locations can be

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 30), ylab= "Watt-Hours", xlab="Time", main = "Sub-Meter 3 - Forecasts from Linear Regression Model")
## Assignment said 20 but that was too small, soo I adjusted to 30

## Sub Meter 1
fitSM1 <- tslm(weekdays060709 ~ trend + season)
summary(fitSM1)
## RMSE is 10.7 and R^2 0.381
forecastfitSM1 <- forecast(fitSM1, h=40)
plot(forecastfitSM1)
## Used 25 weeks

## Create sub-meter 1 forecast with confidence levels 80 and 90
forecastfitSM1c <- forecast(fitSM1, h=40, level=c(80,90))
plot(forecastfitSM1c, ylim = c(0, 70), ylab= "Watt-Hours", xlab="Time", main = "Sub-Meter 1 - Forecasts from Linear Regression Model")


## Sub Meter 2
fitSM2 <- tslm(tuesdaySM2 ~ trend + season)
summary(fitSM2)
## RMSE is 9.363  and R^2 0.304
forecastfitSM2 <- forecast(fitSM2, h=40)
plot(forecastfitSM2)
## Used 25 weeks

## Create sub-meter 2 forecast with confidence levels 80 and 90
forecastfitSM2c <- forecast(fitSM2, h=40, level=c(80,90))
plot(forecastfitSM2c, ylim = c(0, 75), ylab= "Watt-Hours", xlab="Time", main = "Sub-Meter 2 - Forecasts from Linear Regression Model")

## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tmSM3_070809weekly)
## Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)

## Decomposing Sub-Meter 1
decomposeSM1 <- decompose(weekdays060709)
plot(decomposeSM1)
summary(decomposeSM1)


## Decomposing Sub-Meter 2
decomposeSM2 <- decompose(tuesdaySM2)
plot(decomposeSM2)
summary(decomposeSM2)

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tmSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)
## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))

##Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))

## SM1 Holt Winter
SM1Adjusted <- weekdays060709 - decomposeSM1$seasonal
autoplot(SM1Adjusted)
plot(decompose(SM1Adjusted))

SM1HW <- HoltWinters(SM1Adjusted, beta=FALSE, gamma=FALSE)
plot(SM1HW, ylim = c(0, 25))

SM1HWfor <- forecast(SM1HW, h=25)
plot(SM1HWfor, ylim = c(0, 25), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")

SM1HWforC <- forecast(SM1HW, h=25, level=c(10,25))
## Plot only the forecasted area
plot(SM1HWforC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1", start(2010))

## SM2 Holt Winter
SM2Adjusted <- tuesdaySM2 - decomposeSM2$seasonal
autoplot(SM2Adjusted)
plot(decompose(SM2Adjusted))

SM2HW <- HoltWinters(SM2Adjusted, beta=FALSE, gamma=FALSE)
plot(SM2HW, ylim = c(0, 25))

SM2HWfor <- forecast(SM2HW, h=25)
plot(SM2HWfor, ylim = c(0, 25), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")

SM2HWforC <- forecast(SM2HW, h=25, level=c(10,25))
## Plot only the forecasted area
plot(SM2HWforC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))

