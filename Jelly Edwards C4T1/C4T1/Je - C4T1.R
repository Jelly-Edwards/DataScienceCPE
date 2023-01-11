install.packages("RMariaDB")
install.packages("backports")
install.packages("googledrive")
install.packages("googlesheets4")
install.packages("lubridate")

library(RMariaDB)
library(tidyverse)
library(lubridate)

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