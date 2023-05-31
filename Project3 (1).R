library(DescTools)
'We are loading the "DescTools" library, which provides a collection of tools for descriptive statistics and data analysis.'
##
df<-read.csv('C:/Users/utkar/Downloads/Real Estate.csv')
dim(df)
'We are reading a CSV file called "Real Estate.csv" from the specified directory using the read.csv() function and assigning it to a data frame called df.
We are checking the dimensions of the df data frame using the dim() function, which returns the number of rows and columns in the data frame.
We have 414 rows and 7 columnsin our dataset.'
head(df)

#Number of null values in each column.
colSums(is.na(df))
'We do not have any null value in our dataset.We have calculated the number of null values in each column of the df data frame
using the colSums() function in combination with the is.na() function, which returns a vector of the same length as the number 
of columns in the data frame, with each element representing the number of missing values in the corresponding column.So we got 0 null values in each column.'

'The objective set:
The objective of this study is to understand the relationship between house price of unit area and the other factors'
#Variable and their types
sapply(df,typeof)
'The discrete variables from the given output are: Number.of.convenience.stores and Transaction.date.
The continuous variables from the given output are: House.age, Distance.to.the.nearest.MRT.station,
Latitude, Longitude, and House.price.of.unit.area.'

'Descirptions of variables :
1] Transaction.date - the year at which the transaction has took place
2] House.age - Age of house
3] Distance.to.the.nearest.MRT.station - distance between the house and nearest metro station
4] Number.of.convenience.stores - Number of the convenience stores around the house
5] Latitude - Latitude of house location
6] Longitude - Longitude of house location
7] House.price.of.unit.area - house price of unit area'
#Unique years in dataset
unique(df$Transaction.date)
'The data in the data frame spans over two years(2012 & 2013).'
#The dataset contains 2 years of data. From the year 2012 to 2013.

#Unique years in dataset
unique(df$House.age)
'This code will return the unique values of the column "House.age" from the data-frame df.
This code is useful for exploring the unique values in a column and understanding the 
distribution of values in the dataset.'
#A quick statistical summary of each column:
summary(df)
'For the variable "Transaction.date", it shows the minimum and maximum values, as well as the first, second (median), and third quartiles. The same information is provided for the variables "House.age", "Distance.to.the.nearest.MRT.station", "Number.of.convenience.stores", "Latitude", "Longitude", and "House.price.of.unit.area".
From this summary, we can see that the "Transaction.date" column contains data only from the years 2012 and 2013. The "House.age" column ranges from 0 to 43.8 years, with a median age of 16.1 years. The "Distance.to.the.nearest.MRT.station" column has a large range, from 23.38 to 6488.02, and a median of 492.23 meters. The "Number.of.convenience.stores" column ranges from 0 to 10, with a median of 4. The "Latitude" column has a small range, from 24.93 to 25.01, and a median of 24.97. The "Longitude" column has a small range, from 121.5 to 121.6, and a median of 121.5. Finally, the "House.price.of.unit.area" column has a wide range, from 7.6 to 117.5, and a median of 38.45.
This summary provides a good overview of the range and distribution of the variables in the dataset .'


plot(df)


#Outlier detection
boxplot(df$House.age)
boxplot(df$Distance.to.the.nearest.MRT.station)
boxplot(df$Number.of.convenience.stores)
boxplot(df$Latitude)
boxplot(df$Longitude)
'After examining the boxplots of each column, it can be observed that there are many 
outliers that cannot be simply removed from the dataset. Therefore, a data cleaning 
technique called "winsorizing" needs to be applied to address these outliers.
Winsorizing is a statistical method used to handle outliers. It involves replacing
the values that exceed the extreme value thresholds with the limit values. These 
extreme values are identified by calculating a certain percentile of the data.For eg To provide an illustration, 
applying a 90% winsorization to a dataset would imply that any data point that lies outside the 5th and 95th percentiles of the distribution will be replaced with the values of the 5th and 95th percentile data points, correspondingly.'
df[1:6]<-lapply(df[1:6], Winsorize)
boxplot(df$Distance.to.the.nearest.MRT.station)
boxplot(df$Latitude)
boxplot(df$Longitude)


## Assumption 1: 
#> Linear relationship between dependent and independent variable.
#Checking this assumption by plotting each dependent variable against dependent variable in a scatter plot.

#Before Using BoxCox
plot(House.price.of.unit.area ~ Transaction.date,data=df)
plot(House.price.of.unit.area ~ House.age,data=df)
plot(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station,data=df)
plot(House.price.of.unit.area ~ Number.of.convenience.stores,data=df)
plot(House.price.of.unit.area ~ Latitude,data=df)
plot(House.price.of.unit.area ~ Longitude,data=df)

for (i in range(1,6)) {
  x = (df[,i])
  lambda = BoxCoxLambda(x)
  df[,i] = BoxCox(x, lambda)
}

#After using BoxCox
plot(House.price.of.unit.area ~ Transaction.date,data=df)
plot(House.price.of.unit.area ~ House.age,data=df)
plot(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station,data=df)
plot(House.price.of.unit.area ~ Number.of.convenience.stores,data=df)
plot(House.price.of.unit.area ~ Latitude,data=df)
plot(House.price.of.unit.area ~ Longitude,data=df)

df[1:6]

#Assumption 2:
#Dependent Variables are not highly correlated
correlation <- cor(df[1:6])
correlation>0.7
heatmap(correlation)
#As we can see that the dependent Variables are not highly correlated

library(cars)
#VIF
df.lm<-lm(House.price.of.unit.area ~Transaction.date+House.age+Distance.to.the.nearest.MRT.station+Number.of.convenience.stores+Latitude+Longitude,data=df)

#Checking multicollinearity 
vif(df.lm)[vif(df.lm)>1]

#Assumption 3, 4 and 5
par(mfrow=c(2,2))
plot(df.lm)

#The model and variable selection
summary(df.lm)


#Transaction.date, House.age,Distance.to.the.nearest.MRT.station and Latitude are the variables that we will
#select for Multiple linear regression as we can see from above summary that the probability of each of
#these parameters' slope with respect to House.price.of.unit.area to be 0 is significantly lower than 1%e

df.lm<-lm(House.price.of.unit.area ~Transaction.date+House.age+Distance.to.the.nearest.MRT.station+Latitude,data=df)
summary(df.lm)



library(leaps)
set.seed(1)
p <- 6
regfit.best = regsubsets(House.price.of.unit.area ~Transaction.date+House.age+Distance.to.the.nearest.MRT.station+Number.of.convenience.stores+Latitude+Longitude, data=df, nvmax = p, method = "forward")
par(mfrow=c(1,1))
plot(regfit.best, scale = 'adjr' )
