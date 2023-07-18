getwd()
setwd('C:/Users/DIBYARANJAN/R_Code/newRcode')

library(ggplot2)
library(dplyr)
library(caTools)
library(readr)
library(DataExplorer)
library(lubridate)

#Reading the Data set
ecom <- read.csv('Ecommerce.csv')
View(ecom)
summary(ecom)
glimpse(ecom)
dim(ecom)

#Null Check
sum(is.na(ecom))
sapply(ecom, function(x) sum(is.na(x)))
plot_missing(ecom)

#Dropping NA values
ecom <- na.omit(ecom)
dim(ecom)
View(ecom)

#Formatting the Date Column
ecom$dates <- dmy(ecom$InvoiceDate)
ecom$dates <- as.Date(ecom$dates, format= "%d/%m/%Y")
typeof(ecom$dates)
class(ecom$dates)
ecom$dates <- as.character(ecom$dates)
ecom$year <- sapply(ecom$dates, FUN = function(x) {strsplit(x, split= '[-]')}[[1]][1])
ecom$month <- sapply(ecom$dates, FUN = function(x) {strsplit(x, split= '[-]')}[[1]][2])
ecom$dayOfweek <- wday(ecom$dates, label=TRUE)

#Adding the Sales Column
Sales <- ecom$Quantity * ecom$UnitPrice
ecom$Sales <- Sales 
View(ecom)
names(ecom)

#1.Identifying Outliers

summary(ecom$CustomerID)
sort(ecom$CustomerID)
boxplot(ecom$CustomerID,
        main="CustomerID",
        horizontal=TRUE)

summary(ecom$Quantity)
boxplot(ecom$Quantity,
        main='Quantity',
        horizontal=TRUE)

#1.Dropping Quantity Outliers
ecom <- subset(ecom, Quantity >= 0 & Quantity <10000  )
View(ecom)

#2.Identifying Outliers
summary(ecom$UnitPrice)
boxplot(ecom$UnitPrice,
        main='Unit Price',
        horizontal=TRUE)

#2.Dropping Price Outliers
ecom <- subset(ecom, UnitPrice < 5000)
View(ecom)
summary(ecom)


#Converting to Factors
ecom$Country <- as.factor(ecom$Country)
ecom$month <- as.factor(ecom$month)
ecom$year <- as.factor(ecom$year)
ecom$dates <- as.factor(ecom$dates)
View(ecom)

#Visualization of  Sales by Dates
ecom %>% group_by(dates) %>%
  summarise(Revenue =sum(Sales)) %>% 
  ggplot (aes(x= dates , y=Revenue))+ geom_line() +geom_smooth(method = 'auto', se= FALSE) + labs(x= 'Date', y='Revenue', Title="Sales by Date")

#Sales by Day of the Week
ecom%>% group_by(dayOfweek) %>%
  summarise(Revenue = sum(Sales)) %>%
  ggplot(aes(x= dayOfweek, y = Revenue))+ geom_col() + labs(x = 'Day of Week', y = 'Revenue', title = 'Sales by Day of Week')

#High Value Customer Find Out
High_Value_Customer <- ecom %>% group_by(CustomerID) %>% summarise(Revenue = sum(Sales))
High_Value_Customer <- High_Value_Customer %>% arrange(desc(Revenue))
View(High_Value_Customer)

#Getting ready with test data
names(ecom)
data <- ecom %>% select(c(4,6,10,11,13))
View(data)
data <- na.omit(data)
glimpse(data)
summary(data)
plot_missing(data)
########################K-means Clustering
result <- kmeans(data,centers=8)

# view the result
result

# Finding Elbow point
wssplot <- function(data, nc){
  wss <- 0
  for (i in 1:nc){
    wss[i] <- sum(kmeans(data,centers = i)$withinss)}
  plot(1:nc,wss,type='b',xlab="number of clusters",ylab="within groups sum")}

wssplot(data,10)

################## Hierarchical Clustering ##########################
View(data)
data$year <- as.numeric(data$year)
data$month <- as.numeric(data$month)
glimpse(data)
summary(data)
m <- apply(data,2,mean) # find out the center of a column
s <- apply(data,2,sd) # find out the Std Deviation
z <- scale(data,m,s)
summary(z)
glimpse(z)
distance <- dist(z)

hc_l <- hclust(distance) # create hierarchical clustering

hc_l

members <- cutree(hc_l,3) # cut the dendrogram
members


data$cluster <- members

View(data)













