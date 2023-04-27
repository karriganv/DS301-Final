df <- read.csv("Aemf1.csv")
head(df)
library(tidyverse)
library(ggplot2)

#EXPLORATION

str(df)

dim(df)

variable.names(df)

sum(is.na(df))

df$City <- as.factor(df$City)
df$Day <- as.factor(df$Day)
df$Room.Type <- as.factor(df$Room.Type)
df$Superhost <- as.factor(df$Superhost)

levels(df$Superhost)

levels(df$Room.Type)

summary(df$Price)

summary(df$Guest.Satisfaction)

summary(df$City)

hist(df$Cleanliness.Rating)

hist(df$City.Center..km.)

ggplot(df, aes(x = City, fill = City)) + geom_histogram(stat = "count") + theme(axis.text.x=element_text(size=11, angle=30, vjust=.8, hjust=0.8)) 

ggplot(df, aes(x = Price, color = City)) + geom_point(stat = "count")


##

model <- lm(Price~., data = df)

set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]
dim(train)
dim(test)

summary(model)

