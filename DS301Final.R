df <- read.csv("https://raw.githubusercontent.com/karriganv/DS301-Final/main/Aemf1.csv")
head(df)
library(tidyverse)
<<<<<<< HEAD
=======
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

summary(df$Cleanliness.Rating)

hist2  =  hist(df$City.Center..km., xlab = "Distance from city center (km)", col = 'lightblue', main = "City Center (km)")

ggplot(df, aes(x = City, fill = City)) + geom_histogram(stat = "count")

ggplot(df, aes(x = Price, color = City)) + geom_point(stat = "count")

## Identification of Variable Importance

library(caret)
library(randomForest)
library(varImp)

regressor <- randomForest(Price~., data = df, importance=TRUE) 

varImp(regressor) 
varImp(regressor, conditional=TRUE)

##

model <- lm(Price~., data = df)

set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]
dim(train)
dim(test)

model.train <- lm(Price~.-Shared.Room -Private.Room -Room.Type -Attraction.Index -Restraunt.Index, data = train)

predicted_values = predict(model.train,test)

predicted_values

install.packages("ltm")
library(ltm)

# correlation exploration
cor(df$Bedrooms, df$Person.Capacity)
biserial.cor(df$Bedrooms,df$Superhost)
kruskal.test(df$Person.Capacity,df$Room.Type)


library(tree)
tree.df = tree(City~.-Shared.Room -Private.Room -Room.Type -Attraction.Index -Restraunt.Index,split=c("deviance"),data=train)

summary(tree.df)

plot(tree.df)
text(tree.df,pretty=0)

tree.pred = predict(tree.df, newdata=test)

Y.test = df[-train,"City"]
mean((tree.pred - Y.test)^2)

