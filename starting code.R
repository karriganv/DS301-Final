<<<<<<< HEAD
df <- read.csv("Aemf1.csv")
=======
df <- read.csv("https://raw.githubusercontent.com/karriganv/DS301-Final/main/Aemf1.csv")
>>>>>>> ba1cfe50a020d895d5fe17d89543419c79b74d27
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

ggplot(df, aes(x = City, fill = City)) + geom_histogram(stat = "count")

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

model.train <- lm(Price~.-Shared.Room -Private.Room -Room.Type -Attraction.Index -Restraunt.Index, data = train)

MSE_train = mean((train$Price - model.train$fitted.values)^2) 
MSE_train

predicted_values = predict(model.train,test)

MSE_test = mean((test$Price - predicted_values)^2)
MSE_test

library(leaps)
regfit = regsubsets(Price~.-Shared.Room -Private.Room -Room.Type -Attraction.Index -Restraunt.Index,data=train,nbest=1,nvmax=20) ##nbest = how many best models of size n do you want to report
##nvmax = maximum number of predictors you want to consider

regfit.sum = summary(regfit)
regfit.sum

n = dim(train)[1]
p = rowSums(regfit.sum$which)
adjr2 = regfit.sum$adjr2
cp = regfit.sum$cp
rss = regfit.sum$rss
AIC = n*log(rss/n) + 2*(p)
BIC = n*log(rss/n) + (p)*log(n)

cbind(p,rss,adjr2,cp,AIC,BIC)
plot(p,BIC)
plot(p,AIC)

which.min(BIC) 
which.min(AIC)
which.min(cp)
which.max(adjr2)

coef(regfit,16)

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
