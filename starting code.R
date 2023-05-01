df <- read.csv("https://raw.githubusercontent.com/karriganv/DS301-Final/main/Aemf1.csv")
head(df)
library(tidyverse)
library(randomForest)
library(ggplot2)

#EXPLORATION

str(df)

dim(df)

variable.names(df)

sum(is.na(df))

df$City <- as.factor(df$City)
df$Day <- as.factor(df$Day)
df$Shared.Room <- as.factor(df$Shared.Room)
df$Private.Room <- as.factor(df$Private.Room)
df$Room.Type <- as.factor(df$Room.Type)
df$Multiple.Rooms <- as.factor(df$Multiple.Rooms)
df$Business <- as.factor(df$Business)
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

library(scales)
theme_set(theme_classic())


ggplot(df, aes(x=City, y=Price)) + 
  geom_point(col="navy", size=3) +   
  geom_segment(aes(x=City, 
                   xend=City, 
                   y=min(Price), 
                   yend=max(Price)), 
               linetype="dashed", 
               linewidth=0.1) +   
  labs(title="Dot Plot", 
       subtitle="City vs Price") +  
  coord_flip()


## QUESTION 1

#Initial Variable Selection

library(ISLR2)

#check for multicollinearity
cor(df[sapply(df, is.numeric)])

#install.packages("car")
library(car)
fit1 = lm(Price~.-Attraction.Index -Restraunt.Index -Shared.Room -Private.Room -Normalised.Restraunt.Index,data=df)
summary(fit1)
vif(fit1)

#Best Subset Selection - AIC,BIC, etc.



library(leaps)
regfit = regsubsets(Price~.-Attraction.Index -Restraunt.Index -Shared.Room -Private.Room -Normalised.Restraunt.Index,data=df,nbest=1,nvmax=21)

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

coef(regfit,19)

#Best Subset Selection - Cross Validation

k = 10
folds = sample(1:k,nrow(df),replace=TRUE)

val.errors = matrix(NA,k,20)

for(j in 1:k){
  test1 = df[folds==j,]
  train1 = df[folds!=j,]
  
  best.fit = regsubsets(Price~.-Attraction.Index -Restraunt.Index -Shared.Room -Private.Room -Normalised.Restraunt.Index,data=train1,nbest=1,nvmax=20) ##nbest = how many best models of size n do you want to report
  
  for(i in 1:20){
    test.mat = model.matrix(Price~.-Attraction.Index -Restraunt.Index -Shared.Room -Private.Room -Normalised.Restraunt.Index,data=test1)
    
    coef.m = coef(best.fit,id=i)
    
    pred = test.mat[,names(coef.m)]%*%coef.m
    val.errors[j,i] = mean((test1$Price-pred)^2)
  }
  
}

cv.errors = apply(val.errors,2,mean)
which.min(cv.errors)

coef(regfit,19)

## QUESTION 2

set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

library(tree)
tree.df = tree(City~.-Shared.Room -Private.Room -Room.Type -Attraction.Index -Restraunt.Index,split=c("deviance"),data=train)

summary(tree.df)

plot(tree.df)
text(tree.df,pretty=0)

tree.pred = predict(tree.df, test, type='class')
table(tree.pred,test$City)

library(caret)
library(randomForest)
library(varImp)

regressor <- randomForest(City~., data = df, importance=TRUE) 

varImp(regressor) 
varImp(regressor, conditional=TRUE)


