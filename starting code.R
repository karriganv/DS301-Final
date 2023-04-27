df <- read.csv("Aemf1.csv")
head(df)
library(tidyverse)
library(ggplot2)

#EXPLORATION

str(df)

dim(df)

variable.names(df)

sum(is.na(df))

summary(df$Price)

df$City <- as.factor(df$City)
df$Day <- as.factor(df$Day)
df$Room.Type <- as.factor(df$Room.Type)

summary(df$City)

ggplot(df, aes(x = City, fill = City)) + geom_histogram(stat = "count")


##

model <- lm(Price~., data = df)

set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]
dim(train)
dim(test)



