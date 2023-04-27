library(tidyverse)

# Ellie Karrigan Megan

airbnb = read.csv("https://raw.githubusercontent.com/karriganv/DS301-Final/main/Aemf1.csv")
head(airbnb)
library(tidyverse)

model <- lm(Price~., data = airbnb)

set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(airbnb), replace=TRUE, prob=c(0.7,0.3))
train  <- airbnb[sample, ]
test   <- airbnb[!sample, ]
dim(train)
dim(test)

