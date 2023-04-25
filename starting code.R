df <- read.csv("Aemf1.csv")
head(df)
library(tidyverse)

model <- lm(Price~., data = df)

set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]
dim(train)
dim(test)



