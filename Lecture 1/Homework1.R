#######################################################
### Solutions to week 1 homework
################################################

# Set working director to make it easier to load data
setwd('C:\\Users\\dbarron\\Dropbox\\Advanced Quant')
# Import data

peru <- read.csv('PeruMicroWeek1.csv')

# Review
str(peru)

# Note, has 9 variables, 31 observations

head(peru)

# Note, first three are integers, others numeric. regulated looks like a dummy variable

summary(peru)

## Means & standard deviations

# Simple way

with(peru,  mean(avgloanbal))
with(peru, mean(femaleperc, na.rm = TRUE))
with(peru, sd(avgloanbal))
with(peru, sd(femaleperc, na.rm = TRUE))

# Using package dplyr

library(dplyr)

peru %>% summarise_all(funs(mean, sd), na.rm = TRUE)


# Histogram

hist(peru$avgloanbal, xlab = 'Average loan balance')

lapply(peru, hist)

# Scatter plot

plot(selfsuff ~ I(avgloanbal / 10000), data = peru, ylim = c(0.7, 1.8))
abline(lm(selfsuff ~ I(avgloanbal / 10000), data = peru))

## Using ggplot2

library(ggplot2)

ggplot(peru, aes(x = avgloanbal, y = selfsuff)) +
  geom_point() +
  geom_smooth(method = 'lm')

# Regression

lm1 <- lm(selfsuff ~ I(avgloanbal / 10000) + femaleperc, data = peru)
summary(lm1)

# avgloanbal is statistically significant
# 
library(car)
car::vif(lm1)

qqPlot(lm1)
