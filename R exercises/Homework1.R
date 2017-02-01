#######################################################
### Solutions to week 1 homework
################################################

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

peru %>% summarise(mean(avgloanbal), sd(avgloanbal),
                   mean(femaleperc, na.rm = TRUE), sd(femaleperc, na.rm = TRUE))

#summarise(peru, mean(avgloanbal), sd(avgloanbal), mean(femaleperc, na.rm = TRUE), 
#          sd(femaleperc, na.rm = TRUE))

# Histogram

hist(peru$avgloanbal, xlab = 'Average loan balance')

# Scatter plot

plot(selfsuff ~ I(avgloanbal / 10000), data = peru, ylim = c(0.7, 1.8))
abline(lm(selfsuff ~ I(avgloanbal / 10000), data = peru))

# Regression

lm1 <- lm(selfsuff ~ I(avgloanbal / 10000) + femaleperc, data = peru)
summary(lm1)

# avgloanbal is statistically significant
# 