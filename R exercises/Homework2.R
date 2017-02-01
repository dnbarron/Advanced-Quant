#######################################################
### Solutions to week 2 homework
################################################
library(car)
library(effects)
data(SLID)

## This gives information about the variables
help(SLID)

str(SLID)
## Shows two numeric variables (with wages having missing cases), one integer 
# variable and two factors

# First attempt

l1 <- lm(wages ~ education + age + sex + language, data=SLID)
summary(l1)

# Normality check
qqnorm(residuals(l1))
qqline(residuals(l1))

# Using car
qqPlot(l1)

## Doesn't look good!
hist(SLID$wages)

## Try log wages

l2 <- update(l1, log(wages) ~ .)
summary(l2)
qqnorm(residuals(l2))
qqline(residuals(l2))

qqPlot(l2)
## Much better!

# Check for non-constant variance
spreadLevelPlot(l2, age)## Doesn't look too bad

ncvTest(l2)  # But this is highly signficant

sqrt(diag(hccm(l2)))
summary(l2)
## These are very similar to regular standard errors, so I think this can be ignored

# Linearity
crPlot(l2, 'education')
crPlot(l2, 'age')

l3 <- update(l2, . ~ . + poly(age,2,raw=TRUE) - age)
summary(l3)
anova(l2, l3)

plot(effects::effect('age', l3))

##  Interactions

l4 <- update(l3, . ~ . + education*sex)
summary(l4)
anova(l3, l4)

plot(effect(c('education', 'sex'), l4), multiline = TRUE)

vif(l4)
