####################
## Week 3 Homework answers
######################

library(AER)
data(ResumeNames)
help('ResumeNames')
str(ResumeNames)
xtabs(~call, data=ResumeNames)
summary(ResumeNames)

mod1 <- glm(call ~ ethnicity + gender + quality, data=ResumeNames, family=binomial)
summary(mod1)

mod2 <- update(mod1, . ~ . + military)
summary(mod2)

mod3 <- update(mod2, . ~ . + industry) 
summary(mod3)

anova(mod2, mod3, test='LRT')
anova(mod2, mod3)

library(effects)
plot(Effect('ethnicity', mod3))

stp <- step(mod1, scope = ~ ethnicity+gender+quality+military+industry+jobs+experience+honors)
summary(stp)
plot(Effect('ethnicity', stp))
