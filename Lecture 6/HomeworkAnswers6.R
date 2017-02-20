library(lme4)
library(arm)
library(plm)
library(effects)
library(ggplot2)

data(Males)
str(Males)
summary(Males)
View(Males)

ggplot(Males, aes(y = exp(wage))) + geom_boxplot(aes(x=occupation)) + coord_flip()


p1 <- plm(wage ~  exper + married, data = Males, index = c('nr', 'year'))
summary(p1)

p2 <- plm(wage ~  married, data = Males, index = c('nr', 'year'), effect = 'twoways', model = 'random')
summary(p2)
exp(.061)

l1 <- lmer(wage ~ married + school + union + exper*married + (1+exper|nr) + (1|year), data = Males )
summary(l1)

#plot(Effect(c('exper','married'), l1, se=FALSE), confint=FALSE)
# Very slow!
# So do it 'by hand' using predict

plot(0:18, predict(l1, newdata=data.frame(married='yes', school=11.8, union='no', exper=0:18, nr=5262, year=1984), 
                   allow.new.levels=TRUE), type='l', ylab='Log wage', xlab='Experience', ylim=c(1,2.5))
lines(0:18, predict(l1, newdata=data.frame(married='no', school=11.8, union='no', exper=0:18, nr=5262, year=1984), 
                   allow.new.levels=TRUE), type='l', col='red')
legend('topleft', col=c('black','red'), lty=1, legend=c('Yes','No'), title='Married')


Males.com <- Males[complete.cases(Males), ]
l1 <- lm(wage ~ married, data = Males.com)
s1 <- step(l1, scope = ~ married + school + exper + union + ethn + industry + occupation + residence, direction='forward')

s1
summary(s1)

pfinal <- plm(wage ~ married + school + exper + union + ethn + industry + occupation + residence, data = Males, index = c('nr', 'year'), model = 'random')
summary(pfinal)
