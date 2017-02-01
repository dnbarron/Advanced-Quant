#########################
## Further topics in regression
## David Barron
## Hilary Term 2017
##################################

options('show.signif.stars' = FALSE)

setwd("C:\\Users\\dbarron\\Dropbox\\Teaching\\MSc teaching\\Advanced Quant")

library(car)
library(foreign)
library(effects)
library(ggplot2)

# Duncan occupational prestige data
data(Duncan)
head(Duncan)

# Histograms 
g <- ggplot(Duncan) + theme_light()
g + geom_histogram(aes(x = prestige))
g + geom_histogram(aes(x = income))
g + geom_histogram(aes(x = education))

#hist(Duncan$prestige)
#hist(Duncan$income)
#hist(Duncan$education)

# Scatter plot
g <- ggplot(Duncan, aes(y = prestige)) + theme_light()

g + geom_point(aes(x = income))
g + geom_point(aes(x = education))

#plot(prestige ~ income, data = Duncan)
#plot(prestige ~ education, data = Duncan)

# Regression
duncan.model <- lm(prestige ~ income + education, data = Duncan)
summary(duncan.model)

## Dummy variable

# Read previous generated random data, categorical variable with four categories, one continuous variable.
dta <- read.csv('dummyvars.csv')

xfac <- ifelse(dta$x1==1,'A',
               ifelse(dta$x2==1,'B',
                      ifelse(dta$x3==1,'C','D')))
dta$xfac <- xfac

head(dta,25)
tail(dta)
View(dta)
# Estimate using only the continuous variable
cont <- lm(y ~ xc, data=dta)
summary(cont)

## Two ways of getting estimates of categorical variable

xc1 <- lm(y ~ x1 + x2 + x3 + x4 + xc, data=dta)
summary(xc1)

anova(cont, xc1)

xf1 <- lm(y ~ xfac + xc, data=dta)
summary(xf1)

# With different variable excluded
xc2 <- lm(y ~ x1 + x2 + x3 + xc, data=dta)
summary(xc2)

xf2 <- lm(y ~ relevel(dta$xfac,'D') + xc, dta)
summary(xf2)

# Example using Duncan data

xtabs(~type, data = Duncan)

duncan.model.2 <- lm(prestige ~ income + education + type, data = Duncan)
summary(duncan.model.2)

duncan.model.2a <- lm(prestige ~ income + education + relevel(type,'prof'), data = Duncan)
summary(duncan.model.2a)

anova(duncan.model, duncan.model.2)

# This shows that a dummy variable implies parallel regression lines

effect('type', duncan.model.2)
plot(Effect(c('education','type'), duncan.model.2), multiline=TRUE)

#############################################################

## Interactions


# Canadian prestige data, similar to Duncan data

data(Prestige)
summary(Prestige)
pres.model <- lm(prestige ~ income + education + type , data = Prestige)
summary(pres.model)
pres.model.2 <- update(pres.model, . ~ . + income* education)
summary(pres.model.2)

plot(Effect(c('income','education'), pres.model.2 ))

# Use the Labour Force Survey 2002 (an extract for teaching purposes)

lfs <- read.dta('data/lfs2002.dta')
summary(lfs)
lfs$Loghourpay <- log(lfs$hourpay)
lfs$Loghourpay[is.nan(lfs$Loghourpay)] <- NA
lfs$hourpay[lfs$hourpay <= 0] <- NA

lfs$edage[lfs$edage < 0] <- NA
lfs$allchildren <- with(lfs, numchild + numchil1)

lfs1 <- lm(Loghourpay ~ sex + age + allchildren + married, data=lfs)
summary(lfs1)

lfs2 <- update(lfs1, . ~ . + sex*married + sex*allchildren) 
summary(lfs2)

lfs.ef <- allEffects(lfs2)
plot(lfs.ef)

lfs.ef[[2]]

lfs.kids <- Effect(c('sex','allchildren'), lfs2)
plot(lfs.kids, xlab='Children under 16', ylab='Log hourly pay (GBP)', rug=TRUE)

lfs2.plot <- sjp.int(lfs2, type = 'eff')

## Normality

# Look at hourly pay
hist(lfs$hourpay)
hist(lfs$Loghourpay)
lfs2.nolog <- lm(hourpay ~ sex*married + sex*allchildren + age, data=lfs)
summary(lfs2.nolog)
summary(lfs2)

nolog.resid <- residuals(lfs2.nolog)
log.resid <- residuals(lfs2)
plot(density(nolog.resid), xlim=c(-10,50), ,ylim=c(0,.25), main='Density plot')
mn.res <- mean(nolog.resid)
sd.res <- sd(nolog.resid)

lines(-10:50,dnorm(-10:50,mn.res,sd.res), col='red')

plot(density(log.resid), xlim=c(-2,2))
mn.logres <- mean(log.resid)
sd.logres <- sd(log.resid)
x <- seq(-2,2,by=.1)
lines(x,dnorm(x,mn.logres,sd.logres), col='red')


qqPlot(nolog.resid, ylab='Residuals')
qqPlot(log.resid, ylab='Residuals')


## Collinearity

lfs2.vif <- vif(lfs2)
lfs2.vif

## Cp

Cp <- function(mod1, mod2){
  
  if (mod2$rank < mod1$rank){
    warning('Model 2 has fewer parameters than model 1; swapping models')
    tmp <- mod1
    mod1 <- mod2
    mod2 <- tmp
  }
  sse <- deviance(mod1)
  sigma <- summary(mod2)$sigma^2
  p <- mod1$rank
  n <- nobs(mod1)
  if (n != nobs(mod2)) {
    warning('Different number of observations')
  }
  return((sse/sigma) + (2 * p) - n)
}

find.eg <- function(seed = 1234, max.iter = 5, n = 30){
  i <- 1
  while (i <= max.iter){
    set.seed(seed)
    n <- 30
    x1 <- rnorm(n, 2, 2)
    x2 <- 2 + 0.8 * x1 + rnorm(n, 0, 1)
    
    y <- 4 + 0.5 * x1 - 1.3 * x2 + rnorm(n, 0, 1)
    l1 <- lm(y ~ x1 + x2)
    l2 <- lm(y ~ x2)
    if ((coef(l1) / sqrt(diag(vcov(l1))))[2] < 2){
      return(list(seed, x1, x2, y))
    }
    seed <- seed + 1
    i <- i + 1
  }
}
find.eg(max.iter = 10)

set.seed(1239)
n <- 30
x1 <- rnorm(n, 2, 2)
x2 <- 2 + 0.8 * x1 + rnorm(n, 0, 1)

y <- 4 + 0.5 * x1 - 1.3 * x2 + rnorm(n, 0, 1)
summary(l1 <- lm(y ~ x1 + x2))
summary(l2 <- lm(y ~ x2))
Cp(l2, l1)

data(CreditCard, package='AER')
cc.full <- lm(reports ~ age + income + share + owner + dependents + months, data=CreditCard)

summary(cc.full)

cc1 <- lm(reports~share, CreditCard)
cc2 <- lm(reports~share+owner, CreditCard)
cc3<- lm(reports~share+owner+months, CreditCard)
cc.step <- step(cc.full, direction='back')
summary(cc1)
summary(cc2)
summary(cc3)
Cp(cc1,cc.full)
Cp(cc2,cc.full)
Cp(cc3,cc.full)

op <- step(cc1, scope = list(lower= ~ age, upper= ~ . + owner + share + months), scale = summary(cc.full)$sigma^2, direction='for')

## Outliers

# Hat values

weak <- read.table('Weakliem.txt', header=TRUE)
summary(weak)
# Data from World Values Survey, 1990s
# secpay: attitude towards two secretaries with the same jobs getting paid different amounts if one is better at his/her job. 1=Fair, 2=Not Fair. Variable is national average
# gini: measure of income inequality: 0=perfect equality, 1=perfect inequality
# gdp: per capita gdp in USD
# democracy: 1=experienced democratic rule for at least 10 years

weak.nondem <- weak[weak$democrat==0,]

wplot <- ggplot(weak.nondem, aes(y=secpay))
wplot + geom_point(aes(x=gdp)) + geom_smooth(aes(x=gdp), method='lm')
wplot + geom_point(aes(x=gini)) + geom_smooth(aes(x=gini),method='lm')

weak1 <- lm(secpay ~ gini + gdp, data=weak.nondem)
summary(weak1)
outlie <- c(7,26)
weak2 <- update(weak1,data=weak.nondem[-outlie,])
summary(weak2)

plot(secpay ~ gini, data=weak.nondem, xlab='Gini coefficient', ylab='Attitudes towards inequality', ylim=c(1,1.65))
abline(weak1)
abline(weak2, col='red', lty=2)
identify(weak.nondem$gini, weak.nondem$secpay, labels=rownames(weak.nondem))

hv <- hatvalues(weak1)
plot(hatvalues(weak1), main='Hat values',ylim=c(0,.35))
abline(h=c(2,3)*3/26, lty=2)
identify(1:26,hatvalues(weak1),labels=rownames(weak.nondem))

# Studentized residuals

epst <- rstudent(weak1)
epst
weak1.del1 <- lm(secpay ~ gini + gdp, data=weak.nondem[-26,])
summary(weak1.del1)
summary(weak1)
resid(weak1)[26]/(.104*sqrt(1-hv[26]))

# Alternative mean shift method
dum <- c(rep(0,25), 1)  # Slovakia is the last case
weak1.ms1 <- lm(secpay ~ gini + gdp + dum, data=weak.nondem)
summary(weak1.ms1)

plot(epst)

plot(epst, main='Studenized residuals',ylab='Residual')
abline(h=c(-2,2), lty=2)
identify(1:26,epst,labels=rownames(weak.nondem))

weak.ot <- car::outlierTest(weak1,.5)
# Or calculate them for yourself:
2*pt(epst,22,lower.tail=F)*26

## DFBETA

summary(weak1.del1)
summary(weak1)
b.full <- coef(weak1)
b.del1 <- coef(weak1.del1)

b.full - b.del1

weak.dfb <- dfbetas(weak1)
plot(weak.dfb[,2:3], main="DFBETAS")
abline(h=2/sqrt(26),v=2/sqrt(26),lty=2)
identify(weak.dfb[,2:3],labels=rownames(weak.nondem))

rstand <- rstandard(weak1)
rstand^2/3 * hv/(1-hv)
cd <- cooks.distance(weak1)
cd

plot(cd, main="Cook's distance")
abline(h=4/23, lty=2)
identify(1:26,cd,labels=rownames(weak.nondem))


ggplot(Duncan, aes(x = education, y = prestige, fill = type, colour = type)) + geom_point() +
  geom_smooth(method = 'lm', se = FALSE)
