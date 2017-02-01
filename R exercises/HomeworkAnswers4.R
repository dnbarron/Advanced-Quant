library(AER)
library(effects)
library(MASS)
data(NMES1988)

hist(NMES1988$visits)

## Or, if you prefer ggplot:

library(ggplot2)
ggplot(NMES1988, aes(x = visits)) + geom_histogram()

xtabs(~ visits, data = NMES1988)
summary(NMES1988)

base <- glm(visits ~ 1, data = NMES1988, family = poisson())

summary(base)

vis.step <- step(base, scope = ~ hospital + health + chronic + gender + school + insurance, 
                 direction='forward', trace = 1)
summary(vis.step)
# All those variables are statistically significant

## Test for overdispersion by doing negative binomial regression
vis.nb <- glm.nb(visits ~ hospital + health + chronic + gender + school + insurance, data = NMES1988)
summary(vis.nb)

# You can see that Theta is much more than twice its standard error.  Could do a likelihood ratio test.
# I'll illustrate how to write a function.

overdisp.test <- function(mod, alpha = 0.05){
  # mod is the result of running a glm.nb regression
  if (class(mod)[1] != 'negbin') stop('require model of class negbin\n')
  if (alpha < 0 | alpha > 1) stop('alpha must be in the range (0, 1)')
  # obtain Poissin regression results
  poisreg <- glm(formula = eval(mod$call$formula), data = eval(mod$call$data), family = poisson)
  llP <- logLik(poisreg)
  llNB <- logLik(mod)
  D <- 2 * (llNB - llP)
  cv <- qchisq(1 - (2 * alpha), df = 1)
  pval <- pchisq(D, df = 1, lower.tail = FALSE) / 2
  cat('Likelihood ratio test of H0: no overdispersion\n')
  cat('Test statistic: ', D, '\n')
  cat('Critical value of test statistic: ', cv, '\n')
  cat('p-value: ', pval, '\n')
  invisible(c(stat = D, critval = cv, pval = pval))
}

odt <- overdisp.test(vis.nb)

# THere is a similar function in the package pscl, called odTest
pscl::odTest(vis.nb)
# There is very clear evidence of overdispersion

summary(vis.nb)

# You can compare the Poisson & negbin results side by side like this:
compareCoefs(vis.step, vis.nb)

# Notice that all the standard errors are larger

# Interpretation
# Number of chronic conditions. This varies from 0 to 8.  You can do an effect plot 'by hand' like this:

plot(0:8, exp(1.03 + (0:8) * 0.175))
# Or using the effects package (which also adds means of other variables)

plot(Effect('chronic',  vis.nb))

# You can do them all at once like this:

plot(allEffects(vis.nb), type = 'response')
plot(Effect(c('gender','insurance'), vis.nb, multline = TRUE, type = 'response'))

# So, for example we can see that women make on average 0.6 more visits than mens

# Check for outliers
outlierTest(vis.nb)
influenceIndexPlot(vis.nb, id.n = 3)
residualPlot(vis.nb)
