library(tidyverse)

setwd("~/Teaching/MSc teaching/Advanced Quant")
options('show.signif.stars'=FALSE)


# Maximum likelihood explanation ------------------------------------------

# dat <- tribble(~y, ~x,
#                0,  1.5,
#                1,  5.9,
#                0, 0.4,
#                1, 5.7)

set.seed(123)
y <- sample(c(0, 1), 100, replace = TRUE)
x <- rnorm(100) + 1.5 * y
  
plot(x, y)

b0 <- -1.02

b1 <- 1.32

eta <- b0 + b1 * x

phat <- 1 / (1 + exp(-eta))

like <- phat^y * (1 - phat)^(1 - y)

loglik <- -sum(log(like))
loglik


eg1 <- glm(y ~ x, family = binomial)
summary(eg1)
logLik(eg1)

egnull <- glm(y ~ 1, family = binomial)
-2 * logLik(egnull)

logreg <- function(b) {
  eta <- b[1] + b[2] * x 
  phat <- 1 / (1 + exp(-eta))
  -sum(y * log(phat) + (1 - y) * log(1 - phat))
}

optim(c(.1,.1), logreg)


b1 <- seq(0, 3, by = 0.01)

ll <- numeric(length = length(b1))
for (i in seq_along(b1)){
  ll[i] <- logreg(c(b0, b1[i]))
}

ll
plot(b1, ll, type = 'l')

b0 <- seq(-3, 0, by = 0.01)
ll <- matrix(nrow = length(b0), ncol = length(b1))

for (i in seq_along(b0)){
  for (j in seq_along(b1)){
    ll[i, j] <- logreg(c(b0[i], b1[j]))
  }
}

ll

contour(b0, b1, ll)
persp(b0, b1, ll)
