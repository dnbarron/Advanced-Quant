library(lme4)
library(arm)
library(effects)
library(readr)

scot <- read_csv(file = "C:\\Users\\dbarron\\Dropbox\\Advanced Quant\\5.1.txt")

# caseid: student id
# schoolid: School id
# score: Point score calculated from awards in Standard grades taken at age 16. Scores range from 0 to 75, with a higher score indicating a higher attainment
# cohort90: The sample includes the following cohorts: 1984, 1986, 1988, 1990, 1996 and 1998. The cohort90 variable is calculated by subtracting 1990 from each value. Thus values range from -6 (corresponding to 1984) to 8 (1998), with 1990 coded as zero 
# female:  Sex of student (1 = female, 0 = male) 
# sclass: Social class, defined as the higher class of mother or father
#   (1 = managerial and professional, 2 = intermediate, 3 = working, 4 = unclassified) 
# schtype: School type, distinguishing independent schools from state-funded schools 
#   (1 = independent, 0 = state-funded) 
# schurban: Urban-rural classification of school (1 = urban, 0 = town or rural) 
# schdenom: School denomination (1 = Roman Catholic, 0 = non-denominational)

scot$female <- factor(scot$female, labels = c('male', 'female'))
scot$sclass <- factor(scot$sclass, labels = c('professional','intermediate','working','unclassified'))
scot$schtype <- factor(scot$schtype, labels = c('state', 'independent'))
scot$schurban <- factor(scot$schurban, labels = c('rural', 'urban'))
scot$schdenom <- factor(scot$schdenom, labels = c('non-denom', 'RC'))

base <- lm(score ~ cohort90 + female + sclass, data = scot)
summary(base)

m1 <- lmer(score ~ 1|schoolid, data = scot)
summary(m1)

m2 <- lmer(score ~ cohort90 + female + sclass + (1|schoolid), data = scot)
summary(m2)

m3 <- lmer(score ~ cohort90 + female + sclass*schtype + (1 + sclass|schoolid), data = scot)
display(m3)

library(effects)
plot(Effect(c('sclass','schtype'), m3))

m4 <- lmer(score ~ cohort90 + female + sclass*schtype + (1 + sclass|schoolid), data = scot)
display(m4)

m5 <- lmer(score ~ cohort90 + female + schdenom + sclass + schtype + (1|schoolid) , data = scot)
display(m5)
plot(Effect(c('female','schdenom'), m5))

lattice::dotplot(coef(m4))
