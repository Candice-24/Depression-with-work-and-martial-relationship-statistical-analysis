library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(broom)
library(GGally)

depress <- read.table("depression.dat",header = T)
head(depress)

reg_int <- lm(DEP~AGE+SEX+WP+MC+SEX*WP+SEX*MC,data = depress)
summary(reg_int)

plot(depress)
cor(depress)

plot(reg_int)

ggplot(data=reg_int, mapping = aes(sample=reg_int$residuals)) +
  geom_qq() +
  geom_qq_line()+
  ggtitle("Q-Q Plot(Normality)")

plot(reg_int$fitted.values, reg_int$residuals,main="residuals vs fitted")

plot(depress$AGE,reg_int$residuals,pch=19,main="residuals vs Age")
plot(depress$SEX,reg_int$residuals,pch=19,main="residuals vs SEX")
plot(depress$WP,reg_int$residuals,pch=19,main="residuals vs WP")
plot(depress$MC,reg_int$residuals,pch=19,main="residuals vs MC")

# response variable (DEP) transform: not good
depress <- mutate(depress, DEP_log = log(DEP))
reg.logdep_int <- lm(DEP_log~AGE+SEX+WP+MC+SEX*WP+SEX*MC,data = depress)
summary(reg.logdep_int)

plot(reg.logdep_int$fitted.values, reg.logdep_int$residuals,
     main="residuals vs fitted (after logging y)")

ggplot(data=reg.logdep_int, mapping = aes(sample=reg.logdep_int$residuals)) +
  geom_qq() +
  geom_qq_line()+
  ggtitle("Q-Q Plot(Normality) after logging y")

# explanatory variables (MC) transform: good
depress <- mutate(depress, MC_log = log(MC))
reg.logMC_int <- lm(DEP~AGE+SEX+WP+MC_log+SEX*WP+SEX*MC_log,data = depress)
summary(reg.logMC_int)

plot(depress$MC_log,reg.logMC_int$residuals,pch=19,
     main="residuals vs MC (after logging MC)")

ggplot(data=reg.logMC_int, mapping = aes(sample=reg.logMC_int$residuals)) +
  geom_qq() +
  geom_qq_line()+
  ggtitle("Q-Q Plot(Normality) after logging MC")

# F-test (Q.6)

anova(reg.logMC_int)

# testing interaction

reg <- lm(DEP~AGE+SEX+WP+MC_log, data= depress)
anova(reg,reg.logMC_int)

# Q7

laverage <- hatvalues(reg.logMC_int)
plot(laverage, type="h")
student <- rstudent(reg.logMC_int)
plot(student, type="h")
dfs <- dffits(reg.logMC_int)
plot(dfs, type="h")
cooksd <- cooks.distance(reg.logMC_int)
plot(cooksd, type="h")
fram <- data.frame(DEP=depress$DEP, fitted=reg.logMC_int$fitted.values, 
           residuals=reg.logMC_int$residuals, laverage, student, 
           dffits=dfs, cooksd)

# Exclude extreme values of no.1,2,10,16,20,22,23

reg.logMC_int1=lm(DEP~AGE+SEX+WP+MC_log+SEX*WP+SEX*MC_log, 
                   data= depress[-1,])
summary(reg.logMC_int1)

reg.logMC_int2=lm(DEP~AGE+SEX+WP+MC_log+SEX*WP+SEX*MC_log, 
                  data= depress[-2,])
summary(reg.logMC_int2)


reg.logMC_int10=lm(DEP~AGE+SEX+WP+MC_log+SEX*WP+SEX*MC_log, 
                   data= depress[-10,])
summary(reg.logMC_int10)

reg.logMC_int16=lm(DEP~AGE+SEX+WP+MC_log+SEX*WP+SEX*MC_log, 
                   data= depress[-16,])
summary(reg.logMC_int16)

reg.logMC_int20=lm(DEP~AGE+SEX+WP+MC_log+SEX*WP+SEX*MC_log, 
                   data= depress[-20,])
summary(reg.logMC_int20)

reg.logMC_int22=lm(DEP~AGE+SEX+WP+MC_log+SEX*WP+SEX*MC_log, 
                   data= depress[-22,])
summary(reg.logMC_int22)

reg.logMC_int23=lm(DEP~AGE+SEX+WP+MC_log+SEX*WP+SEX*MC_log, 
                   data= depress[-23,])
summary(reg.logMC_int23)


reg.logMC_int_ex=lm(DEP~AGE+SEX+WP+MC_log+SEX*WP+SEX*MC_log, 
                   data= depress[-c(1,2,10,16,20,22,23),])
summary(reg.logMC_int_ex)

# test interaction after excluding no.22

reg.22=lm(DEP~AGE+SEX+WP+MC_log,data= depress[-22,])
anova(reg.22, reg.logMC_int22)

# test interaction after excluding no.1.2.10.16.20.22.23

reg.ex=lm(DEP~AGE+SEX+WP+MC_log,data= depress[-c(1,2,10,16,20,22,23),])
anova(reg.ex, reg.logMC_int_ex)

# without interation

anova(reg)

# without interation after excluding no.22

anova(reg.22)

# without interation after excluding no.1.2.10.16.20.22.23

anova(reg.ex)

# Determined a final model

step(reg.logMC_int)

-41.721*log(1.5)
