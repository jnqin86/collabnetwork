# Author Credit: Dr. Jeff Hemsley, Syracuse University (2018).

options(scipen = 999)
# The second diameter is actually 'mean distance"

dat.dir <- "C:/Users/User/Dropbox/_._Publications/Journal Papers/GenBank/data/"

fname.regression.vars <- "regressionVars_v2.csv"

vars <- read.csv(paste0(dat.dir, fname.regression.vars), quote="\"", header = TRUE, sep=",", stringsAsFactors=F)
View(vars)
colnames(vars)

my.cols <- c("pat.num", "pat.auth.median", "sub.net.vcount", "sub.net.ecount"
             , "sub.net.density", "sub.net.diameter", "sub.net.mean.dist"
             , "sub.net.transitivity", "sub.net.centr_degree", "sub.net.centr_closeness")

library(PerformanceAnalytics)
chart.Correlation(vars[ , my.cols])

dummy <- rep(1, dim(vars)[1])
dummy[1:14] <- 0

plot(vars$pat.num)
plot(vars$sub.net.centr_closeness[-zap])
plot(vars$pat.num[-21], vars$sub.net.vcount[-21])
plot(vars$pat.num, vars$sub.net.vcount)

zap <- c(1,2,3,21)

plot(log10(vars$pat.num[-zap]), log10(vars$sub.net.density[-zap]))

plot(log10(vars$sub.net.density), type = "l")
plot(log10(vars$pat.num[-zap]), log10(vars$sub.net.centr_closeness[-zap]))

plot(log10(vars$pat.num[-zap]))
plot(log10(vars$pat.num[-zap]), log(vars$sub.net.centr_closeness[-zap]))

fit <- lm(log10(vars$pat.num[-zap]) ~ 
            vars$sub.net.vcount[-zap]
          # + vars$pat.auth.mean[-zap]
          + vars$mean_auths[-zap]
          # + log10(vars$sub.net.density[-zap])
          #+ vars$sub.net.transitivity[-zap]
          + log10(vars$sub.net.centr_closeness[-zap])
          #+ vars$sub.net.mean.dist[-zap]
            #vars$sub.net.diameter[-zap]
          #+ dummy[-zap]
          # + vars$sub.net.mean.dist
          #+ vars$pat.auth.mean[-21]
          )
fit
summary(fit)
par(mfrow =c(2,2))
plot(fit)

library(faraway)
vif(fit)

#plot(fit$residuals)


hist(vars$pat.num)
qqnorm(vars$pat.num)
qqnorm(sqrt(vars$pat.num))
qqnorm(log(vars$pat.num))

qqnorm(fit$residuals)
qqline(fit$residuals)
# The null-hypothesis of this test is that the population is normally distributed.
shapiro.test(sqrt(vars$pat.num))

library(MASS)
fit <- rlm(vars$pat.num ~ vars$sub.net.vcount)
summary(fit)
