set.seed(12345)
N <- 500
x1 <- rnorm(N, m=5, s=1)
x2 <- rnorm(N)
x3 <- rnorm(N)
x4 <- rnorm(N)
xcat1 <- gl(2,50, labels=c("M","F"))
xcat2 <- cut(rnorm(N), breaks=c(-Inf, 0, 0.4, 0.9, 1, Inf), labels=c("R", "M", "D", "P", "G"))
dat <- data.frame(x1, x2, x3, x4, xcat1, xcat2)
rm(x1, x2, x3, x4, xcat1, xcat2)

###The design matrix for categorical variables, xcat numeric
dat$xcat1n <- with(dat, contrasts(xcat1)[xcat1, ])
dat$xcat2n <- with(dat, contrasts(xcat2)[xcat2, ])


stde <- 2
dat$y <- with(dat, 0.03 + 11.5*log(x1)*xcat1n + 0.1*x2 + 0.04*x2^2 + stde*rnorm(N))

stde <- 1              
dat$y2 <- with(dat, 0.03 + 0.1*x1 + 0.1*x2 + 0.25*x1*x2 + 0.4*x3 -0.1*x4 + stde*rnorm(N))
stde <- 8
dat$y3 <- with(dat, 3 + 0.5*x1 + 1.2 * (as.numeric(xcat1)-1) +
-0.8* (as.numeric(xcat1)-1) * x1 +  stde * rnorm(N))

stde <- 8

dat$y4 <- with(dat, 3 + 0.5*x1 + xcat2n %*% c(0.1, -0.2, 0.3, 0.05)  + stde * rnorm(N))




##ordinary regression
m1 <- lm(y ~ log(x1)*xcat1 + x2 + I(x2^2), data=dat)
summary(m1)
plotCurves(m1, plotx="x1", modx="xcat1")

plotCurves(m1, plotx="x2", modx="x1")
##OK

plotCurves(m1, plotx="x2", modx="xcat1")
##error

m1 <- lm(y ~ log(x1)*xcat1 + xcat1*(x2 + I(x2^2)), data=dat)
summary(m1)
plotCurves(m1, plotx="x2", modx="xcat1")
##error. 

plotCurves(m1, plotx="x2", modx="x1")
##OK

plotCurves(m1, plotx="x2", modx="x1")
##OK



m2 <- lm(y ~ poly(x2,2) + xcat1, data=dat)
plotCurves(m2, plotx="x2", modx="xcat1")
#OK



m2 <- lm(y ~ x2 + I(x2^2) + xcat1, data=dat)
plotCurves(m2, plotx="x2", modx="xcat1")
#OK

m3 <- lm(log(y+10) ~ poly(x2, 2)*xcat1 + x1, data=dat)
summary(m3)
plotCurves(m3, plotx="x2", modx="xcat1")
#OK
plotCurves(m3, plotx="x2", modx="x1")
#OK




m2 <- lm(y2 ~ x1*x2 + x3 +x4, data=dat)
summary(m2)
plotCurves(m2, plotx="x1", modx="x2")

plotCurves(m2, plotx="x1", modx="x2", modxVals=c( -2, -1, 0, 1, 2))

plotCurves(m2, plotx="x3", modx="x2")

m3 <- lm(y2 ~ x1 * x2 + x3, data=dat)
plotCurves(m3, plotx="x3", modx="x2")
plotCurves(m3, plotx="x1", modx="x2")
plotCurves(m3, plotx="x2", modx="x3")


### Examples with categorical Moderator variable

d1 <- data.frame(xcontinuous= rnorm(N))
d1$xcategorical <- gl(2,50, labels=c("Gigantic","Humongous"))
stde <- 8
y <- 3 + 0.5*xcontinuous + 1.2 * (as.numeric(xcategorical)-1) +
-0.8* (as.numeric(xcategorical)-1) * xcontinuous +  stde * rnorm(N)

m1 <- lm (y ~ xcontinuous*xcategorical, data=d1)
summary(m1)

plotCurves(m1, modx = "xcategorical", plotx = "xcontinuous")



m2 <- lm (y ~ xcontinuous * xcategorical)
summary(m2)
plotCurves(m2, modx = "xcategorical", plotx = "xcontinuous")


library(car)
m3 <- lm(statusquo ~ income * sex, data = Chile)
summary(m3)
plotCurves(m3, modx = "sex", plotx = "income")


m4 <- lm(statusquo ~ region * income, data= Chile)
summary(m4)
plotCurves(m4, modx = "region", plotx = "income")

plotCurves(m4, modx = "region", plotx = "income", plotPoints=FALSE)


m5 <- lm(statusquo ~ region * income + sex + age, data= Chile)
summary(m5)
plotCurves(m5, modx = "region", plotx = "income")

m6 <- lm(statusquo ~ income * age + education + sex + age, data=Chile)
summary(m6)
plotCurves(m6, modx = "income", plotx = "age")

plotCurves(m6, modx = "income", plotx = "age", plotPoints=FALSE)


## Should cause error because education is not numeric
## m7 <- lm(statusquo ~ income * age + education + sex + age, data=Chile)
## summary(m7)
## plotCurves(m7, modx = "income", plotx = "education")

## Should cause error because "as.numeric(education") not same as
## plotx="education"
## m8 <- lm(statusquo ~ income * age + as.numeric(education) + sex + age, data=Chile)
## summary(m8)
## plotCurves(m8, modx = "income", plotx = "education")

## Still fails. 
## plotCurves(m8, modx = "income", plotx = "as.numeric(education)")

## Must recode variable first so that variable name is coherent
Chile$educationn <- as.numeric(Chile$education)
m9 <- lm(statusquo ~ income * age + educationn + sex + age, data=Chile)
summary(m9)
plotCurves(m9, modx = "income", plotx = "educationn")
