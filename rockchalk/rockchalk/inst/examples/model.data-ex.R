library(rockchalk)

set.seed(12345)
x1 <- rpois(100, l=6)
x2 <- rnorm(100, m=50, s=10)
x3 <- rnorm(100)
xcat1 <- gl(2,50, labels=c("M","F"))
xcat2 <- cut(rnorm(100), breaks=c(-Inf, 0, 0.4, 0.9, 1, Inf), labels=c("R", "M", "D", "P", "G"))
dat <- data.frame(x1, x2, x3, xcat1, xcat2)
rm(x1, x2, x3, xcat1, xcat2)
xcat1n <- with(dat, contrasts(xcat1)[xcat1, ,drop=FALSE])
xcat2n <- with(dat, contrasts(xcat2)[xcat2, ])

STDE <- 20
dat$y <- 0.03 + 0.8*dat$x1 + 0.1*dat$x2 + 0.7*dat$x3 + xcat1n %*% c(2) + xcat2n %*% c(0.1,-2,0.3, 0.1) + STDE*rnorm(100)
rownames(dat$y) <- NULL
## rownames don't match dat, I've not seen that problem before

## Will fail because m0 has no data argument
## m0 <- lm(y ~ log(10+x1) + x2)
## m0.data <- model.data(m0) # should fail

m1 <- lm(y ~ poly(x1, 2), data=dat)
m1.data <- model.data(m1)
summarize(m1.data)
attr(m1.data, "ivnames")

## Check to make sure d is not mistaken for a data column
d <- 2
m2 <- lm(y ~ poly(x1, d), data=dat)
m2.data <- model.data(m2)
summarize(m2.data)
attr(m2.data, "ivnames")


## Check to see how the 10 in log is handled
m3 <- lm(y ~ log(10 + x1) + poly(x1, d) + sin(x2), data=dat)
m3.data <- model.data(m3)
summarize(m3.data)
attr(m3.data, "ivnames")


m4 <- lm(log(50+y) ~ log(d+10+x1) + poly(x1, 2), data=dat)
m4.data <- model.data(m4)
summarize(m4.data)
attr(m4.data, "ivnames")


m4 <- lm(y ~ x1*x1, data=dat)
m4.data <- model.data(m4)
summarize(m4.data)
attr(m4.data, "ivnames")


m4 <- lm(y ~ x1 + I(x1^2), data=dat)
m4.data <- model.data(m4)
summarize(m4.data)
attr(m4.data, "ivnames")

## Put in some missings.
## poly doesn't work if there are missings, but
## can test with log
dat$x1[sample(100, 5)] <- NA
dat$y[sample(100, 5)] <- NA
dat$x2[sample(100, 5)] <- NA
dat$x3[sample(100,10)] <- NA


m1 <- lm(y ~ log(10 + x1), data=dat)
m1.data <- model.data(m1)
summarize(m1.data)
attr(m1.data, "ivnames")



m2 <- lm(y ~ log(x1 + 10), data=dat)
m1.data <- model.data(m1)
summarize(m1.data)
attr(m1.data, "ivnames")

d <- 2
m3 <- lm(log(50+y) ~ log(d+10+x1) + x2 + sin(x3), data=dat)
m3.data <- model.data(m3)
summarize(m3.data)
attr(m3.data, "ivnames")


m4 <- lm(y ~ x1^3 + log(x2), data=dat)
m4.data <- model.data(m4)
summarize(m4.data)
attr(m4.data, "ivnames")


m5 <- lm(y ~ x1 + I(x1^2) + cos(x2), data=dat)
m5.data <- model.data(m4)
summarize(m5.data)
attr(m5.data, "ivnames")

