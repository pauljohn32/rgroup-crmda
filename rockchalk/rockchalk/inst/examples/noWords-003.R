x <- c("A", "B", "C", "A", "A", "B", "B", "B", "C", 
    "C")
y <- rnorm(10)

xf1 <- factor(x)

attributes(xf1)
class(xf1)
levels(xf1)
summary(xf1)

table(xf1)

summary(mxf1 <- lm(y ~ xf1))

contrasts(xf1)


summary(mxf1.2 <- lm(y ~ -1 + xf1))

anova(mxf1, mxf1.2, test = "F")  ##digital noise

xf2 <- factor(x, levels = c("C", "B", "A"))
levels(xf2)

table(xf2, xf1)


summary(mxf2 <- lm(y ~ xf1))

anova(mxf2, mxf1, test = "F")


xf3 <- factor(x, levels = c("B", "A", "C"), labels = c("New Long Name For Level B", 
    "Name for Level A", "Original C is Called This Now"))

table(xf3, xf1)



rm(x, xf1, xf2, xf3, mxf1, mxf1.2, mxf2, y)




x <- c(19, 33, 18, 11, 21, 24, 44, 24, 16, 15)

xf1 <- factor(x)
levels(xf1)

xf1n <- as.numeric(xf1)
xf1n
table(xf1n)
summary(xf1n)



xRecovered <- as.numeric(levels(xf1))[xf1]

identical(x, xRecovered)
plot(x, xRecovered)


rm(x, xf1, xf1n, xRecovered)


y <- rnorm(100)

xf1 <- gl(4, 25)
levels(xf1)
table(xf1)  #boring


xf1 <- gl(4, 25, labels = c("whatever", "whichever", 
    "whenever", "whoever"))
attributes(xf1)
levels(xf1)
table(xf1)


xf2 <- sample(xf1, 100, replace = TRUE)
t1 <- table(xf2)
barplot(t1)

t1b <- prop.table(t1)
barplot(t1b, ylab = "You (not me) asked for proportions")

contrasts(xf2)
designMatrix <- contrasts(xf2)[as.numeric(xf2), ]

y1 <- designMatrix %*% c(0.2, 0.6, 0.9) + rnorm(100)

m1 <- lm(y1 ~ xf2)
summary(m1)
termplot(m1)
termplot(m1, se = TRUE, partial = TRUE)


y2 <- designMatrix %*% c(0.2, 0.6, 0.9) + 3 * rnorm(100)

m2 <- lm(y2 ~ xf2)
summary(m2)
termplot(m2)
termplot(m2, se = TRUE, partial = TRUE)



xf2o <- as.ordered(xf2)
m1o <- lm(y1 ~ xf2o)
summary(m1o)
m2o <- lm(y2 ~ xf2o)
summary(m2o)

op <- par(no.readonly = TRUE)
par(mfcol = c(2, 1))
termplot(m1, partial = TRUE, se = TRUE, ylab = "Fit with xf2")
termplot(m1o, partial = TRUE, se = TRUE, ylab = "Fit with xf2o")
par(op)


plot(fitted(m1), fitted(m1o), xlab = "Predicted by model fitted with xf2", 
    ylab = "Predicted by model fitted with as.ordered(xf2)", 
    main = "Predictions Identical. Models Same?")


xf3 <- relevel(xf2, "whoever")
m3 <- lm(y1 ~ xf3)
termplot(m3, partial = TRUE, se = TRUE, ylab = "Fit with xf3")
par(mfcol = c(2, 1))
termplot(m1, partial = TRUE, se = TRUE, ylab = "Fit with xf2")
termplot(m3, partial = TRUE, se = TRUE, ylab = "Fit with xf3")
par(op)
