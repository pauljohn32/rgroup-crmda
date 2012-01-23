## Paul Johnson
## pauljohn@ku.edu 2012-01-23
##
## One final R program to demonstrate my point that "centered variables"
## don't really make a difference in regressions with interaction.
## Centering does not help with multicollinearity, but I mean to say
## more than that. It does not help with regression interpretation,
## if one correctly understands what the parameter estimates and the
## predicted values mean in a regression with interaction.

## Here the idea is the following. The centered b's and se's "seem" different,
## but they are actually calculated on the EXACT SAME fitted plane. The
## difference is that the notcentered model has the y axis positioned at
## x1=0,x2=0, while in the centered model it is instead at
## x1=meanx1, x2=meanx2. The predicted values for any x1, x2 combination
## are EXACTLY the same with either model, as are the estimates of
## uncertainty (in the form of confidence intervals or standard errors).

## Thus it should be possible to take the estimates from the
## notcentered regression and calculate the slopes at x1=meanx1,
## x2=meanx2, and re-produce them.  AND I CAN!! This demonstrates
## that claim at the end.


library(rockchalk)
set.seed(222233)
dat3 <- genCorrelatedData(rho = .31, stde = 80, beta=c(0.1, 0.2, 0.3, -0.2))

nointeract <-  mcGraph3(dat3$x1, dat3$x2, dat3$y, theta=-10, interaction = FALSE)
summary(nointeract[[1]])

## First, fit the model without the interaction term.
##

## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 424.1485    45.8004   9.261 5.29e-15 ***
## x1           -8.9042     0.8499 -10.477  < 2e-16 ***
## x2           -9.2701     0.8039 -11.531  < 2e-16 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 77.77 on 97 degrees of freedom
## Multiple R-squared: 0.8011,	Adjusted R-squared: 0.797 
## F-statistic: 195.4 on 2 and 97 DF,  p-value: < 2.2e-16 
##
## Yeah, it is "significant"
##
## Add an interaction. Watch, it ruins everything!
##
noncentered <-  mcGraph3(dat3$x1, dat3$x2, dat3$y, theta=-10, interaction = TRUE)
summary(noncentered[[1]])

## Booh, the model's "no good" without centering


## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)   
## (Intercept) -74.45719  179.94096  -0.414  0.67995   
## x1            1.57257    3.75575   0.419  0.67636   
## x2            0.64618    3.55471   0.182  0.85614   
## x1:x2        -0.20526    0.07181  -2.859  0.00522 **
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 75.05 on 96 degrees of freedom
## Multiple R-squared: 0.8167,	Adjusted R-squared: 0.811 
## F-statistic: 142.6 on 3 and 96 DF,  p-value: < 2.2e-16 

## Mean center the variables

meanx1 <- mean(dat3$x1)
meanx2 <- mean(dat3$x2)

x1c <- dat3$x1 - mean(dat3$x1)
x2c <- dat3$x2 - mean(dat3$x2)


centered <-  mcGraph3(x1c, x2c, dat3$y, theta=-10, interaction = TRUE)
summary(centered[[1]])

## Hooray, centering "saved the day". 


## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -456.69849    8.01647 -56.970  < 2e-16 ***
## x1            -8.38383    0.84005  -9.980  < 2e-16 ***
## x2            -9.47969    0.77920 -12.166  < 2e-16 ***
## x1:x2         -0.20526    0.07181  -2.859  0.00522 ** 
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 75.05 on 96 degrees of freedom
## Multiple R-squared: 0.8167,	Adjusted R-squared: 0.811 
## F-statistic: 142.6 on 3 and 96 DF,  p-value: < 2.2e-16 


## Yeah! Mean centering saved the day! Woo Hoo! All your
## t values are big and your p values are small.

## Unfortunately, Its just a mirage. It is describing
## slope estimates at a DIFFERENT POINT on the same
## curving plane.

## First, please note the fitted models offer
## EXACTLY THE SAME predicted values! I don't mean "similar"
## I mean exactly. Look like this:

par(mfcol=c(1,2))

centered <-  mcGraph3(x1c, x2c, dat3$y, theta=-10, interaction = TRUE)
noncentered <-  mcGraph3(dat3$x1, dat3$x2, dat3$y, theta=-10, interaction = TRUE)

## Or just scatter the predicted values from the models

par(mfcol=c(1,1))

plot(fitted(centered[[1]]), fitted(noncentered[[1]]), xlab="predicted from uncentered x", ylab="predicted from centered x", main="(Not)Centered Predictions Identical")

## Second, consider the estimates of the slope at a particular point.
## We already know the estimate is identical, but what about the
## "standard error" of that estimate.

## Use the noncentered model to calculate slopes at mean

coef(noncentered[[1]])["x1"] + coef(noncentered[[1]])["x1:x2"] * meanx2
##       x1 
## -8.383827 
##

coef(noncentered[[1]])["x2"] + coef(noncentered[[1]])["x1:x2"] * meanx1

##      x2 
## -9.479689 
##

## Please note, those estimates of the slopes EXACTLY match the
## coefficient estimates reported by the centered model. That is to
## say, if you restrict your attention to a particular value of
## x1, the centered and noncentered models produce EXATCLY the same
## slope estimates.

## And the standard errors are the same as well.
## Reproduce the standard errors in centered model from noncentered model

V <- vcov(noncentered[[1]])

sqrt(V["x1","x1"] + meanx2^2 * V["x1:x2","x1:x2"] + 2 * meanx2 * V["x1","x1:x2"])

## [1] 0.8400474

## That's the SAME number reported in the Std.Err. column for the centered
## model.

sqrt(V["x2","x2"] + meanx1^2 * V["x1:x2","x1:x2"] + 2 * meanx1 * V["x2","x1:x2"])

##[1]  0.7791997
##
## Bingo, Fits exactly. The estimates of the centered model are reproduced
## exactly from the notcentered model once the correct co-ordinate
## translation is put in place. The EXACT same t ratios, etc.

## Centering has NO EFFECT whatsoever on multicollinearity. It does
## not affect predicted values, it does not affect our confidence
## in estimates of slopes or predicted values.

## In short, if you understand what a multiple regression with
## interaction "really does," it is impossible to reach any
## conclusion except the following: mean centering makes no
## difference at all in the estimation or interpretation of
## regression models with interaction effects.

## Mean centering only aids the interpretation if one is too
## lazy to understand the curvature of the estimated plane
## and work with the predicted values that are relevant
## to a problem.  It is silly to say "the intercept is
## more easily interpreted with mean centering" because
## the intercept is just the predicted value when the
## predictors are set at the mean.  It is not more
## easy to interpret the EXACT SAME NUMBER from one model
## or the other.
