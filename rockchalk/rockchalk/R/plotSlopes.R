##' Assists creation of predicted value lines for values of a moderator variable.
##'
##' This is a "simple slope" plotter, especially for regressions that
##' include interactive variables.  This version works with factor or
##' numeric moderator.  It insists that the plotx variable must be
##' numeric.  It only prints out the t tests for "simple slopes"  if
##' user gives a numeric moderator. Otherwise, user can simply read
##' the t tests from the regression output itself. I may re-think that
##' (stubborn) stance.  Here is the wrinkle. How to calculate
##' predicted values in regressions that include categorical
##' variables? Obviously, numeric variables can be set at observed
##' means. This version sets categorical (factor) variables at mode value (most
##' frequently observed value).

##' @param model Fitted regression object. Must have a predict method
##' @param plotx String with name of IV to be plotted on x axis
##' @param modx String for moderator variable name. May be either numeric or factor.
##' @param modxVals A vector of numeric values for which plotted lines
##' are sought.  This is intended for numeric variables referred to by
##' "modx". These are user specified "for instance" values. If
##' omitted, quantiles will be used for numeric variables. If "modx"
##' is a factor, this parameter is ignored and a predictive line is
##' drawn for each level of the factor. That is a shortcoming in the
##' current implementation.
##' @param plotPoints Should the plot include the scatterplot points along with the lines.
##' @param ... further arguments that are passed to plot
##' @export
##' @import car
##' @return If modx is the name of a numeric variable, will return a
##' table of estimates and hypothesis tests for the simple slopes.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @examples
##' set.seed(12345)
##' x1 <- rnorm(100)
##' x2 <- rnorm(100)
##' x3 <- rnorm(100)
##' x4 <- rnorm(100)
##' y <- rnorm(100)
##' y2 <- 0.03 + 0.1*x1 + 0.1*x2 + 0.25*x1*x2 + 0.4*x3 -0.1*x4 + 1*rnorm(100)
##' dat <- data.frame(x1,x2,x3,x4,y, y2)
##' rm(x1, x2, x3, x4, y, y2)
##' 
##' ##ordinary regression
##' m1 <- lm(y ~ x1 + x2 +x3 + x4, data=dat)
##' ## must specify depvar parameter
##' plotSlopes(m1, plotx="x1", modx="x2", modxVals=c(-0.5,0,0.5))
##' plotSlopes(m1, plotx="x1", modx="x2")
##' plotSlopes(m1, plotx="x4", modx="x1")
##' 
##' m2 <- lm(y2 ~ x1*x2 + x3 +x4, data=dat)
##' summary(m2)
##' plotSlopes(m2, plotx="x1", modx="x2")
##' 
##' plotSlopes(m2, plotx="x1", modx="x2", modxVals=c( -2, -1, 0, 1, 2))
##' 
##' plotSlopes(m2, plotx="x3", modx="x2")
##' 
##' m3 <- lm(y2 ~ x1 * x2 + x3, data=dat)
##' plotSlopes(m3, plotx="x3", modx="x2")
##' plotSlopes(m3, plotx="x1", modx="x2")
##' plotSlopes(m3, plotx="x2", modx="x3")
##'
##' 
##' ### Examples with categorical Moderator variable
##' 
##' xcontinuous <- rnorm(100)
##' xcategorical <- gl(2,50, labels=c("Gigantic","Humongous"))
##' stde <- 8
##' y <- 3 + 0.5*xcontinuous + 1.2 * (as.numeric(xcategorical)-1) +
##'   -0.8* (as.numeric(xcategorical)-1) * xcontinuous +  stde * rnorm(100)
##' 
##' m1 <- lm (y ~ xcontinuous + xcategorical)
##' summary(m1)
##' 
##' plotSlopes(m1, modx = "xcategorical", plotx = "xcontinuous")
##' 
##' m2 <- lm (y ~ xcontinuous * xcategorical)
##' summary(m2)
##' plotSlopes(m2, modx = "xcategorical", plotx = "xcontinuous")
##' 
##' 
##' library(car)
##' m3 <- lm(statusquo ~ income * sex, data = Chile)
##' summary(m3)
##' plotSlopes(m3, modx = "sex", plotx = "income")
##' 
##' 
##' m4 <- lm(statusquo ~ region * income, data= Chile)
##' summary(m4)
##' plotSlopes(m4, modx = "region", plotx = "income")
##' 
##' plotSlopes(m4, modx = "region", plotx = "income", plotPoints=FALSE)
##' 
##' 
##' m5 <- lm(statusquo ~ region * income + sex + age, data= Chile)
##' summary(m5)
##' plotSlopes(m5, modx = "region", plotx = "income")
##' 
##' m6 <- lm(statusquo ~ income * age + education + sex + age, data=Chile)
##' summary(m6)
##' plotSlopes(m6, modx = "income", plotx = "age")
##' 
##' plotSlopes(m6, modx = "income", plotx = "age", plotPoints=FALSE)
##' 
##' 
##' ## Should cause error because education is not numeric
##' ## m7 <- lm(statusquo ~ income * age + education + sex + age, data=Chile)
##' ## summary(m7)
##' ## plotSlopes(m7, modx = "income", plotx = "education")
##' 
##' ## Should cause error because "as.numeric(education") not same as
##' ## plotx="education"
##' ## m8 <- lm(statusquo ~ income * age + as.numeric(education) + sex + age, data=Chile)
##' ## summary(m8)
##' ## plotSlopes(m8, modx = "income", plotx = "education")
##' 
##' ## Still fails. 
##' ## plotSlopes(m8, modx = "income", plotx = "as.numeric(education)")
##' 
##' ## Must recode variable first so that variable name is coherent
##' Chile$educationn <- as.numeric(Chile$education)
##' m9 <- lm(statusquo ~ income * age + educationn + sex + age, data=Chile)
##' summary(m9)
##' plotSlopes(m9, modx = "income", plotx = "educationn")

plotSlopes <-
  function (model = NULL, plotx = NULL, modx = NULL, modxVals = NULL, 
            plotPoints = TRUE, ...) 
{
  if (is.null(model)) 
    stop("plotSlopes requires a fitted regression model.")
  if (is.null(plotx)) 
    stop("plotSlopes requires the name of the variable to be drawn on the x axis")
  if (is.null(modx)) 
    stop("plotSlopes requires the name of moderator variable for which several slopes are to be drawn")
  mm <- model.matrix(model)
  depVar <- model$model[, 1]
  modxVar <- model$model[, modx]
  plotxVar <- model$model[, plotx]
  if (!is.numeric(plotxVar)) 
    stop(paste("plotSlopes: The variable", plotx, "should be a numeric variable"))
  ylab <- colnames(model$model)[1]
  plotyRange <- range(depVar)
  plotxRange <- range(mm[, plotx])
  plotxSeq <- plotSeq(plotxRange, l = 40)
  if (is.factor(modxVar)) {
    if (is.null(modxVals)) {
      modxVals <- levels(modxVar)
    }
    lmx <- length(modxVals)
  }
  else {
    modxRange <- range(mm[, modx])
    if (is.null(modxVals)) {
      modxVals <- quantile(mm[, modx])
    }
    lmx <- length(modxVals)
  }
  predictors <- colnames(model$model)[-1]
  predictors <- setdiff(predictors, c(modx, plotx))
  newdf <- data.frame(expand.grid(plotxRange, modxVals))
  colnames(newdf) <- c(plotx, modx)
  if (length(predictors) > 0) {
    newdf <- cbind(newdf, centralValues(as.data.frame(model$model[, predictors])))
    colnames(newdf) <- c(plotx, modx, predictors)
  }
  newdf$pred <- predict(model, newdata = newdf)
  dotargs <- list(...)
  if (!plotPoints){
    parms <- list(mm[, plotx], depVar, xlab = plotx, ylab = ylab, 
         type = "n")
    parms <- modifyList(parms, dotargs)
    do.call("plot", parms)
  } else {
    if (is.factor(modxVar)) {
      parms <- list(mm[, plotx], depVar, xlab = plotx, ylab = ylab, 
           col = modxVar)
      parms <- modifyList(parms, dotargs)
      do.call("plot", parms)
    }
    else {
      parms <- list(mm[, plotx], depVar, xlab = plotx, ylab = ylab)
      parms <- modifyList(parms, dotargs)
      do.call("plot", parms)
    }
  }
  for (i in 1:lmx) {
    pdat <- newdf[newdf[, modx] %in% modxVals[i], ]
    lines(pdat[, plotx], pdat$pred, lty = i, col = i, lwd = 2)
  }
  if (is.null(names(modxVals))) {
    legnd <- paste(modx, " = ", modxVals, sep = "")
  }
  else {
    legnd <- paste(modx, " = ", names(modxVals), sep = "")
  }
  legend("topleft", legend = legnd, lty = 1:lmx, col = 1:lmx, 
         bg = "white")
  if (!is.factor(modxVar)) {
    ivs <- attr(terms(model), "term.labels")
    bs <- coef(model)
    V <- vcov(model)
    relevantInteractions <- c(paste(plotx, ":", modx, sep = ""), 
                              paste(modx, ":", plotx, sep = ""))
    bmodx <- NULL
    bplotx <- bs[plotx]
    if (any(relevantInteractions %in% ivs)) {
      interactionsIn <- relevantInteractions[which(relevantInteractions %in% 
                                                   ivs)]
      bmodx <- bs[interactionsIn]
      bsimple <- bplotx + bmodx * modxVals
      covbsimple <- cbind(1, modxVals^2, 2 * modxVals) %*% 
        c(V[plotx, plotx], V[names(bmodx), names(bmodx)], 
          V[plotx, names(bmodx)])
      tbsimple <- bsimple/sqrt(covbsimple)
    }
    else {
      bmodx <- 0
      bsimple <- rep(bplotx, length(modxVals))
      covbsimple <- vcov(model)[plotx, plotx]
      tbsimple <- bsimple/sqrt(covbsimple)
    }
    data.frame(modx = modxVals, b = bsimple, se = sqrt(covbsimple), 
               t = tbsimple, p = 2 * pt(abs(tbsimple), df = model$df.residual, 
                               lower.tail = FALSE))
  }
}
