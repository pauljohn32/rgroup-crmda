## Paul E. Johnson <pauljohn@ku.edu>
## 2011-11-10

## Second Development version of a simple slope plotter.
## This version works with factor or numeric moderator.
## It insists that the plotx variable must be numeric.
## It only prints out the t tests for "simple slopes" 
## if user gives a numeric moderator. Otherwise, user can simply read
## the t tests from the regression output itself. I may re-think
## that (stubborn) stance.

## Added an option "plotPoints" so users can elect to see only
## lines, no points. (Suggested by Alex Schoemann)

## The substantive difference in this version is in the calculations
## of predicted values from regression. In simpleSlopes-1, I had set
## all variables at their means when calcuating predictions, except
## for modx and plotx, of course. That is OK if all variables are
## numeric, but not so meaningful if some are categorical.

## In simpleSlopes-01.R, I decided to set:
## Numeric variables at observed means
## Categorical variables at mode value (most frequently
## observed value).  This is setting the stage for some other
## improvements I would like to incorporate in a replacement for
## R's termplot function.



###Just a convenience function. Is in rockchalk package
plotSeq <- function(x, length.out=length(x)){
  xr <- range(x)
  pseq <- seq(xr[1], xr[2], length.out=length.out)
  pseq
}


## Takes a data frame and returns a new data frame with
## only 1 row, in which the mean or mode of the columns is reported.
centralValues <- function(x){
  if( !is.data.frame(x)) stop("represent: x must be a data frame!")
  nc <- NCOL(x)
  nams <- colnames(x)
  represents <- list()
  for (i in 1: nc) {
    represents[[nams[i]]] <- if (is.numeric(x[ ,i])){
      mean(x[ ,i], na.rm=T)
    } else  levels(x[,i]) [which.max(table(x[ ,i]))]
  }
  as.data.frame(represents)
}


## ssplot= simple slope plot
## ssplot a function that takes these arguments:
## model <- m1 # fitted regression object. must have a predict method
## plotx <- "x1" #string with name of IV to be plotted on x axis
## modx <- "x2" #string for moderator variable name
## modxVals <- c(1,2,3) #user specified "for instance" values. If
## omitted, quantiles will be used for numeric variables, and
## observed levels will be used for factor variables.

ssplot <- function(model=NULL, plotx=NULL, modx=NULL, modxVals=NULL, plotPoints=TRUE){
  if (is.null(model)) stop("ssplot requires a fitted regression model.")
  if (is.null(plotx)) stop("ssplot requires the name of the variable to be drawn on the x axis")
  if (is.null(modx)) stop("ssplot requires the name of moderator variable for which several slopes are to be drawn")
  ##grab model matrix
  mm <- model.matrix(model)
  depVar <- model$model[ ,1] #first column is DV
  modxVar <- model$model[ , modx]
  plotxVar <- model$model[ ,plotx]
  if(!is.numeric(plotxVar)) stop(paste("ssplot: The variable", plotx, "should be a numeric variable"))
  ylab <- colnames(model$model)[1]
  plotyRange <- range(depVar)
  plotxRange <- range(mm[ , plotx])
  plotxSeq <- plotSeq(plotxRange, l=40)

  if (is.factor(modxVar)){
    if (is.null(modxVals)){
      modxVals <- levels(modxVar)
    }
    lmx <- length(modxVals)
  }else{                      
    modxRange <- range(mm[ , modx])
    ## use quantile values of modx unless told othewise
    if (is.null(modxVals)) {
      modxVals <- quantile(mm[ , modx])
      ##    names(modxVals) <- round(modxVals , 2)
    }
    lmx <- length(modxVals)
  }
    
  predictors <- colnames(model$model)[-1]
  predictors <- setdiff(predictors, c(modx,plotx))
  
  newdf <-data.frame(expand.grid(plotxRange, modxVals))
  colnames(newdf) <- c(plotx, modx)
  if (length(predictors) > 0) newdf <- cbind(newdf, centralValues(model$model[ ,predictors]))
  newdf$pred <- predict(model, newdata=newdf)
  
##  mmmeans <- apply(mm, 2, mean)
##  ##  remove plotx and modx variables from means
##  mmmeans <- mmmeans[ -which(names(mmmeans) %in% c(plotx, modx))]
  ##
##  newdf <- vector(length(modxRange), mode="list")
##  for( i in 1:length(modxVals)){
##    newdf[[i]] <- data.frame(plotxSeq)
##    colnames(newdf[[i]]) <- c(plotx)
##    newdf[[i]][ , modx] <- modxVals[i]
##    newdf[[i]] <- data.frame(newdf[[i]], t(mmmeans))
##    newdf[[i]]$pred <-predict(model, newdata=newdf[[i]])
##  }
  
#  plot( mm[, plotx], dat$y)
#  lapply(newdf, function(mydat) { lines (mydat[[plotx]] , mydat$pred)} )
  if (!plotPoints) plot(mm[, plotx], depVar, xlab=plotx, ylab=ylab, type="n")
  else {
    if (is.factor(modxVar)){
       plot( mm[, plotx], depVar, xlab=plotx, ylab=ylab, col=modxVar)
     }else{
       plot( mm[, plotx], depVar, xlab=plotx, ylab=ylab)
     }
  }
#  for(i in 1:length(modxVals)){
#    lines (newdf[[i]][[plotx]] , newdf[[i]][["pred"]], lty=i, col=i)
#  }

  for( i in 1:lmx){
    pdat <- newdf[ newdf[, modx]  %in% modxVals[i], ]
    lines( pdat[ , plotx] , pdat$pred, lty=i, col=i, lwd=2 )
  }

  
  if (is.null(names(modxVals))) {
    legnd <- paste(modx," = ", modxVals, sep= "")
  } else { legnd<- paste(modx," = ", names(modxVals), sep = "")}
  legend("topleft", legend=legnd, lty=1:lmx, col=1:lmx, bg="white")
  ### Plot work finished.

  ### Now work on t-statistics for those slopes
  if (!is.factor(modxVar)) {
  ## If there IS an interaction term in model, we need to calculate
### simple-slope = b_plotx + modxVals * b_modx:plotx
### But If the IS NO interaction term, we just need
### simple-slope = b_plotx.
    ivs <- attr(terms(model), "term.labels")
    bs <- coef(model)
    V <- vcov(model)
    relevantInteractions <-  c(paste(plotx,":",modx,sep=""), paste(modx,":",plotx,sep=""))
    
    bmodx <- NULL
    bplotx <- bs[plotx]
    if (any(relevantInteractions %in% ivs)) {
      interactionsIn <- relevantInteractions[which (relevantInteractions %in% ivs)]
      bmodx <- bs[interactionsIn]
      bsimple <- bplotx + bmodx * modxVals
      covbsimple <- cbind(1, modxVals^2, 2*modxVals) %*% c(V[plotx , plotx],  V[names(bmodx), names(bmodx)],  V[plotx, names(bmodx)])
      tbsimple <- bsimple / sqrt(covbsimple)
    } else {
      bmodx <- 0
      bsimple <- rep(bplotx, length(modxVals))
      covbsimple <- vcov(model)[plotx, plotx]
      tbsimple <- bsimple / sqrt(covbsimple)
    }
    
    data.frame(modx = modxVals, b = bsimple, se= sqrt(covbsimple), t= tbsimple, p= 2 * pt(abs(tbsimple), df=model$df.residual, lower.tail = FALSE))
  }
}  


set.seed(12345)
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
x4 <- rnorm(100)
y <- rnorm(100)
y2 <- 0.03 + 0.1*x1 + 0.1*x2 + 0.25*x1*x2 + 0.4*x3 -0.1*x4 + 1*rnorm(100)
dat <- data.frame(x1,x2,x3,x4,y, y2)
rm(x1, x2, x3, x4, y, y2)

##ordinary regression
m1 <- lm(y ~ x1 + x2 +x3 + x4, data=dat)
## must specify depvar parameter
ssplot(m1, plotx="x1", modx="x2", modxVals=c(-0.5,0,0.5))
ssplot(m1, plotx="x1", modx="x2")
ssplot(m1, plotx="x4", modx="x1")

m2 <- lm(y2 ~ x1*x2 + x3 +x4, data=dat)
summary(m2)
ssplot(m2, plotx="x1", modx="x2")

ssplot(m2, plotx="x1", modx="x2", modxVals=c( -2, -1, 0, 1, 2))

ssplot(m2, plotx="x3", modx="x2")




### Examples with categorical Moderator variable

xcontinuous <- rnorm(100)
xcategorical <- gl(2,50, labels=c("Gigantic","Humongous"))
stde <- 8
y <- 3 + 0.5*xcontinuous + 1.2 * (as.numeric(xcategorical)-1) +
  -0.8* (as.numeric(xcategorical)-1) * xcontinuous +  stde * rnorm(100)

m1 <- lm (y ~ xcontinuous + xcategorical)
summary(m1)

ssplot(m1, modx = "xcategorical", plotx = "xcontinuous")

m2 <- lm (y ~ xcontinuous * xcategorical)
summary(m2)
ssplot(m2, modx = "xcategorical", plotx = "xcontinuous")


library(car)
m3 <- lm(statusquo ~ income * sex, data = Chile)
summary(m3)
ssplot(m3, modx = "sex", plotx = "income")


m4 <- lm(statusquo ~ region * income, data= Chile)
summary(m4)
ssplot(m4, modx = "region", plotx = "income")

ssplot(m4, modx = "region", plotx = "income", plotPoints=FALSE)


m5 <- lm(statusquo ~ region * income + sex + age, data= Chile)
summary(m5)
ssplot(m5, modx = "region", plotx = "income")

m6 <- lm(statusquo ~ income * age + education + sex + age, data=Chile)
summary(m6)
ssplot(m6, modx = "income", plotx = "age")

ssplot(m6, modx = "income", plotx = "age", plotPoints=F)


##Should cause error because education is not numeric
m7 <- lm(statusquo ~ income * age + education + sex + age, data=Chile)
summary(m7)
ssplot(m7, modx = "income", plotx = "education")

## Should cause error because "as.numeric(education") not same as
## plotx="education"
m8 <- lm(statusquo ~ income * age + as.numeric(education) + sex + age, data=Chile)
summary(m8)
ssplot(m8, modx = "income", plotx = "education")

## Still fails. 
ssplot(m8, modx = "income", plotx = "as.numeric(education)")

## Must recode variable first so that variable name is coherent
Chile$educationn <- as.numeric(Chile$education)
m9 <- lm(statusquo ~ income * age + educationn + sex + age, data=Chile)
summary(m9)
ssplot(m9, modx = "income", plotx = "educationn")
