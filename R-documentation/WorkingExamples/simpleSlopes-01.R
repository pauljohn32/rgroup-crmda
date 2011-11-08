## Paul E. Johnson <pauljohn@ku.edu>
## 2011-11-05

## Development version of a simple slope plotter.
## This version only works with numeric moderator.

## TODO: add switch to allow use of categorical moderator
## TODO: add swith to draw shaded "zones" to summarize
## statistical sigificance.

###Just a convenience function. Is in rockchalk package
plotSeq <- function(x, length.out=length(x)){
  xr <- range(x)
  pseq <- seq(xr[1], xr[2], length.out=length.out)
  pseq
}


## ssplot= simple slope plot
## ssplot a function that takes these arguments:
## model <- m1 # fitted regression object. must have a predict method
## plotx <- "x1" #string with name of IV to be plotted on x axis
## modx <- "x2" #string for moderator variable name
## modxVals <- c(1,2,3) #user specified "for instance" values. If
## omitted, quantiles will be used.
## depVar <- NULL. The dependent variable must be specified, or it must
## be available inside the fitted regression object (set lm argument y=TRUE)
## if plotx or modx is missing, stop

ssplot <- function(model=NULL, plotx=NULL, modx=NULL, modxVals=NULL){
  if (is.null(model)) stop("ssplot requires a fitted regression model.")
  if (is.null(plotx)) stop("ssplot requires the name of the variable to be drawn on the x axis")
  if (is.null(modx)) stop("ssplot requires the name of moderator variable for which several slopes are to be drawn")
  ##grab model matrix
  mm <- model.matrix(model)
  
  depVar <- model$model[ ,1] #first column is DV
  ylab <- colnames(model$model)[1]

  plotyRange <- range(depVar)
  plotxRange <- range(mm[ , plotx])
  modxRange <- range(mm[ , modx])
  ## use quantile values of modx unless told othewise
  if (is.null(modxVals)) {
    modxVals <- quantile(mm[ , modx])
##    names(modxVals) <- round(modxVals , 2)
  }
  
  
  plotxSeq <- plotSeq(plotxRange, l=40)
  
  mmmeans <- apply(mm, 2, mean)
  ##  remove plotx and modx variables from means
  mmmeans <- mmmeans[ -which(names(mmmeans) %in% c(plotx, modx))]
  
  newdf <- vector(length(modxRange), mode="list")
  for( i in 1:length(modxVals)){
    newdf[[i]] <- data.frame(plotxSeq)
    colnames(newdf[[i]]) <- c(plotx)
    newdf[[i]][ , modx] <- modxVals[i]
    newdf[[i]] <- data.frame(newdf[[i]], t(mmmeans))
    newdf[[i]]$pred <-predict(model, newdata=newdf[[i]])
  }
  
#  plot( mm[, plotx], dat$y)
#  lapply(newdf, function(mydat) { lines (mydat[[plotx]] , mydat$pred)} )
  
  plot( mm[, plotx], dat$y, xlab=plotx, ylab=ylab)
  for(i in 1:length(modxVals)){
    lines (newdf[[i]][[plotx]] , newdf[[i]][["pred"]], lty=i, col=i)
  }

  if (is.null(names(modxVals))) {
    legnd <- paste(modx," = ", modxVals, sep= "")
  } else { legnd<- paste(modx," = ", names(modxVals), sep = "")}
  legend("topleft", legend=legnd, lty=1:length(modxVals), col=1:length(modxVals), bg="white")
  ### Plot work finished.

  ### Now work on t-statistics for those slopes
  ### If there IS an interaction term in model, we need to calculate
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
