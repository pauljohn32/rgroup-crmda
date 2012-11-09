## Title: jntest
## Author: Alexander M. Schoemann <schoemann@ku.edu>
## Date posted: 02/04/12
## Depends: None
## Description: A function to find regions of signficance for 2 way interactions 
## using the Jonnson-Neyman Technique. 
## ----------------------------------------------------------------------------------------------------


## jntest= Finding regions of significance with the Johnson-Neyman Technique
## jntest a function that takes these arguments:
## model <- m1 #lm or glm (?) object
## plotx <- "x1" #string with name of IV to be plotted on x axis
## modx <- "x2" #string for moderator variable name
## depVar <- NULL. The dependent variable must be specified, or it must
## be available inside the fitted regression object (set lm argument y=TRUE)
## if plotx or modx is missing, stop

jntest <- function(model=NULL, plotx=NULL,modx=NULL, depVar=NULL){
  if (is.null(model))
      stop("sstest requires a fitted regression model.")
  if (is.null(plotx))
      stop("sstest requires the name of the variable to be drawn on the x axis")
  if (is.null(modx))
      stop("sstest requires the name of moderator variable for which several slopes are to be drawn")
  ##grab model matrix
  mm <- model.matrix(model)
  if (is.null(depVar)){
    if (is.null(model$y))
        stop("sstest must have the dependent variable. You must either supply depVar as an argument or create your lm model with the argument y=T so that sstest can grab the dependent variable from the fitted model.")
     else depVar <- model$y
  }

  #allow user to sepcify either variable as the moderator
  if (sum((attr(terms(m1),'term.labels')==paste(plotx,":",modx,sep="")))==1)
    modname <- paste(plotx,":",modx,sep="")
  else
    modname <- paste(modx,":",plotx,sep="")

  #which finds the location of the parameter. Can be used to find location in the model matrix.
  xnum<-which(attr(terms(model),'term.labels')==plotx)
  znum<-which(attr(terms(model),'term.labels')==modx)
  xznum<-which(attr(terms(model),'term.labels')==modname)

  acov <-vcov(model) ##get variance/covariance matrix of parameters
  tcrit<-qt(.975,model$df)

  jna <- tcrit^2*acov[xznum,xznum]-coef(model)[xznum]^2
  jnb <- 2*(tcrit^2*acov[xnum,xznum]-coef(model)[xnum]*coef(model)[xznum])
  jnc <- tcrit^2*acov[xnum,xnum]-coef(model)[xnum]^2

  inroot<- (jnb^2)-(4*jna*jnc)
  regsig<-c(NA,NA)

  if(inroot < 0)
    stop("Regions of significance are imaginary and cannot be computed")
  else
    #Compute regions of significance
    regsig[1]<-(-jnb-sqrt(inroot))/(2*jna)
    regsig[2]<-(-jnb+sqrt(inroot))/(2*jna)

  ##Determine if simple slopes are significant inside or outside the region
  midp<-mean(regsig)
  midps <- coef(model)[xnum] + coef(model)[znum]*midp
  midpse <- sqrt(acov[xnum,xnum] + 2*midp*acov[xnum,xznum] + (midp^2)*acov[xznum,xznum])
  midpp <- pt(-abs(midps/midpse),model$df)
  if(midpp>.05)
    direction<-"Simple slopes are significant outside this region"
  else
    direction<-"Simple slopes are significant inside this region"

  regsig<-list(regsig,direction)

  return(regsig)
}



set.seed(12345)
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
x4 <- rnorm(100)
y <- rnorm(100)
y2 <- 0.03 + 0.15*x1 + 0.1*x2 + 0.5*x1*x2

dat <- data.frame(x1,x2,x3,x4,y, y2)
rm(x1, x2, x3, x4, y, y2)

##ordinary regression
m1 <- lm(y ~ x1 * x2, data=dat)
## must specify depvar parameter
jntest(m1, plotx="x1", modx="x2", depVar=dat$y)
