##J-N technique
##Alex Schoemann & Kris Preacher 
## jnplot= Johnson-Neyman technique for probing interactions and plotting regions of significance
##Replicates the JN plot found at quantpsy.org
## jnplot a function that takes these arguments:
## model <- m1 #object
## plotx <- "x1" #string with name of IV to be plotted on x axis
## modx <- "x2" #string for moderator variable name
## depVar <- NULL. The dependent variable must be specified, or it must
## be available inside the fitted regression object (set lm argument y=TRUE)
## if plotx or modx is missing, stop




jnplot <- function(model=NULL, plotx=NULL,modx=NULL, depVar=NULL){
  if (is.null(model)) stop("jnplot requires a fitted regression model.")
  if (is.null(plotx)) stop("jnplot requires the name of the variable to be drawn on the x axis")
  if (is.null(modx)) stop("jnplot requires the name of moderator variable for which several slopes are to be drawn")
  ##grab model matrix
  mm <- model.matrix(model)
  if (is.null(depVar)){
    if (is.null(model$y)) stop("jnplot must have the dependent variable. You must either supply depVar as an argument or create your lm model with the argument y=T so that jnplot can grab the dependent variable from the fitted model.")
     else depVar <- model$y
  }
    
  #allow user to sepcify either variable as the moderator
 if (sum((attr(terms(m1),'term.labels')==paste(plotx,":",modx,sep="")))==1)
  modname <- paste(plotx,":",modx,sep="")
 else
   modname <- paste(modx,":",plotx,sep="")

  
xnum<-which(attr(terms(model),'term.labels')==plotx) #which finds the location of the parameter. Can be used to find location in matrix.
znum<-which(attr(terms(model),'term.labels')==modx) #which finds the location of the parameter. Can be used to find location in matrix.
xznum<-which(attr(terms(model),'term.labels')==modname) #which finds the location of the parameter. Can be used to find location in matrix.

acov <-vcov(model) ##get variance/covariance matrix of parameters
tcrit<-qt(.975,model$df)

    jna <- tcrit^2*acov[xznum,xznum]-coef(model)[xznum]^2
	jnb <- 2*(tcrit^2*acov[xnum,xznum]-coef(model)[xnum]*coef(model)[xznum])
	jnc <- tcrit^2*acov[xnum,xnum]-coef(model)[xnum]^2
	
	inroot<- (jnb^2)-(4*jna*jnc)
	regsig<-c(NA,NA)
	
	if(	inroot < 0) stop("Regions of significance are imaginary and cannot be computed")
	else
	regsig[1]<-(-jnb-sqrt(inroot))/(2*jna)
	regsig[2]<-(-jnb+sqrt(inroot))/(2*jna)
	
	midp<-mean(regsig)
  	midps <- coef(model)[xnum] + coef(model)[znum]*midp
	midpse <- sqrt(acov[xnum,xnum] + 2*midp*acov[xnum,xznum] + (midp^2)*acov[xznum,xznum])
	midpp <- pt(-abs(midps/midpse),model$df)
	if(midpp>.05) direction<-"Simple slopes are significant outside this region"
	else
	direction<-"Simple slopes are significant inside this region"
	
	#sigtest<- sstest(model, plotx=plotx, modx=modx, modxVals=regsig, depVar=depVar)
	#regsig<-list("Region of Significance"=regsig,direction,"Simple Intercepts and Slopes at Region Boundaries"=sigtest)
	regsig<-list("Region of Significance"=regsig,direction)

	
z1=-10  #supply lower bound for z
z2=10   #supply upper bound for z
z <- seq(z1,z2,length=1000)
fz <- c(z,z)
y1 <- ((coef(model)[xnum])+(coef(model)[xznum])*z)+(1.9858*sqrt(acov[xnum,xnum])+(2*z*(acov[xnum,xznum])+((z^2)*acov[xznum,xznum])))
y2 <- ((coef(model)[xnum])+(coef(model)[xznum])*z)-(1.9858*sqrt(acov[xnum,xnum])+(2*z*(acov[xnum,xznum])+((z^2)*acov[xznum,xznum])))
fy <- c(y1,y2)
fline <- ((coef(model)[xnum])+(coef(model)[xznum])*z)
plot(fz,fy,type='p',pch='.',font=2,font.lab=2,col=2,xlab='Moderator',ylab='Simple Slope',main='Confidence Bands')
lines(z,fline)
f0 <- array(0,c(1000))
lines(z,f0,col=8)
abline(v=-2.7554,col=4,lty=2)
abline(v=0.1557,col=4,lty=2)
	
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
jnplot(m1, plotx="x1", modx="x2", depVar=dat$y)
