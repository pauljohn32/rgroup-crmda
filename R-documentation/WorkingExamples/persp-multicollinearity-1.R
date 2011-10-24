## multicollinearity-plots.R
## 3-dimensional demonstration of multicollinearity using persp
## Author: Paul Johnson
## Date: 2011-10-22

## This is a set of functions that faciliates the examination
## of multicollinearity. The "true" relationship is
##
##  y[i] = 0.2 * x1[i] + 0.2 * x2[i] + e
##
## where e is Normal(0, stde^2).  The slope coefficients
## are written into the function genCorrelatedData, the
## error standard deviation stde can be set as a user
## argument for that function.

## If you run through this whole thing, you will be
## asked by R to hit return to see the next graph
## several times.

## The main objective here is to demonstrate multicollinearity to
## regression students.


magRange <- function(x, mult = 1.25){
  xr <- range(x)
  magXr <- xr +  c(-1, 1) * (mult-1) * diff(xr)
  magXr
}

plotSeq <- function(x, length.out=length(x)){
  xr <- range(x)
  pseq <- seq(xr[1], xr[2], length.out=length.out)
  pseq
}



mcGraph1 <- function (x1, x2, y, theta=0, phi=15){
  x1range <- magRange(x1, 1.25)
  x2range <- magRange(x2, 1.25)
  yrange <- magRange(y, 1.5)
  
  zZero <- outer( plotSeq(x1range, l=5), plotSeq(x2range, l=5), function( a,b) { a*b*0 + yrange[1] } )
  
  res <- persp(x=plotSeq(x1range, l=5), y= plotSeq(x2range, l=5), z=zZero, zlim=yrange, lwd=1, xlab="x1",ylab="x2",zlab="y", theta=theta, phi=phi)
  
  yMinimum <- rep(yrange[1] , length(x1))
  mypoints1 <- trans3d ( x1, x2, yMinimum, pmat = res )
  points( mypoints1, pch = 16, col= "blue")
}



mcGraph2 <- function(x1,x2,y, shrinky=1, theta=0, phi=15){
  x1range <- magRange(x1, 1.25)
  x2range <- magRange(x2, 1.25)
  yrange <- magRange(y, 1.5)
  
##
  zZero <- outer( plotSeq(x1range, l = 5), plotSeq(x2range, l = 5), function( a,b) { a*b*0 + yrange[1] } )

  res <- persp(x = plotSeq(x1range, l = 5), y = plotSeq(x2range, l = 5), z = zZero, zlim = yrange, lwd = 1, xlab = "x1", ylab = "x2", zlab = "y", theta = theta, phi=phi)
  
  mypoints1 <- trans3d ( x1, x2 ,yrange[1], pmat = res )
  newy <- shrinky * (y - yrange[1]) + yrange[1]
  mypoints2 <- trans3d ( x1 , x2 , newy , pmat = res )
  points( mypoints2, pch = 1, col= "blue")
  points( mypoints1, pch = 16, col=gray(0.8))
  
  mypoints2s <- trans3d ( x1, x2, (0.8)*newy, pmat =res )
  arrows ( mypoints1$x , mypoints1$y , mypoints2s$x , mypoints2s$y , col="red" , lty = 2, lwd=0.3, length=0.1)
}




mcGraph3 <- function(x1, x2, y, theta = 0, phi = 15){
  x1range <- magRange(x1, 1.25)
  x2range <- magRange(x2, 1.25)
  yrange <- magRange(y, 1.5)
  
  
  zZero <- outer( plotSeq(x1range, l = 5), plotSeq(x2range, l = 5), function( a, b) { a*b*0 + yrange[1] } )
  
  
  res <- persp(x = plotSeq(x1range, l = 5), y = plotSeq(x2range, l = 5), z = zZero, zlim = yrange, lwd = 1, xlab = "x1", ylab = "x2", zlab = "y", theta = theta, phi = phi)
  
  mypoints1 <- trans3d( x1, x2, yrange[1], pmat = res )
  
  mypoints2 <- trans3d( x1, x2, y, pmat = res )
  points( mypoints2, pch = 1, col = "blue")
  points( mypoints1, pch = 16, col = gray(0.8))
 
  m1 <- lm( y ~ x1 + x2)
  # summary (m1)
 
  x1seq <- plotSeq (x1range, length = 20)
  x2seq <- plotSeq (x2range , length = 20)
  
  zplane <- outer ( x1seq, x2seq, function(a, b) { predict(m1,
    newdata = data.frame( x1 = a, x2 = b ))} )
  
  for( i in 1:length(x1seq) ){
    lines(trans3d(x1seq[i], x2seq, zplane[i,], pmat = res), lwd = 0.3)
  }
  for( j in 1:length(x2seq) ){
    lines(trans3d(x1seq, x2seq[j], zplane[,j], pmat = res), lwd = 0.3)
  }
  
  mypoints4 <- trans3d (x1 , x2 , fitted(m1) , pmat =res )
##  points(mypoints4)
  newy <- ifelse(fitted(m1) < y, fitted(m1)+ 0.8*(y-fitted(m1)),
                 fitted(m1) + 0.8 * (y-fitted(m1)))
  mypoints2s <- trans3d ( x1, x2, newy, pmat =res )
   
  arrows ( mypoints4$x , mypoints4$y , mypoints2s$x , mypoints2s$y , col = "red" , lty = 4, lwd = 0.3, length = 0.1)
  summary(m1)
}



genCorrelatedData <- function(N = 100, means = c(50,50), sds = c(10,10), rho = 0.0, stde = 1){ 
  require(MASS)
  corr.mat <- matrix(c(1,rho,rho,1), nrow = 2)
  stds.vec <- c(10,10)
  sigma <- diag(sds) %*% corr.mat %*% diag(sds)
  x.mat <-  mvrnorm(n = N, mu = means, Sigma = sigma)
  y = 0 + 0.2* x.mat[,1] + 0.2 * x.mat[,2] + stde*rnorm (100,m = 0, s = 1)
  dat <- data.frame(x.mat, y)
  names(dat) <- c("x1", "x2", "y")
  dat
}
  

##
##pdf(file="mcplots%03d.pdf", onefile=F, paper="special", height=6, width=6)

## Now we get to use those functions to create
## data and plots.


## Create data with x1 and x2 correlated at 0.10
dat <- genCorrelatedData(rho=.1, stde=10)

mcGraph1(dat$x1, dat$x2, dat$y, theta=20, phi=8)


## This will "grow" the "cloud" of points up from the
## x1-x2 axis
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 0.0, theta = 0) 
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 0.1, theta = 0) 
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 0.2, theta = 0)
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 0.3, theta = 0) 
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 0.4, theta = 0) 
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 0.5, theta = 0)
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 0.6, theta = 0) 
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 0.7, theta = 0) 
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 0.8, theta = 0)
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 0.9, theta = 0) 
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 1, theta = 0)

##rotate this

mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 1, theta = 20)
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 1, theta = 40)
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 1, theta = 60)
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 1, theta = 80)
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 1, theta = 100)
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 1, theta = 120)
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 1, theta = 140)
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = 1, theta = 160)

## once they reach the top, make them glitter a while
par(ask=TRUE)
for(i in 1:20){
mcGraph2(dat$x1, dat$x2, dat$y, shrinky = runif(length(dat$x1), .9,1.1), theta = 0)
}
par(ask=FALSE)

mcGraph3(dat$x1, dat$x2, dat$y, theta = 0)

dat2 <- genCorrelatedData(rho = 0, stde = 10)

mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = 0, phi = 10)

mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = 30, phi = 10)

mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = -30, phi = 10)

mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = -30, phi = -10)

mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = -30, phi = -15)



## Now, back to work. Run regressions with not-strongly
## correlated data
par(ask = T)

modset1 <- list()
for(i in 1:20){
  dat2 <- genCorrelatedData(rho = .1, stde = 7)
  summary(lm( y ~ x1 + x2 , data = dat2))
  modset1[[i]] <- mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = -30)
}


## Fit regressions with strongly correlated data
par(ask = T)

modset2 <- list()
for(i in 1:20){
  dat2 <- genCorrelatedData(rho = .981, stde = 7)
  summary(lm( y ~ x1 + x2 , data = dat2))
  modset2[[i]] <- mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = -30)
}

par(ask = F)

##Compare the estimated coefficients for model sets 1 and 2
 lapply(modset1, coef)

 lapply(modset2, coef)

## Turn off the ask permission on plots

##dev.off()
