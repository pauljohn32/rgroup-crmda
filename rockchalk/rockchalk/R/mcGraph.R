#' Illustrate the effect of multicollinearity in regression.
#'
#' This is a set of functions that faciliates the examination
#' of multicollinearity. Suppose the "true" relationship is
#'  y[i] = 0.2 * x1[i] + 0.2 * x2[i] + e
#' where e is Normal(0, stde^2).
#'
#' @usage mcGraph1 (x1, x2, y, ...)
#' @usage mcGraph2 (x1, x2, y, rescaley=1, ...)
#' @usage mcGraph3 (x1, x2, y, ...)
#' @name mcGraph1
#' @aliases  mcGraph2 mcGraph3 mcGraph
#' @param x1 a predictor vector
#' @param x2 a predictor vector
#' @param y  the dependent variable
#' @param ... additional parameters passed to persp
#' @export mcGraph1 mcGraph2 mcGraph3
#' @import MASS
#' @examples
#' set.seed(12345)
#' ## Create data with x1 and x2 correlated at 0.10
#' dat <- genCorrelatedData(rho=.1, stde=7)
#' 
#' mcGraph1(dat$x1, dat$x2, dat$y, theta=20, phi=8, ticktype="detailed", nticks=10)
#' 
#' 
#' ## This will "grow" the "cloud" of points up from the
#' ## x1-x2 axis
#' mcGraph2(dat$x1, dat$x2, dat$y, rescaley = 0.0, theta = 0) 
#' mcGraph2(dat$x1, dat$x2, dat$y, rescaley = 0.1, theta = 0) 
#' mcGraph2(dat$x1, dat$x2, dat$y, rescaley = 0.2, theta = 0)
#' mcGraph2(dat$x1, dat$x2, dat$y, rescaley = 0.3, theta = 0) 
#' mcGraph2(dat$x1, dat$x2, dat$y, rescaley = 0.4, theta = 0) 
#' mcGraph2(dat$x1, dat$x2, dat$y, rescaley = 0.5, theta = 0)
#' mcGraph2(dat$x1, dat$x2, dat$y, rescaley = 0.6, theta = 0) 
#' mcGraph2(dat$x1, dat$x2, dat$y, rescaley = 0.7, theta = 0) 
#' mcGraph2(dat$x1, dat$x2, dat$y, rescaley = 0.8, theta = 0)
#' mcGraph2(dat$x1, dat$x2, dat$y, rescaley = 0.9, theta = 0) 
#' mcGraph2(dat$x1, dat$x2, dat$y, rescaley = 1, theta = 0)
#' 
#' ##rotate this
#' mcGraph2(dat$x1, dat$x2, dat$y, rescaley = 1, theta = 20)
#' mcGraph2(dat$x1, dat$x2, dat$y, rescaley = 1, theta = 40)
#' mcGraph2(dat$x1, dat$x2, dat$y, rescaley = 1, theta = 60)
#' mcGraph2(dat$x1, dat$x2, dat$y, rescaley = 1, theta = 80)
#' 
#' ## once they reach the top, make them glitter a while
#' for(i in 1:20){
#'   mcGraph2(dat$x1, dat$x2, dat$y, rescaley = runif(length(dat$x1), .9,1.1), theta = 0)
#' }
#' 
#' mcGraph3(dat$x1, dat$x2, dat$y, theta = 0)
#' 
#' dat2 <- genCorrelatedData(rho = 0, stde = 7)
#' 
#' mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = 0, phi = 10)
#' mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = 30, phi = 10)
#' mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = -30, phi = 10)
#' mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = -30, phi = -10)
#' mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = -30, phi = -15)
#' 
#' ## Run regressions with not-strongly correlated data
#' modset1 <- list()
#' for(i in 1:20){
#'   dat2 <- genCorrelatedData(rho = .1, stde = 7)
#'   summary(lm( y ~ x1 + x2 , data = dat2))
#'   modset1[[i]] <- mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = -30)
#' }
#' 
#' 
#' ## Run regressions with strongly correlated data
#' modset2 <- list()
#' for(i in 1:20){
#'   dat2 <- genCorrelatedData(rho = .981, stde = 7)
#'   summary(lm( y ~ x1 + x2 , data = dat2))
#'   modset2[[i]] <- mcGraph3(dat2$x1, dat2$x2, dat2$y, theta = -30)
#' }
#' 
#' ##Compare the estimated coefficients for model sets 1 and 2
#' lapply(modset1, coef)
#' lapply(modset2, coef)
#' 
#' 
mcGraph1 <- function (x1, x2, y, ...){
  x1range <- magRange(x1, 1.25)
  x2range <- magRange(x2, 1.25)
  yrange <- magRange(y, 1.5)
  res <- perspEmpty(x1=x1range, x2=x2range, y=yrange, ...)
  yMinimum <- rep(yrange[1] , length(x1))
  mypoints1 <- trans3d ( x1, x2, yMinimum, pmat = res )
  points( mypoints1, pch = 16, col= "blue")
}


mcGraph2 <- function(x1, x2, y, rescaley=1, ...){
  x1range <- magRange(x1, 1.25)
  x2range <- magRange(x2, 1.25)
  yrange <- magRange(y, 1.5)
  
  res <- perspEmpty(x1=x1range, x2=x2range, y=yrange, ...)
  
  mypoints1 <- trans3d ( x1, x2 ,yrange[1], pmat = res )
  newy <- rescaley * (y - yrange[1]) + yrange[1]
  mypoints2 <- trans3d ( x1 , x2 , newy , pmat = res )
  points( mypoints2, pch = 1, col= "blue")
  points( mypoints1, pch = 16, col=gray(0.8))
  
  mypoints2s <- trans3d ( x1, x2, (0.8)*newy, pmat =res )
  arrows ( mypoints1$x , mypoints1$y , mypoints2s$x , mypoints2s$y , col="red" , lty = 2, lwd=0.3, length=0.1)
}




mcGraph3 <- function(x1, x2, y, ...){
  x1range <- magRange(x1, 1.25)
  x2range <- magRange(x2, 1.25)
  yrange <- magRange(y, 1.5)

  res <- perspEmpty(x1=x1range, x2=x2range, y=yrange, ...)
  
  mypoints1 <- trans3d( x1, x2, yrange[1], pmat = res )
  
  mypoints2 <- trans3d( x1, x2, y, pmat = res )
  points( mypoints2, pch = 1, col = "blue")
  points( mypoints1, pch = 16, col = gray(0.8))
 
  m1 <- lm( y ~ x1 + x2)
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


