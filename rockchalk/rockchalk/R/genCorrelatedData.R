#' Generates a data frame (x1,x2,y) with user-specified correlation between x1 and x2
#'
#' @param N Number of cases desired
#' @param means 2-vector of means for x1 and x2
#' @param sds 2-vector of standard deviations for x1 and x2
#' @param rho Correlation coefficient for x1 and x2
#' @param stde standard deviation of the error term in the data generating equation 
#' @export genCorrelatedData

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
  
