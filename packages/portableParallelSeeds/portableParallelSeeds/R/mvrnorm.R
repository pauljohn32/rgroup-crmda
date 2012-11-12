##' Simulate from a Multivariate Normal Distribution
##'
##' This is the \code{\link[MASS]{mvrnorm}} function from the MASS package (Ripley, et al),
##' with one small modification to facilitate replication of random samples. Suppose we draw N samples (rows of multivariate data in a matrix). If we re-set the random seed, we are now able to guarantee that the first N rows in a new sample will be exactly the same as the original sample.  The mvrnorm in MASS does not have that property, as is demonstrated in the example.
##' @title
##' @param n the number of samples ("rows" of data) required.}
##' @param mu a vector giving the means of the variables.
##' @param Sigma positive-definite symmetric matrix specifying the
##'    covariance matrix of the variables.
##' @param tol tolerance (relative to largest variance) for numerical lack
##'    of positive-definiteness in \code{Sigma}
##' @param empirical logical. If true, mu and Sigma specify the empirical
##'    not population mean and covariance matrix.
##' @param EISPACK logical. Set to true to reproduce results from MASS
##'    versions prior to 3.1-21.
##' @return If \code{n = 1} a vector of the same length as \code{mu}, otherwise an
##'  \code{n} by \code{length(mu)} matrix with one sample in each row.
##' @author Ripley, B.D. with revision by Paul Johnson
##' @references
##' Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with
##' S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0
##' @examples
##'
##' library(portableParallelSeeds)
##' set.seed(12345)
##' X0 <- MASS::mvrnorm(n=10, mu = c(0,0,0), Sigma = diag(3))
##' ## create a smaller data set, starting at same position
##' set.seed(12345)
##' X1 <- MASS::mvrnorm(n=5, mu = c(0,0,0), Sigma = diag(3))
##' ## Create a larger data set
##' set.seed(12345)
##' X2 <- MASS::mvrnorm(n=15, mu = c(0,0,0), Sigma = diag(3))
##' ## The first 5 rows in X0, X1, and X2 are not the same
##' identical(X0[1:5, ], X1[1:5, ])
##' identical(X1[1:5, ], X2[1:5, ])
##' set.seed(12345)
##' Y0 <- mvrnorm(n=10, mu = c(0,0,0), Sigma = diag(3))
##' set.seed(12345)
##' Y1 <- mvrnorm(n=5, mu = c(0,0,0), Sigma = diag(3))
##' set.seed(12345)
##' Y2 <- mvrnorm(n=15, mu = c(0,0,0), Sigma = diag(3))
##' identical(Y0[1:5, ], Y1[1:5, ])
##' identical(Y1[1:5, ], Y2[1:5, ])
mvrnorm <-
    function(n = 1, mu, Sigma, tol=1e-6, empirical = FALSE, EISPACK = FALSE)
{
    p <- length(mu)
    if(!all(dim(Sigma) == c(p,p))) stop("incompatible arguments")
    if (missing(EISPACK)) EISPACK <- getOption("mvnorm_use_EISPACK", FALSE)
    eS <- eigen(Sigma, symmetric = TRUE, EISPACK = EISPACK)
    ev <- eS$values
    if(!all(ev >= -tol*abs(ev[1L]))) stop("'Sigma' is not positive definite")
    X <- matrix(rnorm(p * n), n, byrow = TRUE)
    if(empirical) {
        X <- scale(X, TRUE, FALSE) # remove means
        X <- X %*% svd(X, nu = 0)$v # rotate to PCs
        X <- scale(X, FALSE, TRUE) # rescale PCs to unit variance
    }
    X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% t(X)
    nm <- names(mu)
    if(is.null(nm) && !is.null(dn <- dimnames(Sigma))) nm <- dn[[1L]]
    dimnames(X) <- list(nm, NULL)
    if(n == 1) drop(X) else t(X)
}
