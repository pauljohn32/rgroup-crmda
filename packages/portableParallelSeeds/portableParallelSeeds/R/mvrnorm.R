##' Simulate from a Multivariate Normal Distribution
##'
##' This is the \code{\link[MASS]{mvrnorm}} function from the MASS
##' package (Venables and Ripley, 2002), with one small modification
##' to facilitate replication of random samples of various sizes. The
##' aim is to make replicable the first k rows of data generated from
##' mvrnorm, where k < n. This assumes, of course, that the user runs
##' \code{set.seed} to re-initialize the random generator before each
##' usage of mvrnorm.
##'
##' Users who draw a sample size of n=(N+k) may hope that mvrnorm will
##' produce the exact same observations for the first 1:N rows in the
##' output data when k is adjusted. The version of \code{mvrnorm}
##' provided with MASS does not do so.  After re-setting the seed,
##' this function assures that the rows of the smaller set will match
##' the larger sample up to row N. Draws after N will differ, of
##' course, but in a replicable way, so that one could then draw a
##' sample of size (N + k + k2) and the first (N + k) values will
##' match the previous sample. Please run the example for an
##' illustration.
##'
##' Why is this important?  We are trying to isolate the sources of
##' change between samples. \code{mvrnorm} gives the exact same values
##' for column one up to row (n) when a sample size changes, but it
##' gives different results for the other columns. This causes
##' confusion among researchers, some of whom exect the rows should be
##' the same up to a point, while others expect that each column
##' should be completely replaced each time.
##' @param n the number of samples ("rows" of data) required.
##' @param mu a vector giving the means of the variables.
##' @param Sigma positive-definite symmetric matrix specifying the
##'    covariance matrix of the variables.
##' @param tol tolerance (relative to largest variance) for numerical lack
##'    of positive-definiteness in \code{Sigma}
##' @param empirical logical. If true, mu and Sigma specify the empirical
##'    not population mean and covariance matrix.
##' @param EISPACK logical. Set to true to reproduce results from MASS
##'    versions prior to 3.1-21.
##' @imports MASS
##' @export mvrnorm
##' @return If \code{n = 1} a vector of the same length as \code{mu}, otherwise an
##'  \code{n} by \code{length(mu)} matrix with one sample in each row.
##' @author Ripley, B.D. with revision by Paul E. Johnson
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
