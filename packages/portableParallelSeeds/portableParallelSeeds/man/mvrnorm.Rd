\name{mvrnorm}
\alias{mvrnorm}
\title{Simulate from a Multivariate Normal Distribution}
\usage{
  mvrnorm(n = 1, mu, Sigma, tol = 1e-06, empirical = FALSE,
    EISPACK = FALSE)
}
\arguments{
  \item{n}{the number of samples ("rows" of data)
  required.}

  \item{mu}{a vector giving the means of the variables.}

  \item{Sigma}{positive-definite symmetric matrix
  specifying the covariance matrix of the variables.}

  \item{tol}{tolerance (relative to largest variance) for
  numerical lack of positive-definiteness in \code{Sigma}}

  \item{empirical}{logical. If true, mu and Sigma specify
  the empirical not population mean and covariance matrix.}

  \item{EISPACK}{logical. Set to true to reproduce results
  from MASS versions prior to 3.1-21.}
}
\value{
  If \code{n = 1} a vector of the same length as \code{mu},
  otherwise an \code{n} by \code{length(mu)} matrix with
  one sample in each row.
}
\description{
  This is the \code{\link[MASS]{mvrnorm}} function from the
  MASS package (Venables and Ripley, 2002), with one small
  modification to facilitate replication of random samples
  of various sizes. The aim is to make replicable the first
  k rows of data generated from mvrnorm, where k < n. This
  assumes, of course, that the user runs \code{set.seed} to
  re-initialize the random generator before each usage of
  mvrnorm.
}
\details{
  Users who draw a sample size of n=(N+k) may hope that
  mvrnorm will produce the exact same observations for the
  first 1:N rows in the output data when k is adjusted. The
  version of \code{mvrnorm} provided with MASS does not do
  so.  After re-setting the seed, this function assures
  that the rows of the smaller set will match the larger
  sample up to row N. Draws after N will differ, of course,
  but in a replicable way, so that one could then draw a
  sample of size (N + k + k2) and the first (N + k) values
  will match the previous sample. Please run the example
  for an illustration.

  Why is this important?  We are trying to isolate the
  sources of change between samples. \code{mvrnorm} gives
  the exact same values for column one up to row (n) when a
  sample size changes, but it gives different results for
  the other columns. This causes confusion among
  researchers, some of whom exect the rows should be the
  same up to a point, while others expect that each column
  should be completely replaced each time.
}
\examples{
library(portableParallelSeeds)
set.seed(12345)
X0 <- MASS::mvrnorm(n=10, mu = c(0,0,0), Sigma = diag(3))
## create a smaller data set, starting at same position
set.seed(12345)
X1 <- MASS::mvrnorm(n=5, mu = c(0,0,0), Sigma = diag(3))
## Create a larger data set
set.seed(12345)
X2 <- MASS::mvrnorm(n=15, mu = c(0,0,0), Sigma = diag(3))
## The first 5 rows in X0, X1, and X2 are not the same
identical(X0[1:5, ], X1[1:5, ])
identical(X1[1:5, ], X2[1:5, ])
set.seed(12345)
Y0 <- mvrnorm(n=10, mu = c(0,0,0), Sigma = diag(3))
set.seed(12345)
Y1 <- mvrnorm(n=5, mu = c(0,0,0), Sigma = diag(3))
set.seed(12345)
Y2 <- mvrnorm(n=15, mu = c(0,0,0), Sigma = diag(3))
identical(Y0[1:5, ], Y1[1:5, ])
identical(Y1[1:5, ], Y2[1:5, ])
}
\author{
  Ripley, B.D. with revision by Paul E. Johnson
}
\references{
  Venables, W. N. & Ripley, B. D. (2002) Modern Applied
  Statistics with S. Fourth Edition. Springer, New York.
  ISBN 0-387-95457-0
}

