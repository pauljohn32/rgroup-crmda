\name{portableParallelSeeds-package}
\alias{portableParallelSeeds-package}
\alias{portableParallelSeeds}
\docType{package}
\title{
Creates, saves, reloads, and selects among streams of
random numbers. 
}

\description{
Creates, saves, reloads, and selects among streams of
random numbers. 

}

\details{
\tabular{ll}{
Package: \tab portableParallelSeeds\cr
Type: \tab Package\cr
License: \tab GPL (v >= 2)\cr
}
This demonstrates one way to create and use a collection of initializing
values for the R random number generator to assure replication in a
collection of runs, whether executed in parallel or in serial.  Each
individual run within a simulation project can be individually
replicated and verfied. From a batch of thousands of
simulations, one can select any arbritray "run" of the model and
re-initialize it in isolation. Identical random number streams are
assured whether a simulation is executed on one computer or 1000 nodes
in a cluster. In addition, this framework allows each "run" to
draw from several separate streams of random numbers. Please see the
help file for the function \code{initPortableSeeds} for
a full working example. A vignette called pps is also being prepared.
}
\author{
Paul E. Johnson <pauljohn@ku.edu>
Maintainer: Paul E. Johnson <pauljohn@ku.edu>
}
\references{
  Chambers, John M. (2008) \emph{Software for Data Analysis: Programming with
  R}. New York: Springer.
  
  L'Ecuyer, P. (1999). Good Parameters and Implementations
  for Combined Multiple Recursive Random Number Generators.
  \emph{Operations Research}, 47(1), 159-164.

  L'Ecuyer, P., Simard, R., Chen, E. J., & Kelton, W. D. (2002). An
  Object-Oriented Random-Number Package with Many Long Streams and
  Substreams. \emph{Operations Research}, 50(6), 1073-1075.

  Sevcikova, Hana and A. J. Rossini (2010). snowFT: Fault Tolerant
  Simple Network of Workstations. R package version 1.2-0.
  \url{http://CRAN.R-project.org/package=snowFT}
}

\keyword{ iteration }
\keyword{ programming }
\keyword{ environment }
\seealso{
  \code{\link[rstream:rstream-package]{rstream}}
  \code{\link[snowFT:snowFT-package]{snowft}}
  \code{\link[parallel:parallel-package]{parallel}}
  \code{\link[SoDA]{SoDA}}
}
