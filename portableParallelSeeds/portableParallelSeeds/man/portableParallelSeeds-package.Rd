\name{portableParallelSeeds-package}
\alias{portableParallelSeeds-package}
\alias{portableParallelSeeds}
\docType{package}
\title{
Creates, saves, reloads, and selects among streams of
random numbers. 
}
\description{
This implements one approach for exact replication of
simulations that are conducted on different compute systems. From a
batch of thousands of simulations, one can select any arbritray "run"
of the model and re-initialize it (replicate it) in isolation. Why is
this useful? Suppose a simulation requires an exercise to be conducted
2000 times for various randomly-generated data. We want to be sure we
get the same results if we run that in a compute cluster or if we run
on a single workstation. It is thus necessary to create a matrix of
random generator states (loosely referred to as seeds, but they really
are random generator states), one for each "run", and then conduct the
simulation so that the saved seeds are properly put to user when a
particular run is to be executed. This framework allows each "run" to
draw from several separate streams of random numbers.
}
\details{
\tabular{ll}{
Package: \tab portableParallelSeeds\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-10-24\cr
License: \tab GPL (v >= 2)\cr
}
Creates, saves, reloads, and selects among streams of
random numbers. 
}
\author{
Paul E. Johnson <pauljohn@ku.edu>

Maintainer: Paul E. Johnson <pauljohn@ku.edu>
}
\references{
  Chambers, (2008) Software for Data A
  
 L'Ecuyer, P. (1999). Good Parameters and Implementations
  for Combined Multiple Recursive Random Number Generators.
  Operations Research, 47(1), 159-164.

 L'Ecuyer, P., Simard, R., Chen, E. J., & Kelton, W. D. (2002). An
  Object-Oriented Random-Number Package with Many Long Streams and
  Substreams. Operations Research, 50(6), 1073-1075.  } 
\keyword{ iteration }
\keyword{ programming }
\keyword{ environment }
\seealso{
  \code{\link[rstream:rstream-package]{rstream}}
  \code{\link[snowFT:snowFT-package]{snowft}}
  \code{\link[parallel:parallel-package]{parallel}}
  \code{\link[rlecuyer]{ResetNextSubstream}}
}
\examples{
   mySeeds <- seedCreator(500, 5, file="mySeeds.rds", seed = 123123)
   mySeeds[[17]][[1]]
   mySeeds[[17]][[4]]
   initPortableStreams(mySeeds, run=17)
   .Random.seed
   runif(2)
   rm(mySeeds)
   savedSeeds <- initPortableStreams("mySeeds.rds", run=17)
   .Random.seed
   runif(2)
   getCurrentStream()
   useStream(4)
   .Random.seed
   unlink("mySeeds.rda")
}
