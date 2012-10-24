\name{seedCreator}
\alias{seedCreator}
\title{Creates a portable sets of seeds to be used for replication of
simulations. The seeds are suitable for replicatable parallel
computations. This creates the seed object and, if the file
argument is specified, it writes the object on disk.}
\usage{
  seedCreator(nReps = 2000, streamsPerRep = 3, seed,
    file = NULL)
}
\arguments{
  \item{nReps}{Number of replications for which starting
  seed values are to be created.}

  \item{streamsPerRep}{Number of streams to be created for
  each separate simulation.}

  \item{seed}{An integer seed value that can be used to
  initialize the creation of the many-separate-substreams.}

  \item{file}{The file name in which the list of stream
  seeds is to be collected, by default, that file is called
  "projSeeds.rds".}
}
\value{
  A list that includes "nReps" elements. Each element is a
  vector of "streamsPerRep" stream starting values.
}
\description{
  Using the L'Ecuyer-CMRG random generator provided by the
  R package parallel, this follows the strategy outlined in
  L'Ecuyer, et. al (2002). The very very long random stream
  is subdivided into smaller streams, thus allowing the
  creation of several separate streams of random numbers
  for each of the runs of a simulation exercise.
}
\details{
  This makes it possible to execute a batch of simulations,
  and then later re-start any particular run of interest.
  This works whether the calculations are done in parallel
  on a cluster or serial on a single computer. This
  function takes into account the possibility that, within
  each run of the model, it may be necessary to draw values
  from several separate streams of random numbers (for
  example, to keep separate streams for the generation of
  data about teachers, students, school buildings, and so
  forth). This function creates the seeds, allowing for
  accurate replication on a diverse network of computing
  devices. It generates the seeds of many separate streams
  and returns them in an object (and also writes them in a
  file).  The seeds to re-start a simulation are kept in a
  list object.

  The user specifies the number of simulation runs,
  "nReps," so the output list will have "nReps" elements.
  In each of those objects inside the list, there will be
  seeds to initialize "streamsPerRep" streams.
}
\examples{
projSeeds <- seedCreator(2000, 3, seed = 123456, file = "fruits.rds")
A1 <- projSeeds[[787]]

rm(projSeeds)
# read from file, take run 787's seed
myFruitySeeds <- readRDS("fruits.rds")
B1 <- myFruitySeeds[[787]]

identical(A1, B1) # check
unlink("fruits.rds") #delete file
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}
\references{
  L'Ecuyer, P. (1999). Good Parameters and Implementations
  for Combined Multiple Recursive Random Number Generators.
  Operations Research, 47(1), 159-164. L'Ecuyer, P.,
  Simard, R., Chen, E. J., & Kelton, W. D. (2002). An
  Object-Oriented Random-Number Package with Many Long
  Streams and Substreams. Operations Research, 50(6),
  1073-1075.
}
\seealso{
  snowft, streams, parallel
}

