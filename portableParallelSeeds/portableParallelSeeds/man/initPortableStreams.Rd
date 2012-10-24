\name{initPortableStreams}
\alias{initPortableStreams}
\title{initPortableStreams}
\usage{
  initPortableStreams(projSeeds, run, verbose = TRUE)
}
\arguments{
  \item{projSeeds}{Either an object of class portableSeeds
  (created by \code{seedCreator}) or a text string giving
  the name of an R saved file of the appropriate format
  (created by the seedCreator function).}

  \item{run}{Integer indicating which element from the
  portable seed collection is to be selected}

  \item{verbose}{True or False: print out the state of the
  current generator}
}
\value{
  nothing is returned. This function is used for the side
  effect of seetting three objects in the global
  environment, the startStates (list), currentStates
  (list), and currentStream (an integer).
}
\description{
  Brings saved seeds back to life. Reads a portable
  parallel seeds (object or file) and sets the seed
  collection in the global environment.
}
\details{
  The portable seeds object is a list of lists. The list
  includes many seed collections, one for each run of a
  simulation. For each run, there is one or more sets of
  information that can be used to initialize random streams
  for a given replication.  These of "initializing states"
  are the internal states of CMRG random generators (see
  L'Ecuyer, 1999; L'Ecuyer, et al, 2002).

  The user must specify a particular run's seeds in the
  command line.

  This function scans the project's portable parallel seeds
  file, selects the desired element, and then it writes 3
  variables into the R global environment. 1) startStates
  is a collection of states, one for each random stream
  from which the user might wish to draw random numbers, 2)
  currentStates is the collection of random generator
  states that will be updated as random numbers are drawn,
  3) currentStream indicates which of the currentStates
  should be used to draw the next random value. At the
  outset, startStates and currentStates are identical and
  currentStream equals 1 (meaning the first element of
  currentStates is taken as the state of the random
  generator).
}
\examples{
mySeeds <- seedCreator(500, 5, file="mySeeds.rds", seed = 123123)
initPortableStreams(mySeeds, run=17)
.Random.seed
runif(2)
.Random.seed
runif(2)
rm(mySeeds)
savedSeeds <- initPortableStreams("mySeeds.rds", run=17)
.Random.seed
runif(2)
.Random.seed
runif(2)
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}
\seealso{
  \code{seedCreator} to generate the input file for this
  function and \code{useStream} to change from one stream
  to another.
}

