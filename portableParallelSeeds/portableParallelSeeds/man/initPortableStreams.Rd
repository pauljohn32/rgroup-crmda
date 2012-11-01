\name{initPortableStreams}
\alias{initPortableStreams}
\title{initPortableStreams}
\usage{initPortableStreams(projSeeds, run, verbose=FALSE)}
\description{Brings saved random streams back to life. Reads a portable
parallel seeds object (or file) and sets the seed collection in
the global environment.}
\details{The portable seeds object is created by the function
\link{seedCreator}. It is a list of lists. The list includes one
set of initializing states for each separate run of a simulation.
Within each of these sets, there will be enough information to
initialize one or more streams of random numbers.  These of
"initializing states" are the internal states of CMRG random
generators (see L'Ecuyer, 1999; L'Ecuyer, et al, 2002).

This function scans the project's portable parallel seeds (either
an in-memory object or a named file), selects the desired run, and
then it writes 3 variables into the R global environment. 1)
startStates is a collection of random generator states, one for
each random stream from which the user might wish to draw random
numbers. This is a fixed value which should not be altered during
the program. It can be used to reset the generators to their
initial positions. 2) currentStates is the collection of random
generator states that will be updated. When the program calls the
\link{useStream} function, the currentStates vector is updated.
3) currentStream indicates which of the currentStates should be
used to draw the next random value.

At the outset, startStates and
currentStates are identical and currentStream equals 1 (meaning
the first element of currentStates is taken as the state of the
random generator).}
\value{nothing is returned. This function is used for the side effect of seetting three objects in the global environment, the startStates (list), currentStates (list), and currentStream (an integer).}
\author{Paul E. Johnson <pauljohn@ku.edu>}
\seealso{\code{seedCreator} to generate the input file for this function and \code{useStream} to change from one stream to another.}
\arguments{\item{projSeeds}{Either an object of class portableSeeds (created
by \code{seedCreator}) or a text string giving the name of an R
saved file of the appropriate format (created by the seedCreator function).}
\item{run}{Integer indicating which element from the portable seed collection is to be selected}
\item{verbose}{True or False: print out the state of the current generator}}
