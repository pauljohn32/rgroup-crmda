\name{getCurrentStream}
\alias{getCurrentStream}
\title{Return integer representing currently selected random stream}
\usage{
  getCurrentStream()
}
\value{
  Integer index value of currently selected stream
}
\description{
  The environment variable currentStates is a list of
  states of possible random generators. The variable
  currentStream indicates which of those generators is
  currently being used by R.
}
\examples{
mySeeds <- seedCreator(500, 5, file="mySeeds.rds", seed = 123123)
initPortableStreams(mySeeds, run = 17)
getCurrentStream()
useStream(2)
runif(2)
}
\author{
  Paul E. Johnson <pauljohn@ku.edu>
}

