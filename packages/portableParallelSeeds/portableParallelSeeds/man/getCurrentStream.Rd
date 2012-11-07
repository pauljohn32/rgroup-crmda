\name{getCurrentStream}
\alias{getCurrentStream}
\title{Return integer representing currently selected random stream...}
\description{Return integer representing currently selected random stream}
\details{The environment variable currentStates is a list of states
of possible random generators. The variable currentStream
indicates which of those generators is currently being used
by R.}
\value{Integer index value of currently selected stream}
\author{Paul E. Johnson <pauljohn@ku.edu>}
\examples{mySeeds <- seedCreator(500, 5, file="mySeeds.rds", seed = 123123)
initPortableStreams(mySeeds, run = 17)
getCurrentStream()
useStream(2)
runif(2)}
