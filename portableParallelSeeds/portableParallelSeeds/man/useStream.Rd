\name{useStream}
\alias{useStream}
\title{Selects among available random streams.}
\usage{useStream(n, origin=FALSE, verbose=FALSE)}
\description{Selects among available random streams.}
\details{R's global environment variable .Random.seed is re-set so that
random numbers generated after this call will follow from a
designated random number state. This will fail unless the
\code{initPortableStreams} function has already been called. This
function simply selects the n'th random stream for use,
presupposing that those stream states have already been set in the
global environment.

This should handle three chores.  1) Copy the existing state of
the random generator (.Random.seed) into
currentStates[[currentStream]]. That allows us to "go back" to
that generator when we want to.  2) Change currentStream variable
to n, then 3) Re-set R's .Random.seed variable to the value from
currentStates[[currentStream]], so that successively drawn random
numbers follow the proper generator.}
\value{Nothing. This function is run to change environment variables.}
\author{Paul E. Johnson <pauljohn@ku.edu>}
\arguments{\item{n}{An integer that selects which random stream should be used for the following work.}
\item{origin}{True or False. Should the stream be set at its original starting position, so as to re-generate the stream starting from the beginning? If FALSE, random numbers are drawn from the stream's current state.}
\item{verbose}{Requests detailed output for diagnostics.}}
