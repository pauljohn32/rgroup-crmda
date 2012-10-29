## Paul E. Johnson
## pauljohn@ku.edu 2012-10-29

## I carelessly lost the original writeup, but
## have reconstructed with less commentary.

## Change the generator type to the CMRG
## and study the internal state by looking at
## .Random.seed over and over

RNGkind("L'Ecuyer-CMRG")
set.seed(12345)
t0 <- .Random.seed
runif(1)
t1 <- .Random.seed
runif(1)
t2 <- .Random.seed
runif(1)
t3 <- .Random.seed
rnorm(1)
t4 <- .Random.seed
cbind(t1, t2, t3, t4)

## Here is the really important working bit of the
## "many substreams" approach of L'Ecuyer, et al.
## It was offered in R's new parallel package for
## the first time in version 2.14.

require(parallel) ## for nextRNGStream
substreams <- vector("list", 5)
substreams[[1]] <- t0
substreams[[2]] <- nextRNGStream(t0)
substreams[[3]] <- nextRNGStream(substreams[[2]])
substreams[[4]] <- nextRNGStream(substreams[[3]])
substreams[[5]] <- nextRNGStream(substreams[[4]])
substreams

## Each of those can be used by assign() to
## initialize the random generator at far-apart values
## of the really long random vector (thus giving us
## substreams that we know are separate).

## Here's a fun fact. rnorm takes two values from the
## generator, but runif and rpois only take one.


## Why would this be interesting?  Try this:

set.seed(12345)
x1 <- runif(10)
x2 <- rpois(10, lambda=7)
x3 <- runif(10)

set.seed(12345)
y1 <- runif(10)
y2 <- rnorm(10)
y3 <- runif(10)

identical(x1, y1) ## yeah
identical(x2, y2) ## duh!
identical(x3, y3) ## oh, no!

## Shouldn't x3 and y3 be the same! This is baaadddd.

## why did that happpen? The rnorm knocks twice at the generator's door.

## That problem is easiest to see with MT19937

RNGkind("Mersenne-Twister")
set.seed(12345)
invisible(runif(1)); s1 <- .Random.seed
invisible(runif(1)); s2 <- .Random.seed
invisible(runif(1)); s3 <- .Random.seed
invisible(rnorm(1)); s4 <- .Random.seed
cbind(s1, s2, s3, s4)[1:8, ]

##Look at the counter, in the 2nd line. The
## step from s3 to s4 gobbles up 2 values from
## the generator.

## Now re-start same generator sequence, but
## draw 5 uniforms.

assign(".Random.seed", t1, envir=.GlobalEnv)
u1 <- .Random.seed
invisible(runif(1))
u2 <- .Random.seed
invisible(runif(1))
u3 <- .Random.seed
invisible(runif(1))
u4 <- .Random.seed
invisible(runif(1))
u5 <- .Random.seed
cbind(u1, u2, u3, u4, u5)

## Things to note. Given what I've said above, this:
## set.seed(12345)
##
## is the same as this
##
## .Random.seed <- t1
##
## is the same as
##
## assign(.Random.seed, t1, envir=.GlobalEnv)

## note the weirdness that rgamma may draw 2,3, or more
## numbers from the generator. Easy to see with Mersenne-Twister.

RNGkind("Mersenne-Twister")
set.seed(12345)
invisible(rgamma(1, shape = 1)); v1 <- .Random.seed[1:4]
invisible(rgamma(1, shape = 1)); v2 <- .Random.seed[1:4]
invisible(rgamma(1, shape = 1)); v3 <- .Random.seed[1:4]
invisible(rgamma(1, shape = 1)); v4 <- .Random.seed[1:4]
invisible(rgamma(1, shape = 1)); v5 <- .Random.seed[1:4]
invisible(rgamma(1, shape = 1)); v6 <- .Random.seed[1:4]
cbind(v1, v2, v3, v4, v5, v6)

