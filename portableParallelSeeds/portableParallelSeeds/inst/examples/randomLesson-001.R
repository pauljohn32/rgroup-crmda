## Paul Johnson
## 2012-10-24

## R's default random generator is MT19937.  This illustrates the
## internal state of MT19937 and discusses some ways to re-set that
## generator to a given initial position.
##

## First, note that the R random generator is NOT actually created
## yet. We cannot review its internal state:

.Random.seed

## However, setting the seed value, or running any command that
## attempts to draw on the random generator, will initialize
## the generator.

set.seed(12345)

## That re-sets the existing default generator to a known
## position. But its internal state is NOT 12345. It is, instead
## a "state" vector that can be derived from 12345.

## That number 12345, the thing we applied researchers call "the
## seed", is actually no a seed. It is just some integer we
## specify that R can use to construct the intial state of the
## random generator.

## Where to random draws come from? After you get the
## equally-likely integers from the generator, that is, where
## to random normals or poissons or gammas come from?

## It is especially easy to generate random numbers from a uniform
## distribution. All we have to do is take the randomly drawn positive
## integer (which the generator provides, because that's what
## generators do) and divide by the largest possible integer the
## generator can provide. This is such a common service that
## many generators will supply output within the [0,1] interval.
## However, my background in the Swarm toolkit makes me insist
## that a generator is a thing that gives integers (stubbornly)

runif(1)

## What is the internal state of the generator?  I literally mean,
## what numbers summarize the machine that can generate random
## numbers? Run this to see. Don't hate me if it fills up a page.

.Random.seed

## That's 626 numbers, it summarizes the internal state of
## this particular generator in R.

##
## As you can see from .Random.seed, its internal state is quite a
## complicated thing. Other random generator functions have shorter
## states, so study .Random.seed, but also try to stay flexible.

## Lets save a copy of that

s1 <- .Random.seed

## and look at just the first few.
s1[1:20]

## Study the first two numbers in the output. I see the first
## element is

## 403

## That's R's indicator for the type of the generator.  403 is really
## a "4" and an "03".  The 4 indicates the normal is simulated by
## "inversion" while the "03" indicates that the generator is MT19937.

## Look at the second number, I see

## 1

## That is a COUNTER. It is a marker peculiar to the way MT19937
## works.  It indicates that this particular vector has been used to
## draw one random number.


## MT19937 is updated in an interesting way. The MT19937 does not
## update the entire state vector every time it needs to generate a
## random number.  In fact, you can draw quite a few numbers and the
## only value that changes is the second one, the marker for how many
## values have been chosen.

runif(1)

s2 <- .Random.seed

## Draw another
runif(1)

s3 <- .Random.seed

cbind(s1, s2, s3)[1:20, ]

## Now draw a normal variable. There's actually a point here.

rnorm(1)

s4 <- .Random.seed

cbind(s1, s2, s3, s4)[1:20, ]

## There's a little bit of an interesting difference there.
## Note that when we draw a normally distributed variable,
## the 2nd element in the state increments by 2. That must
## mean that creating a normal variable requires 2 random
## numbers from the generator. (It does, I checked).

## As soon as we draw more random numbers--enough
## to cause the 2nd variable to increment past 624--
## then the WHOLE VECTOR changes.

invisible(runif(619))

## That positions the state right at the end, at 624
s5 <- .Random.seed
cbind(s4, s5)[1:10, ]


## Now, draw one more.
invisible(runif(1))
s6 <- .Random.seed

cbind(s1, s4, s5, s6)[1:10, ]
## See that? The whole thing snapped!

## Draw a few more random uniform values, you
## will see the counter start ticking up, inexorably,
## toward 624, when the whole thing blows up again.



## Please note, just for clarity, it is the same thing
## to simply view the contents of the internal state with the command
.Random.seed[1:10]
## as it is to use the get function:

get(".Random.seed", envir = .GlobalEnv)[1 : 10]

## This latter format is safer, however, because it
## leaves no mystery about which environment is storing
## the value .Random.seed.

## Do you want to force the generator back to an earlier state?

## You can. A command like

## set.seed(12345)

## will push it back to the original initial state.

## The counterpart of get() is assign(), which can
## place a value into an environment. Rather than
## assigning .Random.seed with <-, this way is safer.

assign("Random.seed", s1, envir=.GlobalEnv)

runif(1)


## I just noticed one more interesting thing about MT19937 and the way
## it is initialized.  Let's return the generator's state to the
## initial position so we can see what it is.

set.seed(12345)

.Random.seed[1:8]

## Ah. Running set.seed() pushes the counter variable, the 2nd element
## of the vector, to 624. That means that, when the next random number
## is requested, the whole state vector will update itself.

## Now, notice, that the random uniform we get is exactly
## the same one we got at the beginning of this program.
runif(1)

## How to I know the generator is MT19937?  Here's one way.
## Ask R:

RNGkind()

## From that, I get
## [1] "Mersenne-Twister" "Inversion"

## The first is MT19937, the second specifies the method of
## drawing values from a normal distribution. There are a lot
## of competing algorithms for simulating values from a normal,
## R uses 'inversion' as the default method.
