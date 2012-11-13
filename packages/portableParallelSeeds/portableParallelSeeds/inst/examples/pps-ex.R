library(portableParallelSeeds)

projSeeds <- seedCreator(2000, 3, seed = 123456, file = "fruits.rds")
A1 <- projSeeds[[787]]
A1 ## shows states of 3 generators for run 787


initPortableStreams(projSeeds, run = 787, verbose = TRUE)
.Random.seed
getCurrentStream()
runif(4)

## read from file, take run 787's seed
myFruitySeeds <- readRDS("fruits.rds")
B1 <- myFruitySeeds[[787]]

identical(A1, B1) # check


initPortableStreams("fruits.rds", run=787)
.Random.seed
runif(4)


runOneSimulation <- function(run, streamsource, N, m, sd){
    initPortableStreams(streamsource, run = run, verbose= FALSE)
    datX <- rnorm(N, mean = m, sd = sd)
    datXmean <- mean(datX)
    useStream(2)
    datY <- rpois(N, lambda = m)
    datYmean <- mean(datY)
    useStream(1)
    datXplusOne <- rnorm(1, mean = m, sd = sd)
    ## Should be N+1'th element from first stream
    c("datXmean" = datXmean, "datYmean" = datYmean, "datXplusOne" = datXplusOne)
}


## Give seed collection object to each simulation, let each pick desired seed
serial1 <- lapply(1:1000, runOneSimulation, projSeeds, N=800, m = 14, sd = 10.1)


## First re-load the seed object, then give to simulations
fruits2 <- readRDS("fruits.rds")
serial2 <- lapply(1:1000, runOneSimulation, fruits2, N=800, m = 14, sd = 10.1)


## Re-load file separately in each run
serial3 <- lapply(1:1000, runOneSimulation, "fruits.rds", N = 800, m = 14, sd=10.1)
identical(serial1, serial2)
identical(serial1, serial3)

## Next question. Is the 801'th random normal from stream 1 equal to
## the 3'rd element in the returned vector? Lets check run 912

initPortableStreams("fruits.rds", run = 912, verbose = FALSE)
(X801 <- rnorm(801, m=14, sd = 10.1)[801])

## Look at the 801'th element of X801

X801

## Compare with the 3rd element in serial1's 912th element
serial1[[912]]

##Bingo. I'm right. Can draw a understandably replicatable streams of
## random numbers, whether we draw 800, switch to a different stream,
## and then change back to draw another, or if we just draw 801 in one
## series.

unlink("fruits.rds") #delete file
