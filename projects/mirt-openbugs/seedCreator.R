library(parallel)

RNGkind("L'Ecuyer-CMRG")
set.seed(23456)

## nrep = number of repetitions (or tasks)
## streamsPerRep = number of streams needed by each repetition
nReps <- 500
streamsPerRep <- 3 

## projSeeds=list of lists of stream seeds
projSeeds <- vector(mode="list", nReps)
for (i in 1:nReps) projSeeds[[i]] <- vector(mode="list", streamsPerRep)

runif(1) ##establishes .Random.seed
##Grab first seed
s <- .Random.seed
origSeed <- s

for (i in 1:nReps) {
  projSeeds[[i]][[1]] <- origSeed
  for (j in 2:streamsPerRep){
     s <- nextRNGStream(s)
    projSeeds[[i]][[j]] <- s
  }
}


save(nReps, streamsPerRep, projSeeds, file="projSeeds.rda")

