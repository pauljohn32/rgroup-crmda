## Paul E. Johnson CRMDA <pauljohn@ku.edu>
## Portable Parallel Seeds Project.
## 2012-11-08


library(parallel)
library(portableParallelSeeds)
##that causes environment to be set like: RNGkind("L'Ecuyer-CMRG")


runOneSimulation <- function(run, projSeeds, parm){
  initPortableStreams(projSeeds, run = run)
  dat <- data.frame(x1 = rnorm(parm$N), x2 = rnorm(parm$N))
  useStream(2)
  dat$y <- with(dat, 0.1 + 0.2 * x1 -0.04* x2 + parm$STDEE * rnorm(parm$N))
  m1 <- lm(y ~ x1 + x2, data = dat)
  list("m1" = m1, "m1sum" = summary(m1))
}


nReps <- 100
streamsPerRep <- 2
## Parms needed by each repetition
parms <- list("N" = 999, STDEE = 12)

projSeeds <- seedCreator(nReps, streamsPerRep, seed = 123456, file = "seeds.rds")

serial1 <- lapply(1:nReps, runOneSimulation, projSeeds, parm = parms)


cl <- makeCluster(4, "MPI")

clusterEvalQ(cl, {
  library(parallel); library(portableParallelSeeds)
})



parallel1 <- parLapply(cl, 1:nReps, runOneSimulation, projSeeds, parm = parms)

## Note the results are the same:

serial1[[7]]

parallel1[[7]

all.equal(coef(serial1[[7]][[1]]), coef(parallel1[[7]][[1]]))

all.equal(serial1[[7]][[2]], parallel1[[7]][[2]])

##Prove I can repeat 7'th task in isolation

res7 <- runOneSimulation(7, projSeeds, parm = parms)

all.equal(parallel1[[7]]$m1, res7$m1)

## well, that worked.

parallel2 <-  snow:::clusterApplyLB(cl, 1:nReps, runOneSimulation, projSeeds, parms)


all.equal(parallel2[[7]]$m1, parallel1[[7]]$m1)


## Now increase the sample size. The simulation should give exactly the same
## data for rows 1:1000, while giving new data for 1001:2000.

parms$N <- 2005

parallel1.2005 <- parLapply(cl, 1:nReps, runOneSimulation, projSeeds, parm = parms)

head(model.matrix(parallel1.2005[[7]]$m1))

head(model.matrix(serial1[[7]]$m1)


library(snow)
stopCluster(cl)
mpi.quit()

