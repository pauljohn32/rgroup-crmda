## Paul E. Johnson CRMDA <pauljohn@ku.edu>
## Portable Parallel Seeds Project.
## 2012-11-08


library(parallel)
library(portableParallelSeeds)
##RNGkind("L'Ecuyer-CMRG")
set.seed(234234)


clusterEvalQ(cl, {
  library(parallel); library(portableParallelSeeds)
})



runOneSimulation <- function(run, projSeeds, parm){
  initPortableStreams(projSeeds, run = run)
  dat <- data.frame(x1 = rnorm(parm$N), x2 = rnorm(parm$N))
  useStream(2)
  dat$y <- 0.1 + 0.2 * x1 -0.04* x2 + parm$STDEE * rnorm(parm$N)
  m1 <- lm(y ~ x1 + x2, data = dat)
  list(m1, summary(m1))
}


nReps <- 1000
streamsPerRep <- 2
## Parms needed by each repetition
parms <- list("N" = 999, STDEE = 12)

projSeeds <- seedCreator.R(nReps, streamsPerRep, seed = 123456, file = "seeds.rds")





serial1 <- lapply(1:nReps, runOneSimulation, projSeeds, parm = parms)




cl <- makeCluster(9, "MPI")

clusterEvalQ(cl, {
  library(parallel); library(portableParallelSeeds)
})



parallel1 <- parLapply(cl, 1:nReps, runOneSimulation, projSeeds, parm = parms)

identical(serial1[[7]], parallel1[[7]])

##Prove I can repeat 7'th task in isolation

res7 <- someHorrendousFunction(7, projSeeds, parm = whatever)

identical(parallel1[[7]], res7)

## well, that worked.

parallel2 <-  snow:::clusterApplyLB(cl, 1:nReps, runOneSimulation, projSeeds, projParms)


identical(parallel2[[7]], parallel1[[7]])


library(snow)
stopCluster(cl)
mpi.quit()

