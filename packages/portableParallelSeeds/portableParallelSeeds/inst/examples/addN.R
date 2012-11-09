## Paul E. Johnson CRMDA <pauljohn@ku.edu>
## Portable Parallel Seeds Project.
## 2012-11-08

library(portableParallelSeeds)

runOneSimulation <- function(run, projSeeds, parm){
  initPortableStreams(projSeeds, run = run)
  dat <- data.frame(x1 = rnorm(parm$N))
  useStream(2)
  dat$x2 = rnorm(parm$N)
  useStream(3)
  dat$y <- with(dat, parm$b0 + parm$b1 * x1 + parm$b2 * x2 + parm$STDEE * rnorm(parm$N))
  m1 <- lm(y ~ x1 + x2, data = dat)
  list("m1" = m1, "m1sum" = summary(m1))
}


nReps <- 100
streamsPerRep <- 3
## Parms needed by each repetition
parms <- list("N" = 999, STDEE = 12, b0 = 0.1, b1 = 0.2, b2 = -0.1)

projSeeds <- seedCreator(nReps, streamsPerRep, seed = 123456)

serial1 <- lapply(1:nReps, runOneSimulation, projSeeds, parm = parms)


parms$N <- 2000

serial2 <- lapply(cl, 1:nReps, runOneSimulation, projSeeds, parm = parms)

## Need to verify that the first 999 rows (the original sample
## size) will match exactly.

(s1m1head <- head(model.matrix(serial2[[7]]$m1), n = 999))

(s1m1head <- head(model.matrix(serial1[[7]]$m1), n = 999))

identical(p1m1head, s1m1head)

