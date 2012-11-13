## Paul E. Johnson CRMDA <pauljohn@ku.edu>
## Portable Parallel Seeds Project.
## 2012-11-13

## Note: If one wants to experiment with various sample sizes, I
## suggest that the best approach is to draw the largest set, and then
## THROW AWAY lines to manufacture the smaller subsets.

## This example shows it is possible to re-draw datasets that replicate the
## first block of lines. It is important to be carefult in designing the
## runOneSimulation function. It is VITAL to separate the streams of
## separate random number draws.

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

serial2 <- lapply(1:nReps, runOneSimulation, projSeeds, parm = parms)

## Need to verify that the first 999 rows (the original sample
## size) will match exactly.

serial1head <- head(model.matrix(serial1[[7]]$m1), n = 999)
serial2head <- head(model.matrix(serial2[[7]]$m1), n = 999)

identical(serial1head, serial2head)

## Now verify can draw smaller set as well
parms$N <- 500

serial3 <- lapply(1:nReps, runOneSimulation, projSeeds, parm = parms)

## Need to verify that the first 500 rows against the others.

serial1head <- head(model.matrix(serial2[[7]]$m1), n = 500)
serial2head <- head(model.matrix(serial2[[7]]$m1), n = 500)
serial3head <- head(model.matrix(serial3[[7]]$m1), n = 500)


identical(serial1head, serial2head)
identical(serial2head, serial3head)

## Inspect regression from run 32 in set 3
serial3[[32]]["m1sum"]

## Take subset lines 1:500 from run 32 in set 2
replicate2_32 <- lm(y ~ x1 + x2, data =  model.frame(serial2[[32]][["m1"]]), subset=1:500)

summary(replicate2_32)

## looks the same to me, lets get specific on estimate matrices
all.equal(coef(summary(replicate3_32)), coef(serial3[[32]][["m1sum"]]))


