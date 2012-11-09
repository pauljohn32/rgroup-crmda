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
parms <- list("N" = 999, STDEE = 8, b0 = 0.1, b1 = 0.2, b2 = -0.1)

projSeeds <- seedCreator(nReps, streamsPerRep, seed = 123456)

## If we had only one set of values for parms, we would just
## run one batch of nReps simulations
serial1 <- lapply(1:nReps, runOneSimulation, projSeeds, parm = parms)

## Consider instead a parameter sweep, in which many different
## parametric considitions are applied to the same random data
## streams.

b0s <- c(0.05, 0.01, 0.2, 0.3)
b1s <- c(-0.1, 0, 0.1, 0.2)
b2s <- c(-0.2, -0.1, 0, 0.1, 0.2)

metaParms <- expand.grid(b0=b0s, b1=b1s, b2=b2s)
metaParms <- cbind( "N" = 999, STDEE = 1, metaParms)

r1 <- apply(metaParms, 1, function(onerow) {res <- lapply(1:nReps, runOneSimulation, projSeeds, parm = as.list(onerow)); list(onerow, res)})

length(r1)
## r1 is a big list of lists. It has one object per row of
## metaParms. For recordkeeping, I throw the metaParm row in as the
## first element in the output. See?
r1[[1]][[1]]
r1[[2]][[1]]
r1[[4]][[1]]

## and the 2nd element for each run is the result object, the thing
## we were thinking of as a whole simulation.

## Take the results for metaRow 1, extract the 4th simulation.
r1[[1]][[2]][[4]]

## Now replicate run 4 from that parameter collection
runone14 <- runOneSimulation(4, projSeeds, parm = as.list(metaParms[1,]))


all.equal(r1[[1]][[2]][[4]],  runone14)

## Yeah!


## Suppose we want to compare the estimates of the R square
## across the batches.  This function harvests the rsquare
## from a result object
getRsquares <- function(reslist){
    sapply(reslist, function(mod){mod[["m1sum"]]$r.square})
}

## Get Rsquare estimates for metaParam[5, ], for example:
rsqmod5 <- getRsquares(r1[[5]][[2]])
hist(rsqmod5)


## Get the Rsquares for all the metaParams combinations in one matrix

allRsq <- sapply(1:NROW(metaParms), function(x){ getRsquares(r1[[x]][[2]])})

## That's NROW(metaParams) columns, with nReps rows per.

op <- par(no.readonly = TRUE)
par(mfcol=c(2,2))
R2range <- c(min(allRsq), max(allRsq))
R2range <- R2range*c(0.9, 1.1) ##stretch that
hist(allRsq[ , 2], main="metaParams=2", xlim = R2range, xlab="Estimated R-square")
hist(allRsq[ , 5], main="metaParams=5", xlim = R2range, xlab="Estimated R-square")
hist(allRsq[ , 19], main="metaParams=19", xlim = R2range, xlab="Estimated R-square")
hist(allRsq[ , 77], main="metaParams=77", xlim = R2range, xlab="Estimated R-square"))
par(op)
