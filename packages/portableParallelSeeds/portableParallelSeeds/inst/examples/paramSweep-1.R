## Paul E. Johnson CRMDA <pauljohn@ku.edu>
## Portable Parallel Seeds Project.
## 2012-11-15

## Trying to develop standard "idioms" for Monte Carlo simulation
## of sampling distributions (which is an extremely popular topic
## among my students).

library(portableParallelSeeds)

## genData uses mvrnorm from portableParallelSeeds, which
## means that if you re-set the random stream at the origin
## and draw N cases, and then draw N+k cases, the first N
## rows will be the same in each dataframe.
genData <- function(run, seeds, parm){
    initPortableStreams(seeds, run = run)
    dat <- portableParallelSeeds::mvrnorm(parm$N, mu=c(0,0), Sigma=diag(2))
    dat <- as.data.frame(dat)
    names(dat) <- c("x1", "x2")
    useStream(2)
    dat$err <- rnorm(parm$N, sd = parm$STDEE)
    dat$y <- with(dat, parm$b0 + parm$b1 * x1 + parm$b2 * x2 + err)
    dat
}


analyzeData <- function(dat, parm){
    m1 <- lm(y ~ x1 + x2, data = dat, model = FALSE)
    c1 <- coef(m1)
    m1sum <- summary(m1)
    bias <- c1 - unlist(parms[c("b0","b1","b2")])
    bias  <- c(bias, m1sum$sigma - parm[["STDEE"]])
    names(bias) <- c("b0", "b1", "b2", "stdee")
    list("m1" = m1, "m1sum" = m1sum, "bias" = bias)
}


## Each simulation requires a run number, a seeds object, and a
## parameter list. The first argument MUST be the run number,
## otherwise the functions that access this won't make sense.
runOneSimulation <- function(run, seeds, parm){
    if(missing(seeds)) stop("runOneSimulation: seeds argument missing")
    if(missing(parm))  stop("runOneSimulation: parm argument is missing")
    dat <- genData(run, seeds, parm)
    res <- analyzeData(dat, parm)
}

## Repeat one simulation over and over, "n" times.
## Returns a list with 2 objects. The first is for
## record keeping, it has the parameters that were used
## and the run number. Very similar to R's own "replicate" function here.
## The only difference is we are more general in the return information.
## We have better book keeping, and the return from each simulation run
## need not be a single vector.
oneBatch <- function(parm, n, simFunction, seeds){
    res <- lapply(1:n, simFunction, seeds, parm = as.list(parm));
    list(parm, res)
}
## I've put parm as the first argument for a particular reasons. In
## functions like apply, it is much easier to get the iterations to
## work right if the parm argument is first. You'll see.



### Now apply these functions ####


nReps <- 100
streamsPerRep <- 3
## Create the seed warehouse
projSeeds <- seedCreator(nReps, streamsPerRep, seed = 123456)

## Parms needed by a run
parms <- list("N" = 999, "STDEE" = 8, "b0" = 0.1, "b1" = 0.2, "b2" = -0.1)

##
## run one batch of nReps simulations

serial1 <- oneBatch(parm = parms, n = nReps, simFunction = runOneSimulation, seeds = projSeeds)

## serial1 holds results for 100 regressions.

## Now, consider the problem that most of my students want to
## manage. We want to have several possible "parms" objects,
## and run lots of replications of each one. For example,
## here are possible values of the parms.

b0s <- c(0.05, 0.01, 0.2, 0.3)
b1s <- c(-0.1, 0, 0.1, 0.2)
b2s <- c(-0.2, -0.1, 0, 0.1, 0.2)

## In this example, we have only one value for N and STDEE, but we
## have a set of possibles for b0, b1, and b2.  Now I show a way to
## "mix and match" all possibilities.
metaParms <- expand.grid(N = 999, STDEE = 1, b0 = b0s, b1 = b1s, b2 = b2s)
## review metaParms, notice it has one row for each possible combination
## of all the parameters. Here's the first few
##
head(metaParms)

## Now we ask for oneBatch for eachrow of parameters
   ## Use option cl.core to choose an appropriate cluster size.
     cl <- makeCluster(getOption("cl.cores", 2))

r1 <- apply(metaParms, 1, oneBatch, n = nReps, simFunction = runOneSimulation, seeds = projSeeds)

length(r1)
## r1 is a big list of lists. It has one object per row of
## metaParms. For recordkeeping, I throw the metaParm row in as the
## first element in the output. See?
r1[[1]][[1]]
r1[[2]][[1]]
r1[[4]][[1]]

## and the 2nd element for each run is the result object, the thing
## we were thinking of as a whole simulation.

## Take the  metaRow 1, grab the results, extract the 4th simulation.
r1[[1]][[2]][[4]]

## Now replicate run 4 from that parameter collection
runone14 <- runOneSimulation(4, projSeeds, parm = as.list(metaParms[1,]))


all.equal(r1[[1]][[2]][[4]],  runone14)

## Yeah! I can run a whole batch, or replicate just one piece.


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
hist(allRsq[ , 2], main="metaParams row 2", xlim = R2range, xlab="Estimated R-square")
hist(allRsq[ , 5], main="metaParams row 5", xlim = R2range, xlab="Estimated R-square")
hist(allRsq[ , 19], main="metaParams row 19", xlim = R2range, xlab="Estimated R-square")
hist(allRsq[ , 77], main="metaParams row 77", xlim = R2range, xlab="Estimated R-square")
par(op)
