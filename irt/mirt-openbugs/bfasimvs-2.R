
writeDataFiles <- function(bf.sim, re, nitems, mina, maxa ){
  ##print true item and person parameters and item responses to .txt files
  write.table(cbind("dim", "discs", "diff", "guessing"),
              file = paste("itemtrue", re, ".txt", sep = ""),
              append = F, sep  = "  ", row.names = F, col.names = F)
  
  write.table(cbind(round(bf.sim[[1]], digits = 0), round(bf.sim[[2]],
                                                          digits = 3), round(bf.sim[[3]], 3), round(bf.sim[[4]], 3)), file =
                                                            paste("itemtrue", re, ".txt", sep = ""),append = T, sep = "\t",
              row.names = F, col.names = F)
  
  theta1 <- bf.sim[[5]]
  save(theta1, file = paste("thetatrue",re,".rda", sep=""))          
  
  write.table(bf.sim[[6]], file = paste("ir", re, ".txt", sep = ""), row.names = F,col.names = F)
}



writeBUGSModel <- function(re, nitems, nE, nD, mina, maxa){
  bug.model <- ' model
 {
  for (i in 1:nE) {
  theta[i, 1:nD] ~ dmnorm(mu[], SIG[, ])
  }
  for (j in 1:nitems) {
  strctr[j, 1] <- 1
  b[j] ~ dnorm (0.00, pr.b) Trnct(-3,3)
  g[j] ~ dbeta(20, 80)
  for (k in 2:nD) {
  strctr[j, k] <- equals(k, st[j])
  }
  for (k in 1:nD) {
  temp[j, k] ~ dnorm(0.00, pr.a)  Trnct(0.25, 1.75)
  a[j, k] <- (strctr[j, k]+0.000001) * temp[j, k]
  }
  }
  
  pr.b <- pow(1.0,-2)
  pr.a <- pow(sig.a, -2)
  for (i in 1:nE) {
  for (j in 1:nitems) {
  for (k in 1:nD) {
  pdim[i, j, k] <- a[j, k] * (theta[i, k] - b[j])
  }
  p1[i, j] <- phi(sum(pdim[i, j, 1:4]))
  p[i, j] <- g[j] + (1 - g[j]) * p1[i, j]
  r[i, j] ~ dbern(p[i, j])
  }
  }
 }
  
  '

  ## replace texts with values
  bug.model <- gsub( "nE", nE, bug.model)
  bug.model <- gsub( "nD", nD, bug.model)
  bug.model <- gsub( "nitems", nitems, bug.model)
  ##replace sig.a with uniform sd
  sigma <- round(sqrt(((maxa - mina)^2)/12), 3)
  bug.model <- gsub( "sig.a", sigma, bug.model)
  meana <- (maxa - mina)/2
  bug.model <- gsub( "m.a", meana, bug.model)
  bug.model <- gsub( "Trnct", "T", bug.model)
  
  cat(bug.model, file = "bifactor.txt")
  
}


writeBUGSFiles <- function(bf.sim, re, nD, n.chains, nE=NULL, n.iter=1000, 
                           n.burnin=300, n.thin=1){
                           
  require(rbugs)
  parameters <- c("a", "b", "g", "theta")      
  model.file <- file.path(getwd(), "bifactor.txt")
  data <- list(mu = rep(0,nD), SIG = diag(nD), st = bf.sim[[1]], r = bf.sim[[6]])
  inits <- function() {
    list(a = mean(bf.sim[[2]]),
         b = mean(bf.sim[[3]]),
         g = mean(bf.sim[[4]]),
         theta = rmvnorm(nE, c(rep(0,nD)), diag(nD)))
  }
  n.update <- (n.iter - n.burnin)
  n.beg <- n.burnin + 1
  ##write Bugs data into a file
  data.file <- file.path(getwd(), "data.txt")
  genDataFile(data, data.file)
  ##create/generate initials values file(s)
  n.chains = n.chains
  inits.files <- file.path(getwd(), c(paste("inits_", re,"-", 1:n.chains, ".txt", sep = "")) )
  genInitsFile(n.chains, inits, initsFileStem = paste("inits_", re, "-", sep =""))      #need rbugs package
  
  ##write BUGS script file for dos-/bacth-mode OpenBUGS
  script.file <- file.path(getwd(), "script.txt") 
  
  bug.script <- 'modelDisplay(lg)
  modelCheck(model.file)
  modelData(data.file)
  modelCompile(n.chains)
  modelSetRN(1)
  modelGenInits()
  modelUpdate(n.burnin,n.thin,n.burnin)
  samplesSet(a)
  samplesSet(b)
  samplesSet(g)
  samplesSet(theta)
  samplesSet(deviance)
  summarySet(a)
  summarySet(b)
  summarySet(g)
  summarySet(theta)
  summarySet(deviance)
  dicSet()
  modelUpdate(n.update,n.thin,n.update)
  summaryStats(star)
  dicStats()
  samplesHistory(star)
  modelSaveLog(logodc)
  modelSaveLog(logtxt)
  modelQuit(yes)
  
 '
  bug.script <- gsub( "lg", paste("'log'", sep=""), bug.script)
  bug.script <- gsub( "model.file", paste("'", model.file,"'", sep=""), bug.script)
  bug.script <- gsub( "data.file", paste("'",data.file,"'", sep=""), bug.script)
  bug.script <- gsub( "n.chains", n.chains, bug.script)
  bug.script <- gsub( "n.burnin", n.burnin, bug.script)
  bug.script <- gsub( "n.update", n.update, bug.script)
  bug.script <- gsub( "n.thin", n.thin, bug.script)
  bug.script <- gsub( "star", paste("'*'", sep=""), bug.script)
  logodc.file <- file.path(getwd(), "log.odc")
  logtxt.file <- file.path(getwd(), "log.txt")
  bug.script <- gsub( "logodc", paste("'",logodc.file,"'", sep=""), bug.script)
  bug.script <- gsub( "logtxt", paste("'",logtxt.file,"'", sep=""), bug.script)
  bug.script <- gsub( "yes", paste("'yes'", sep=""), bug.script)
    
  cat(bug.script, file = "script.txt")                  
  
  list(dat = data.file, init = inits.files, script = script.file)
}

runOBugs <- function (bugs = system("which OpenBUGS", TRUE), script, n.chains, 
          workingDir, OpenBugs = TRUE, Windows = TRUE, verbose = FALSE) 
{
  if (Windows) {
    if (is.na(pmatch("\"", bugs))) 
      bugs <- paste("\"", bugs, "\"", sep = "")
    if (is.na(pmatch("\"", script))) 
      script <- paste("\"", script, "\"", sep = "")
    command <- paste(bugs, "/par", script)
  }
  else {
    log.file <- file.path(workingDir, "log.txt")
    command <- paste(bugs, "<", script, ">", log.file)
  }
  #fnames <- getCodaFileNames(n.chains, workingDir, OpenBugs)
  #coda.files <- c(fnames$codaIndexFile, fnames$codaFiles)
  #for (i in coda.files) {
  #  if (file.exists(i)) 
  #    file.remove(i)
  #}
  log.file <- file.path(workingDir, "log.txt")
  if (file.exists(log.file)) 
    file.remove(log.file)
  cont <- 0
  err <- -1
  while ((cont < 2) & (err != 0)) {
    cont <- cont + 1
    err <- system(command)
  }
  if (err != 0) 
    stop("System call to BUGS failed.")
  if (verbose) 
    file.show(log.file)
  #if (!file.exists(coda.files[1])) {
  #  if (Windows) 
   #   warning("Number of iterations may be to small")
  #  stop("BUGS stopped before getting to coda.")
  #}
}



runOpenbugs <- function(script, n.chains)
{

  #--------Execute a BUGS Srcipt from R and run it------ (NEW)
  bfcommirt.sim <- runOBugs(bugs = "/usr/bin/OpenBUGS",
                  script = script, n.chains = n.chains,
                  workingDir = getwd(), OpenBugs = TRUE, Windows=TRUE,
                  verbose = FALSE)
  
  #---windows---
  #bfcommirt.sim <- runOBugs(bugs="C:/OpenBUGS/OpenBUGS321/OpenBugs.exe",
  #                    script = script, n.chains = n.chains,
  #                    workingDir = getwd(), OpenBugs = TRUE, Windows=TRUE,
  #                    verbose = FALSE) 
  
}


bfgena <- function(re = 1, nE = 100, nitems = 30, nD = 4, mina = .75,
                   maxa = 1.25, currentSeeds=NULL) {
  require(msm)
  require(mvtnorm)
  
  ## nested function to perform logit transform with guessing.
  Pra <- function(thet = matrix(0), a = matrix(0), b = 0, g = 0) {         
    g+(1-g)*(1/(1+exp(sum((-a)*(thet - b)))))
  }
  ## set.seed(15937)   ##seed fixed (same group of examinees)
  useStream(1, origin=TRUE)
  mu <- matrix(0, 1, nD) #mean for theta matrix
  SIG <- diag(nD)      #identity matrix for var-cov matrix of theta
  
  theta1 <- rmvnorm(nE, mu, SIG)
  
  
  ##to allow for diff in nE, nitems, nD, mina and maxa
  niD <- nitems/(nD-1) #items per specific dimension
  
  a <- matrix(0, nitems, nD)
  st <- vector(nitems, mode="numeric")
  b <- vector(nitems, mode="numeric")
  g <- rep(0.2, nitems) # fix guessing for each item at 0.2
  
  useStream(2, origin=TRUE)
  #set.seed(7315 + re)           ## seed for a,b and g parameters is set up with an increment of re
  for (d in 1:nD){
    if (d == 1) {
      a[ ,d] <- runif(nitems, min = 0.75, max = 1.25)   # disc for primary is fixed to have mean = 1.0
      st[1:niD] <- 1
    } else {  # assigns to specified dimensions
      k1 = ((d-2)*niD)+1
      k2 = (d-1)*niD  ## k1=((2-2)10)+1=1 < k2=(2-1)10=10 , k1=((3-2)10)+1=11 < k2=(3-1)10=20 , k1=((4-2)10)+1=21 < k2=(4-1)10=30
      a[k1:k2 , d] <- runif( niD, min = mina, max = maxa)   
      st[k1:k2] <- d
    }
  }
  
  b <- rtnorm(nitems, m = 0, sd = 1, lower = -3.0, upper = 3.0)   #fixed b to have N(0,1) with truncation between -2.75 and 2.75
  
  Xa <- matrix(0, nE, nitems)
  
  
  useStream(3, origin=TRUE)
  ###Caution: Following is tedious. Could be done with matrix algebra in 1 step.
  for (p in 1:nE) {
    for (i in 1:nitems) {
      Xa[p, ] <- ifelse (runif(nitems,0,1) < Pra(theta1[p, 1: nD], a[i, 1: nD], b[i], g[i]), 1, 0)
    }
  }
  list(st, a, b, g, theta1, Xa)
  
} #end bfgena




################################Correct the part above, never
################################Look at it again.
################################Here's the part you
################################you should look at
################################when this gets used

runOneSimulation <- function(re, nitems=NULL, nE=NULL, mina=NULL, maxa=NULL, nD=NULL, n.chains=NULL, n.iter=1000, n.burnin = 300, n.thin= 1){
  initSeedStreams(re)
  olddir <- getwd()
  workdir <- paste("batch", nitems, mina, maxa, re, sep="-")
  dir.create(workdir, showWarnings = TRUE, recursive = TRUE)
  setwd(workdir)
  
  bf.sim <- bfgena(re = re, nE = nE, nitems = nitems, nD = nD, mina = mina, maxa = maxa, currentSeeds=currentSeeds)
  writeBUGSModel(re, nitems, nE, nD, mina, maxa )
  writeDataFiles(bf.sim, re, nitems, mina, maxa )
  bugsfiles <- writeBUGSFiles(bf.sim, re, nD, n.chains, nE , n.iter, 
                              n.burnin, n.thin )
  res <- runOpenbugs(bugsfiles$script, n.chains)
  
  #t1 <- try(system(paste("OpenBUGS ", bugsfiles$script, " > results.txt && gzip *.txt")))
  
  #runBugs( script=  bugsfiles$script )
  
  setwd(olddir)
  #t1
}


library(parallel)

## set seed here in case any random draws are called
## for by user code.

RNGkind("L'Ecuyer-CMRG")
set.seed(234234)


load("projSeeds.rda")



## Copy .Random.seed into currentSeeds[[currentStream]]
## Get change currentStream to n, then grab that seed
## and put it into .Random.seed
useStream <- function(n = NULL, origin = FALSE){
  oldseed <-
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  else stop("in useStream, .Random.seed was NULL")
  ## get local copies of currentStream, currentSeeds, startSeeds
  curStream <- get("currentStream", envir = .GlobalEnv, inherits = FALSE)
  curSeeds <- get("currentSeeds", envir = .GlobalEnv, inherits = FALSE)
  
  if (n > length(curSeeds)) stop("requested stream does not exist")
  curSeeds[[curStream]] <- oldseed
  if (origin) {
    strtSeeds <- get("startSeeds", envir = .GlobalEnv, inherits = FALSE)
    assign(".Random.seed", strtSeeds[[n]], envir = .GlobalEnv)
  } else {
    assign(".Random.seed", curSeeds[[n]], envir = .GlobalEnv)
  }
  ## put currentStream and currentSeeds back to .GlobalEnv
  assign("currentStream", n, envir = .GlobalEnv)
  assign("currentSeeds", curSeeds, envir = .GlobalEnv)
}


## will run on worker, so can retrieve seeds for particular run
initSeedStreams <- function(rep = NULL){
  repSeeds <- projSeeds[[rep]]
  assign("currentStream",  1L, envir = .GlobalEnv)
  assign("startSeeds", repSeeds, envir = .GlobalEnv)  
  assign("currentSeeds", repSeeds, envir = .GlobalEnv)
  assign(".Random.seed", repSeeds[[1L]],  envir = .GlobalEnv)
}






nE <- 1500
nitems <- 30
nD <- 4           ## Levels of discrimination in the secondary dimension
mina <- 1.25      ## .25, .50, .75, 1.00, 1.25
maxa <- 1.75      ## .75, 1.00, 1.25, 1.50, 1.75
n.chains <- 2
n.iter <- 10000
n.burnin <- 3000
n.thin <- 1

## To test this out, run this. Does not require cluster framework.
for (re in 1: 2) {
res <- runOneSimulation(re = re, nitems=nitems, nE=nE, mina=mina, maxa=maxa, nD=nD, n.chains=n.chains, n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin)
}



nReps <- 20 


cl <- makeCluster(19, "MPI")

clusterEvalQ(cl, {
  RNGkind("L'Ecuyer-CMRG")
} )

clusterExport(cl, c("projSeeds", "useStream", "initSeeds"))

clusterExport(cl, c("Pra", "writeDataFiles", "writeBUGSModel", 
"writeBUGSFiles", "bfgena"))

#res <- parLapply(cl, 1:nReps, runOneSimulation, nitems=nitems, nE = nE,  mina = mina, maxa = maxa, nD = nD, n.chains = n.chains )

res <- snow:::clusterApplyLB(cl, 1:nReps, runOneSimulation, nitems=nitems, nE = nE,  mina = mina, maxa = maxa, nD = nD, n.chains = n.chains )

res

library(snow)
stopCluster(cl)
mpi.quit()

