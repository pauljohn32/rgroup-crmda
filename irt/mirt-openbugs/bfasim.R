
Pra <- function(thet = matrix(0), a = matrix(0), b = 0, g = 0) {         ## will be used for item responses in bfgena.sim
  g+(1-g)*(1/(1+exp(sum((-a)*(thet - b)))))
}

writeDataFiles <- function(bf.sim, re, nitems, mina, maxa ){
                                        #print true item parameters to .txt file
  write.table(cbind("dim", "discs", "diff", "guessing"),
              file = paste("itemtrue", re, ".txt", sep = ""),
              append = F, sep  = "  ", row.names = F, col.names = F)

  write.table(cbind(round(bf.sim[[1]], digits = 0), round(bf.sim[[2]],
                                         digits = 3), round(bf.sim[[3]], 3), round(bf.sim[[4]], 3)), file =
              paste("itemtrue", re, ".txt", sep = ""),append = T, sep = "\t",
              row.names = F, col.names = F)

  theta1 <- bf.sim[[5]]
  save(theta1, file = paste("thetatrue",re,".rda", sep=""))          ## DD: 1/6/12

  write.table(bf.sim[[6]], file = paste("ir", re, ".txt", sep = ""), row.names = F,col.names = F)
}



writeBUGSModel <- function(re, nitems, nE, nD, mina, maxa){
 ## edited: prior for guessing beta(20,80) and discrimination dnorm(0.00, pr.a)  I(0.00, )

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

 ## replace text "nE" with value nE
 bug.model <- gsub( "nE", nE, bug.model)
 ## replace nD
 bug.model <- gsub( "nD", nD, bug.model)
 ## replace nitems
 bug.model <- gsub( "nitems", nitems, bug.model)
 ##replace tau with maxa - tau = (b-mean/sd, N(mean,sd), normal truncated below b.
 sigma <- round(sqrt(((maxa - mina)^2)/12), 3)
 bug.model <- gsub( "sig.a", sigma, bug.model)
 meana <- (maxa - mina)/2
 bug.model <- gsub( "m.a", meana, bug.model)
 bug.model <- gsub( "Trnct", "T", bug.model)

 cat(bug.model, file = "bifactor.txt")

}

writeBUGSFiles <- function(bf.sim, re, nD, n.chains, nE=NULL){
  require(rbugs)
  r <- bf.sim[[6]]
  mu <- c(rep(0,nD))
  SIG <- diag(nD)
  st <- bf.sim[[1]]
  parameters <- c("a", "b", "g", "theta")      ## AIC and BIC are added 1/14/12

  model.file <- file.path(getwd(), "bifactor.txt")  ## DD: 1/6/12

  ##write Bugs data into a file
  data.file <- file.path(getwd(), paste("data", re, ".txt", sep = ""))
  ## DD: 1/6/12 - don't need nE, nD and nitems since they are written out in the writeBUGSModel()
  data <- list(mu = mu, SIG = SIG, st = st, r = r)
  #bugsData(data, fileName = data.file, digits = 5)   ## or from BRugs package
  genDataFile(data, data.file)

  ##format4Bugs(data, digits = 5)
  ##create/generate initials values file(s)
  n.chains = n.chains
  inits.files <- file.path(getwd(), c(paste("inits_", re,"-", 1:n.chains, ".txt", sep = "")) )
  #inits.files1 <- file.path(getwd(), paste("inits1.txt") )
  #inits.files2 <- file.path(getwd(), paste("inits2.txt") )
  inits <- function() {list(a = mean(bf.sim[[2]]),
                            b = mean(bf.sim[[3]]),
                            g = mean(bf.sim[[4]]),
                            theta = rmvnorm(nE, mu, SIG))  }
  n.iter <- 10000
  n.burnin <- 3000
  n.update <- (n.iter - n.burnin)
  n.beg <- n.burnin + 1
  n.thin <- 1
  genInitsFile(n.chains, inits, initsFileStem = paste("inits_", re, "-", sep =""))      #need rbugs package
  #bugsInits(inits, numChains = 1, fileName = inits.files, digits = 5)   ##from RBugs package

  ##write BUGS script file    (edited 1/14/12)
  script.file <- file.path(getwd(), paste("script", re, ".txt", sep = "") )
  genBugsScript(parameters, n.chains = n.chains, n.iter = n.iter, n.burnin = n.burnin, n.thin = 1,     ##need rbugs package
                dic = TRUE, model.file, data.file, inits.files,
                bugsWorkingDir=getwd(),
                script.file, debug = FALSE,
                OpenBugs=TRUE, Windows=TRUE, seed=NULL)
  srpt <- scan(script.file, what = "character", sep = "\n", multi.line = FALSE)
  srpt1 <- "modelDisplay('log')"                  ## this command is required  (added DD 1/14/12)
  write(c(srpt1,srpt), file = script.file)

  list(dat = data.file, init = inits.files, script = script.file)
}

runOpenbugs <- function(script, n.chains)
{

  #Execute a BUGS Srcipt from R and run it
  bfcommirt.sim <- runBugs(bugs = "/usr/bin/OpenBUGS",
                      script = script, n.chains = n.chains,
                      workingDir = getwd(), OpenBugs = TRUE, Windows=TRUE,
                      verbose = FALSE)
}

bfgena <- function(re = 1, nE = 100, nitems = 30,
                   nD = 4, mina = .75, maxa = 1.25, currentSeeds=NULL) {
  require(msm)
  require(mvtnorm)

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
      k2 = (d-1)*niD  ## note k2 < k1 (really?)  ## I get this-->k1=((2-2)10)+1=1 < k2=(2-1)10=10 , k1=((3-2)10)+1=11 < k2=(3-1)10=20 , k1=((4-2)10)+1=21 < k2=(4-1)10=30
                                        #a[k2:k1 , d] <- runif( niD, min = 0.25, max = 0.75)
      a[k1:k2 , d] <- runif( niD, min = mina, max = maxa)   #DD 1/2/12: I think it should be from k1 to k2
                                        #st[k2:k1] <- d
      st[k1:k2] <- d
    }
  }

  b <- rtnorm(nitems, m = 0, sd = 1, lower = -3.0, upper = 3.0)   #fixed b to have N(0,1) with truncation between -2.75 and 2.75

                                        #Pra <- matrix(0, nE, nitems)
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



runOneSimulation <- function(re, nitems=NULL, nE=NULL, mina=NULL, maxa=NULL, nD=NULL, n.chains=NULL){
  currentStream <- 1
  assign("startSeeds", initSeeds(re), envir = .GlobalEnv)
  assign("currentSeeds", startSeeds, envir = .GlobalEnv)
  assign(".Random.seed",  startSeeds[[currentStream]],  envir = .GlobalEnv)
  olddir <- getwd()
  workdir <- paste("batch", nitems, mina, maxa, re, sep="-")
  dir.create(workdir, showWarnings = TRUE, recursive = TRUE)
  setwd(workdir)

  bf.sim <- bfgena(re = re, nE = nE, nitems = nitems, nD = nD, mina = mina, maxa = maxa, currentSeeds=currentSeeds)
  writeDataFiles( bf.sim, re = re, nitems = nitems, mina = mina, maxa = maxa )
  writeBUGSModel(re, nitems, nE, nD, mina, maxa)
  bugsfiles <- writeBUGSFiles(bf.sim, re, nD, n.chains, nE = nE )
  
  t1 <- try(system(paste("OpenBUGS ", bugsfiles$script, " > results.txt && gzip *.txt")))

  # runBugs( script=  bugsfiles$script )
  
  setwd(olddir)
  t1
}



library(parallel)

## set seed here in case any random draws are called
## for by user code.

RNGkind("L'Ecuyer-CMRG")
set.seed(234234)


load("projSeeds.rda")


## currentStream <- 1
## currentSeeds <- startSeeds <- projSeeds[[1]]
## .Random.seed <- startSeeds[[currentStream]]

useStream <- function(n = NULL, origin = FALSE){
  if (n > length(currentSeeds)) stop("requested stream does not exist")
  currentSeeds[[currentStream]] <<- .Random.seed
  if (origin) assign(".Random.seed", startSeeds[[n]], envir = .GlobalEnv)
  else assign(".Random.seed",  currentSeeds[[n]], envir = .GlobalEnv)
  currentStream <<- n
}


## will run on worker, so can retrieve seeds for particular run
initSeeds <- function(p = NULL){
  currentStream <<- 1
  projSeeds[[p]]
}






nE <- 1500
nitems <- 30
nD <- 4           ## Levels of discrimination in the secondary dimension
mina <- 1.25      ## .25, .50, .75, 1.00, 1.25
maxa <- 1.75      ## .75, 1.00, 1.25, 1.50, 1.75
n.chains <- 2


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

