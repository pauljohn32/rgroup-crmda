
writeDataFiles <- function(bf.sim, re, nitems, mina, maxa ){
  ##print true item parameters to .txt file
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



runOpenBUGS <- function(bf.sim, re, nD, n.chains, nE=NULL, n.iter=1000, n.burnin=300, n.thin=1){
  require(R2OpenBUGS)
   
  parameters <- c("a", "b", "g", "theta")      ## AIC and BIC are added 1/14/12
  model.file <- file.path(getwd(), "bifactor.txt")  ## DD: 1/6/12
  data <- list(mu = rep(0,nD), SIG = diag(nD), st = bf.sim[[1]], r = bf.sim[[6]])
  inits <- function() {
    list(a = mean(bf.sim[[2]]),
         b = mean(bf.sim[[3]]),
         g = mean(bf.sim[[4]]),
         theta = rmvnorm(nE, c(rep(0,nD)), diag(nD)))
  }
  
 ## model <- bugs(data, inits, parameters, model.file = "bifactor.txt", n.chains = n.chains, n.iter = n.iter, n.burnin = n.burnin, n.thin = n.thin,  codaPkg = TRUE, OpenBUGS.pgm="/usr/bin/OpenBUGSCli",  working.directory = getwd(), clearWD=FALSE, )

   model <- bugs(data, inits, parameters, model.file = "bifactor.txt", n.chains = n.chains, n.iter = n.iter, n.burnin = n.burnin, n.thin = n.thin, OpenBUGS.pgm="C:/OpenBUGS/OpenBUGS321/OpenBugs.exe",  working.directory = getwd(), clearWD=FALSE, )
}


## a function to compute errors for all paramaters, average SSI and model fit
parErrSSI <-function(bf.sim, res, re) {
 
  ##Error = true item parameters - estimated item and person parameters from OpenBUGS
  aerr <- bf.sim[[2]] - res$mean$a  ## errors of the discrimination
  berr <- bf.sim[[3]] - res$mean$b
  cerr <- bf.sim[[4]] - res$mean$g
  theterr <- bf.sim[[5]] - res$mean$theta
  ## Absolute Error
  aabserr <- abs(aerr)
  babserr <- abs(berr)
  cabserr <- abs(cerr)
  thetabserr <- abs(theterr)
  ## Error Squared
  aerr2 <- (aerr)^2
  berr2 <- (berr)^2
  cerr2 <- (cerr)^2
  theterr2 <-(theterr)^2
  
  ##deviance, pD, DIC and AIC
  dev <- res$mean$deviance  ## Dbar
  ## res from bugs() gives reverse value, so need to revert them back
  pD <- res$DIC    ##effective number of parameters 
  DIC <- res$pD    ##Deviance information criteria
  AIC <- dev + (2*pD) #approximation of AIC
  
  ## SSI
  ### Subscore separation index is computed from a ratio of the differences between
  ### a pair (primary and secondary) of the estimated subscores to the sum of the standard errors of the two subscores.
  ### The percentage of this ratio over examinees that is greater than 1.0 for all pairs of
  ### subscores is the measure of SSI.
  ## Absolute EAP different for each examinee
  abstheta12 <- abs(res$mean$theta[,1] - res$mean$theta[,2])
  abstheta13 <- abs(res$mean$theta[,1] - res$mean$theta[,3])
  abstheta14 <- abs(res$mean$theta[,1] - res$mean$theta[,4])
  ## total std errors between pairs
  se12 <- res$sd$theta[,1] + res$sd$theta[,2]
  se13 <- res$sd$theta[,1] + res$sd$theta[,3]
  se14 <- res$sd$theta[,1] + res$sd$theta[,4]
  
  ## Absolute EAP different/std errors
  essi12 <- abstheta12/se12
  essi13 <- abstheta13/se13
  essi14 <- abstheta14/se14
  
  ## Subscore Separation Index (SSI) - percent of essi > 1 
  tab12 <- table(essi12 > 1)
  SSI12 <- unlist(tab12[[2]]/nE)
  tab13 <- table(essi13 > 1)
  SSI13 <- unlist(tab13[[2]]/nE)
  tab14 <- table(essi14 > 1)
  SSI14 <- unlist(tab14[[2]]/nE)

  parserrors <- cbind(aerr, berr, cerr, aabserr, babserr, cabserr,
                      aerr2, berr2, cerr2)

  thetaerrors <- cbind(theterr, theterr2, abstheta12, abstheta13, abstheta14)

  fit <- cbind(SSI12, SSI13, SSI14, dev, pD, DIC, AIC)

  results <- list("parserrors"=parserrors, "thetaerrors"=thetaerrors, "fit"=fit)
}

## Take a list of 50 result objects, extract and combine
## matrices perrors, terrors, fit, return summary numbers
summarizeResultList <- function(aList){
  ## list placeholders 
  parserrorList <- vector("list", length=length(aList))
  terrorList <- vector("list", length=length(aList))
  fitList <- vector("list", length=length(aList))

  for( i in seq_along(aList)){
    parserrorList[[i]] <- aList[[i]]$parserrors
    terrorList[[i]] <- aList[[i]]$thetaerrors
    fitList[[i]] <-  aList[[i]]$fit
  }

  perrors <- do.call("rbind", parerrList)
  terrors <- do.call("rbind", terrorList)
  fit <-  do.call("rbind", fitList)
  ## do.call significantly more efficient than repeated use of rbind. For explanation, see
  ## http://pj.freefaculty.org/R/WorkingExamples/stackListItems.R
  
  rm(parserrorList, terrorList, fitList)
  
  ## BIAS
  ## a1bias <- mean(perrors[[1]])
  ## a2bias <- mean(perrors[[2]])
  ## a3bias <- mean(perrors[[3]])
  ## a4bias <- mean(perrors[[4]])
  ## bbias <- mean(perrors[[5]])
  ## cbias <- mean(perrors[[6]])
  
  pbias <- apply( perrors[ , 1:6], 2, mean)
  names(pbias) <- c("a1bias","a2bias","a3bias","a4bias","bbias","cbias")
  
  
  ## t1bias <- mean(terrors[[1]])
  ## t2bias <- mean(terrors[[2]])
  ## t3bias <- mean(terrors[[3]])
  ## t4bias <- mean(terrors[[4]])
  
  tbias <- apply( terrors[, 1:4], 2, mean)
  names(tbias) <- paste("t", 1:4, "bias", sep="")
  
  ## RMSE
  ## a1rmse <- sqrt(mean(perrors[[13]]))
  ## a2rmse <- sqrt(mean(perrors[[14]]))
  ## a3rmse <- sqrt(mean(perrors[[15]]))
  ## a4rmse <- sqrt(mean(perrors[[16]]))
  ## brmse <- sqrt(mean(perrors[[17]]))
  ## crmse <- sqrt(mean(perrors[[18]]))
  
  prmse <- apply( perrors[ , 13:18], 2, function(x) {sqrt(mean(x))})
  names(prmse) <- c("a1rmse", "a2rmse", "a3rmse", "a4rmse", "brmse", "crmse")
  
  
  ## t1rmse <- sqrt(mean(terrors[[5]]))
  ## t2rmse <- sqrt(mean(terrors[[6]]))
  ## t3rmse <- sqrt(mean(terrors[[7]]))
  ## t4rmse <- sqrt(mean(terrors[[8]]))
  
  
  trmse <- apply( terrors[ , 5:8], 2, function(x) {sqrt(mean(x))})
  names(trmse) <-  paste("t", 1:4, "rmse", sep="")
  
  ##SEE (Standard Error of Estimates)
  ## seea1 <- sqrt(a1rmse^2 - a1bias^2)
  ## seea2 <- sqrt(a2rmse^2 - a2bias^2)
  ## seea3 <- sqrt(a3rmse^2 - a3bias^2)
  ## seea4 <- sqrt(a4rmse^2 - a4bias^2)
  ## seeb <- sqrt(brmse^2 - bbias^2)
  ## seec <- sqrt(crmse^2 - cbias^2)
  ## seet1 <- sqrt(t1rmse^2 - t1bias^2)
  ## seet2 <- sqrt(t2rmse^2 - t2bias^2)
  ## seet3 <- sqrt(t3rmse^2 - t3bias^2)
  ## seet4 <- sqrt(t4rmse^2 - t4bias^2)
  
  
  psee <- sqrt(prmse^2 - pbias^2)
  tsee <- sqrt(trmse^2 - tbias^2)
  
  ## SSI (mean SSI over replications)
  ## SSI12 <- mean(ssi[,4])
  ## SSI13 <- mean(ssi[,5])
  ## SSI14 <- mean(ssi[,6])
  
  bffit <- apply( fit[, 1:7], 2, mean)
  names(bffit) <- c("SSI12", "SSI13", "SSI14", "Dbar", "pD", "DIC", "AIC")

  list(pbias, tbias, prmse, trmse,  psee, tsee, bffit)
}


bfgena <- function(re = 1, nE = 100, nitems = 30, nD = 4, mina = .75,
                   maxa = 1.25, currentSeeds=NULL) {
  require(msm)
  require(mvtnorm)

  ## nested function to perform logit transform with guessing.
  Pra <- function(thet = matrix(0), a = matrix(0), b = 0, g = 0) {         ## will be used for item responses in bfgena.sim
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
      k2 = (d-1)*niD  ## note k2 < k1 (really?)  ## I get this-->k1=((2-2)10)+1=1 < k2=(2-1)10=10 , k1=((3-2)10)+1=11 < k2=(3-1)10=20 , k1=((4-2)10)+1=21 < k2=(4-1)10=30
                                        #a[k2:k1 , d] <- runif( niD, min = 0.25, max = 0.75)
      a[k1:k2 , d] <- runif( niD, min = mina, max = maxa)   #DD 1/2/12: I think it should be from k1 to k2
                                        #st[k2:k1] <- d
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
  res <- runOpenBUGS(bf.sim, re, nD, n.chains, nE = nE, n.iter = n.iter, n.burnin = n.burnin , n.thin = n.thin)
  res2 <- parErrSSI(bf.sim, res, re)
  setwd(olddir)
  combinedResults <- c(res, res2)
}



library(parallel)

## set seed here in case any random draws are called
## for by user code.

RNGkind("L'Ecuyer-CMRG")
set.seed(234234)


load("projSeeds.rda")





### ALL PARAMETERS that will ever need to be change should be listed here.
### Never leave numbers that need to be changed in functions above.
### 2012-02-06

nE <- 150
nitems <- 30
nD <- 4           ## Levels of discrimination in the secondary dimension
mina <- 1.25      ## .25, .50, .75, 1.00, 1.25
maxa <- 1.75      ## .75, 1.00, 1.25, 1.50, 1.75
n.chains <- 2
n.iter <- 700
n.burnin <- 300
n.thin <- 1

## To test this out, run this. Does not require cluster framework.

res <- runOneSimulation(re = 1, nitems=nitems, nE = nE,  mina = mina, maxa = maxa, nD = nD, n.chains = n.chains, n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin)



##############################
###Here's the parallel part.
nReps <- 50

cl <- makeCluster(19, "MPI")

clusterEvalQ(cl, {
  RNGkind("L'Ecuyer-CMRG")
} )

clusterExport(cl, c("projSeeds", "useStream", "initSeeds"))

clusterExport(cl, c("writeDataFiles", "writeBUGSModel", 
"writeBUGSFiles", "bfgena"))

resultList <- snow:::clusterApplyLB(cl, 1:nReps, runOneSimulation, nitems=nitems, nE = nE,  mina = mina, maxa = maxa, nD = nD, n.chains = n.chains, n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin)

sumry <- summarizeResultList(resultList)

##keep copies of result object collection and summary in current working directory
save(unlist(sumry), file="resultSummary.rda")
save(resultList, file="resultList.rda")


library(snow)
stopCluster(cl)
mpi.quit()



