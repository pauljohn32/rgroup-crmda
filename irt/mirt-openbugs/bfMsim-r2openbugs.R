
### bifactor: partially compensatory MIRT model for R2OpenBUGS

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
    for (i in 1: nE ) {
		 theta[i, 1: nD ] ~ dmnorm(mu[],SIG[,])
	  }
	  for (j in 1: nitems ) {
		  structure[j,1] <- 1
	  	g[j]~dbeta(5,17)
		  for (k in 2: nD ) {
			  structure[j,k]<-equals(k,st[j])
		  }
		  for (k in 1: nD ) { 
			  temp[j,k] ~ dnorm(0,pr.a) I(0,)
			  a[j,k] <- (structure[j,k]+.0001)*temp[j,k]
		  	b[j,k]~dnorm(0, pr.b)
		  }
	 }
   pr.a <- pow(sig.a, -2)
   pr.b <- pow(2.00, -2)
   for (i in 1: nE ) {
		for (j in 1: nitems ) {
			for (k in 1: nD ) {
				pdim[i,j,k] <- phi(a[j,k]*(theta[i,k]-b[j,k]))
			}
			p1[i,j]<-inprod(pdim[i,j,1],pdim[i,j,2])*inprod(pdim[i,j,3],pdim[i,j,4])
			p[i,j] <- g[j] + (1-g[j])*p1[i,j]
			r[i,j]~dbern(p[i,j])	
		}	
	 }
  }


'
  
  ## replace texts with values
  bug.model <- gsub( "nE", nE, bug.model)
  bug.model <- gsub( "nD", nD, bug.model)
  bug.model <- gsub( "nitems", nitems, bug.model)
  sigma <- 2.00
  bug.model <- gsub( "sig.a", sigma, bug.model)
  meana <- 1.00
  bug.model <- gsub( "m.a", meana, bug.model)
  bug.model <- gsub( "Trnct", "T", bug.model)
  
  cat(bug.model, file = "bifactor.txt")
  
}


runOpenBUGS <- function(bf.sim, re, nitems, nD, n.chains, nE=NULL, n.iter=1000, n.burnin=300, n.thin=1){
  require(R2OpenBUGS)
   
  parameters <- c("a", "b", "g", "theta")      
  model.file <- file.path(getwd(), "bifactor.txt")
  data <- list(mu = rep(0,nD), SIG = diag(nD), st = bf.sim[[1]], r = bf.sim[[6]])
  inits <- function() {
    list(a = rep(mean(bf.sim[[2]]),nitems),
         b = rep(mean(bf.sim[[3]]),nitems),
         g = rep(mean(bf.sim[[4]]),nitems),
         theta = rmvnorm(nE, c(rep(0,nD)), diag(nD)))
  }
  
 ## model <- bugs(data, inits, parameters, model.file = "bifactor.txt", n.chains = n.chains, n.iter = n.iter, n.burnin = n.burnin, n.thin = n.thin,  codaPkg = TRUE, OpenBUGS.pgm="/usr/bin/OpenBUGSCli",  working.directory = getwd(), clearWD=TRUE )

   model <- bugs(data, inits, parameters, model.file = "bifactor.txt", n.chains = n.chains, n.iter = n.iter, n.burnin = n.burnin, n.thin = n.thin, OpenBUGS.pgm="/usr/bin/OpenBUGSCli",  working.directory = getwd(), clearWD = TRUE)
  
  
}


## a function to compute errors for all paramaters, average SSI and model fit
parErrSSI <-function(bf.sim, res, re, nE) {
 
  ##Error = true - estimated
  aerr <- bf.sim[[2]] - res$mean$a
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
  
  ##deviance, pD, DIC, AIC and BIC
  dev <- res$mean$deviance  ## Dbar
  pD <- res$pD    ##effective number of parameters
  LgL <- dev - pD  # -2logL
  DIC <- res$DIC    ##Deviance information criteria
  DIC2 <- res$mean$deviance + res$pD
  AIC <- dev + (2*pD) #approximation of AIC
  BIC <- dev + pD*log(nE)
  pDlin1 <- res$sd$deviance
  pDlin <- (pDlin1^2)/2
  DIClin <- res$mean$deviance + pDlin
  AIClin <- res$mean$deviance + (2*pDlin)
  BIClin <- dev + pDlin*log(nE)
  
  
  ## SSI
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
  #tab12 <- table(essi12 > 1)
  SSI12 <- sum(essi12 > 1)/nE
  #tab13 <- table(essi13 > 1)
  SSI13 <- sum(essi13 > 1)/nE
  #tab14 <- table(essi14 > 1)
  SSI14 <- sum(essi14 > 1)/nE
  
  parserrors <- cbind(aerr, berr, cerr, aabserr, babserr, cabserr,
                      aerr2, berr2, cerr2)
  
  thetaerrors <- cbind(theterr, theterr2, abstheta12, abstheta13, abstheta14)
  
  fit <- cbind(SSI12, SSI13, SSI14, dev, pD, LgL, DIC, DIC2, AIC, BIC, pDlin, DIClin, AIClin, BIClin )
  
  results <- list("parserrors"=parserrors, "thetaerrors"=thetaerrors, "fit"=fit)
  
}

## Take a list of 50 result object names, extract and combine
## matrices perrors, terrors, fit, return summary numbers
summarizeResultList <- function(aList){
  ## list placeholders
  parserrorList <- vector("list", length=length(aList))
  terrorList <- vector("list", length=length(aList))
  fitList <- vector("list", length=length(aList))
  
  for( i in seq_along(aList)){
    load( aList[[i]] ) ##causes combinedResults into memory
    parserrorList[[i]] <- combinedResults$parserrors
    terrorList[[i]] <- combinedResults$thetaerrors
    fitList[[i]] <-  combinedResults$fit
    rm(combinedResults)
  }
  
  perrors <- do.call("rbind", parserrorList)
  terrors <- do.call("rbind", terrorList)
  fit <-  do.call("rbind", fitList)
  ## do.call significantly more efficient than repeated use of rbind. For explanation, see
  ## http://pj.freefaculty.org/R/WorkingExamples/stackListItems.R
  
  rm(parserrorList, terrorList, fitList)
  
  ## BIAS: perrors[[1]] to perrors[[9]]
  pbias <- apply( perrors[ , 1:9], 2, mean)
  names(pbias) <- c("a1bias","a2bias","a3bias","a4bias","b1bias","b2bias","b3bias","b4bias","cbias")
  ##       terrors[[1]] to terrors[[4]]
  tbias <- apply( terrors[, 1:4], 2, mean)
  names(tbias) <- paste("t", 1:4, "bias", sep="")
  
  ## RMSE: perrors[[19]] to perrors[[27]]: sqrt of the mean of errors squared
  prmse <- apply( perrors[ , 19:27], 2, function(x) {sqrt(mean(x))})
  names(prmse) <- c("a1rmse", "a2rmse", "a3rmse", "a4rmse", "b1rmse", "b2rmse", "b3rmse", "b4rmse", "crmse")
  ##      terrors[[5]] to terrors[[8]]
  trmse <- apply( terrors[ , 5:8], 2, function(x) {sqrt(mean(x))})
  names(trmse) <-  paste("t", 1:4, "rmse", sep="")
  
  ##SEE (Standard Error of Estimates) = rmse^2 - bias^2
  psee <- sqrt(prmse^2 - pbias^2)
  names(psee) <- c("a1see", "a2see", "a3see", "a4see", "b1see","b2see","b3see","b4see", "csee")
  tsee <- sqrt(trmse^2 - tbias^2)
  names(tsee) <- paste("t", 1:4, "see", sep="")
  
  ## SSI and model-fit (mean over replications)
  bffit <- apply( fit[, 1:14], 2, mean)
  names(bffit) <- c("SSI12", "SSI13", "SSI14", "Dbar", "pD", "-2logL", "DIC", "DIC2", "AIC", "BIC", "pDlin", "DIClin", "AIClin", "BIClin" )
  
  list(pbias, tbias, prmse, trmse,  psee, tsee, bffit)
  
}


bfgena <- function(re = 1, nE = 100, nitems = 30, nD = 4, mina = 0.00,
                   maxa = 1.75, currentSeeds=NULL) {
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
  b <- matrix(0, nitems, nD) ## b is now associates with each dimension
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
      a[k1: k2 , d] <- runif( niD, min = mina, max = maxa)
      st[k1: k2] <- d
    }
    b[ , d] <- rtnorm(nitems, m = 0, sd = 1, lower = -2.50, upper = 2.50)
  }
  
  useStream(3, origin=TRUE)
  pm<-matrix(0,nD)
  pdimm<-matrix(0,nE,nitems)
  Prm<-matrix(0,nE,nitems)
  Xm<-matrix(0,nE,nitems)
  for (p in 1:nE) {
    for (i in 1:nitems) {
      #multiplicative model - noncompensatory
      for (d in 1:nD) {
        pm[d] <- 1/(1+exp(-a[i,d]*(theta1[p,d]-b[i,d])))
      }
      pdimm[p,i]=prod(pm[1:nD])  #multiplicative probit
      Prm[p,i]<-g[i]+(1-g[i])*pdimm[p,i]    #MIRT 3PL from multiplicative
      Xm[p, i] <- ifelse ( runif(1, 0, 1) < g[i]+(1-g[i])*Prm[p,i], 1, 0)
      
    } #end loop over items
  }  #end loop over examinees
  
  list(st, a, b, g, theta1, Xm)

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
  workdir <- paste("batch100", nitems, mina, maxa, re, sep="-")
  dir.create(workdir, showWarnings = TRUE, recursive = TRUE)
  setwd(workdir)
  
  bf.sim <- bfgena(re = re, nE = nE, nitems = nitems, nD = nD, mina = mina, maxa = maxa, currentSeeds=currentSeeds)
  writeBUGSModel(re, nitems, nE, nD, mina, maxa )
  writeDataFiles(bf.sim, re, nitems, mina, maxa )
  res <- runOpenBUGS(bf.sim, re, nitems, nD, n.chains, nE = nE, n.iter = n.iter, n.burnin = n.burnin , n.thin = n.thin)
  res2 <- parErrSSI(bf.sim, res, re, nE)
  setwd(olddir)
  rhats <- res$summary[ ,8]
  rhfn <- paste("rhat", sprintf("%003d", re),".rda", sep="")
  save(rhats, file = rhfn)
  combinedResults <- c(res, res2)
  newfn <- paste("res", sprintf("%003d", re),".rda", sep="")
  save(combinedResults, file = newfn)
  newfn
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

nE <- 1500
nitems <- 30
nD <- 4           ## Levels of discrimination in the secondary dimension
mina <- 1.25      ## .25, .50, .75, 1.00, 1.25
maxa <- 1.75      ## .75, 1.00, 1.25, 1.50, 1.75
n.chains <- 2
n.iter <- 6500
n.burnin <- 4500
n.thin <- 3



## To test this out, run this. Does not require cluster framework.

#res <- runOneSimulation(re = 1, nitems=nitems, nE = nE,  mina = mina, maxa = maxa, nD = nD, n.chains = n.chains, n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin)

#save(res, file="res1.rda")



## To test this out, run this. Does not require cluster framework.
if (0){
  resultList <- lapply(1:2, runOneSimulation, nitems=nitems, nE = nE,  mina = mina, maxa = maxa, nD = nD, n.chains = n.chains, n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin)
  
  sumry <- summarizeResultList(resultList)
  
}else{
  
  
  ##############################
  ###Here's the parallel part.
  nReps <- 50 
  
  cl <- makeCluster(10, "MPI")
  
  clusterEvalQ(cl, {
    RNGkind("L'Ecuyer-CMRG")
  } )
  
  clusterExport(cl, c("projSeeds", "useStream", "initSeedStreams"))
  
  clusterExport(cl, c("writeDataFiles", "writeBUGSModel", 
                      "runOpenBUGS", "bfgena", "parErrSSI"))
  
  resultList <- snow:::clusterApplyLB(cl, 1:nReps, runOneSimulation, nitems=nitems, nE = nE,  mina = mina, maxa = maxa, nD = nD, n.chains = n.chains, n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin)
  
  
  
  sumry <- summarizeResultList(resultList)
  
  ##keep copies of result object collection and summary in current working directory
  save(sumry, file="resultSummary.rda")
  save(resultList, file="resultList.rda")
  
  
  library(snow)
  stopCluster(cl)
  mpi.quit()
}

