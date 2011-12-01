
#rm(list=ls(all=T))

goBabyGo <- function(runNumber,parms) {
  
  simData <- function(parms){
    require(mvtnorm)
    
    nobs <- parms$nobs
    mfK <- parms$mfK
    
    phi <- matrix(c(1,.5,.5,1),2,2)

    lambda <- matrix(c(rep(.6,5),rep(0,5),
                       rep(0,5),rep(.6,5),
                       rep(.1,5),rep(.2,5),
                       rep(.3,5),rep(.15,5),
                       rep(.2,5),rep(.1,5),
                       rep(.05,5),rep(.1,5)),10,6)
    
    theta <- toeplitz(c(.3,0,0,mfK,0,0,mfK,0,0,mfK))
    eta <- rmvnorm(nobs,c(0,0),phi)
    male <- rbinom(nobs,1,.5)
    white <-  rbinom(nobs,1,.75)
    Covs <- rmvnorm(nobs,c(2,4),matrix(c(1,0,0,3),2,2))
    preds <- cbind(eta,male,white,Covs)
    colnames(preds) <- c("F1","F2","male","white","c1","c2")
    errors <- rmvnorm(nobs,rep(0,10),theta)
    dat <- preds %*% t(lambda) + errors
    newDat <- cbind(dat,preds[,3:6])
   
    colnames(newDat) <- c(paste("a", 1:5, sep=""),paste("b", 1:5, sep=""),"male","white","c1","c2")
    newDat
  }



makeMAR <- function(pm,dat,parms)
  {
    lenScale <- parms$lenScale
    marPred1 <- parms$marPred1
    marPred2 <- parms$marPred2

     Y <- runif(lenScale*.5,0,.25*pm)

     Z <- sample(c(Y+pm,-Y+pm), size=lenScale, replace=F)

     fun1 <- function(x,dat) pnorm(dat,mean(dat),sd(dat)) <= x

     R1 <- sapply(Z[1:(length(Z)*.5)],fun1,dat=dat[,marPred1])
     R2 <- sapply(Z[((length(Z)*.5)+1):length(Z)],fun1,dat=dat[,marPred2])

     R <- cbind(R1,R2,matrix(FALSE,dim(dat)[1],(dim(dat)[2]-(dim(R1)[2]*2))))
    
     dat[R] <- NA
    dat

  }# End makeMAR()



imputeStack <- function(dat,parms)
    {
      require(Amelia)
      
      imps <- parms$imps
      lenScale <- parms$lenScale
      
      ameliaOut <- amelia(dat, m=imps, empri=.1*dim(dat)[1], p2s=0)

      stackedDat <- do.call("rbind", ameliaOut[[1]])

      rawCov <- lapply(ameliaOut[[1]], cov)
      
      superMat <- cov(stackedDat[ , 1:lenScale])

      rm(stackedDat)
      rm(ameliaOut)
      list(rawCov=rawCov, superMat=superMat)
      
    } # end imputeStack() 



fitMissinModels <- function(dat, parms)
    {
      require(lavaan)

      mod1 <- parms$mod1
      smCov <- dat$superMat
      rawCovs <- dat$rawCov
      nobs <- parms$nobs
      imps <- parms$imps
      
      smFullMod <- cfa(mod1, sample.cov=smCov, sample.nobs=nobs, std.lv=T)	

      smResMod <- cfa(mod1, sample.cov=smCov, sample.nobs=nobs, std.lv=T, orthogonal=T)

      fitRawMods <- function(x, parms)
        {
          mod1 <- parms$mod1
          nobs <- parms$nobs
          
          fullMod <- cfa(mod1, sample.cov=x, sample.nobs=nobs, std.lv=T)

          resMod <- cfa(mod1, sample.cov=x, sample.nobs=nobs, std.lv=T, orthogonal=T)

          list(rawFullOut=list(rawFullFit=fitMeasures(fullMod), rawFullCoef=coef(fullMod), rawFullSE=inspect(fullMod, "se"), rawFullDx=inspect(fullMod, "dx"), rawFullLL=logLik(fullMod)),

               rawResOut=list(rawResFit=fitMeasures(resMod), rawResCoef=coef(resMod), rawResSE=inspect(resMod, "se"), rawResDx=inspect(resMod, "dx"), rawResLL=logLik(resMod)))
          
          }

      rawMissOut <- lapply(rawCovs,FUN=fitRawMods,parms=parms)

      list(smMissOut=list(smFullOut=list(smFullFit=fitMeasures(smFullMod), smFullCoef=coef(smFullMod), smFullSE=inspect(smFullMod, "se"), smMissDx=inspect(smFullMod, "dx"), smFullLL=logLik(smFullMod)),

             smResOut=list(smResFit=fitMeasures(smResMod), smResCoef=coef(smResMod), smResSE=inspect(smResMod, "se"), smResDx=inspect(smResMod, "dx"), smResLL=logLik(smResMod))),

           rawMissOut=rawMissOut)
      

    }# End fitMissinModels() 


fitControlModel <- function(dat4,parms4)
    {
      require(lavaan)

      nobs <- parms4$nobs
      lenScale <- parms4$lenScale
      mod1 <- parms4$mod1

      conCov <- cov(dat4[,1:lenScale])
      
      fullMod <- cfa(mod1, sample.cov=conCov, sample.nobs=nobs,std.lv=T)	

      resMod <- cfa(mod1, sample.cov=conCov, sample.nobs=nobs,std.lv=T,orthogonal=T)

      list(conFullOut=list(conFullFit=fitMeasures(fullMod), conFullCoef=coef(fullMod), conFullSE=inspect(fullMod, "se"), conMissDx=inspect(fullMod, "dx"), conFullLL=logLik(fullMod)),

             conResOut=list(conResFit=fitMeasures(resMod), conResCoef=coef(resMod), conResSE=inspect(resMod, "se"), conResDx=inspect(resMod, "dx"), conResLL=logLik(resMod)))
      
    }# End fitControlModel()



  ##
  ## Combine fundamental functions above into a larger aggregation to run the stuff for 1 cell:
  ##



runMissSim <- function(dat3, pm, parms3)
    {
      lenScale <- parms3$lenScale
      nobs <- parms3$nobs

      impDat <- imputeStack(dat=dat3, parms3)

      yourMissinOut <- fitMissinModels(dat=impDat, parms3)

      pctmissing3 <- sum(is.na(dat3))/(lenScale*nobs)

      rm(impDat)
      print("in runMissSim")
      gc(TRUE)

      list(cellPM=list(hypotheticalPM=pm, empiricalPM=pctmissing3), cellMissOut=yourMissinOut)
      
    }# end runMissSim()

  
  ##
  ##
  ## Aggregate the two above functions to run a single row of the results matrix:
  ##
  ##



rowTask <-function(x, dat, runNumber, parms)
    {
      nobs <- parms$nobs
      lenScale <- parms$lenScale

      NewNobs <- (nobs-x)
      
      cullDat <- dat[1:NewNobs,]

      parms$nobs <- NewNobs
      
      conOut <- fitControlModel(cullDat,parms)

      ##   incomp.list <- impose.missing(dat=cullDat,parms)

      ##    missOut <- lapply(incomp.list,FUN=runMissSim,parms)

      ##PJ: Redesign, aim to keep only one dataset in memory
      ##and one set of amelia runs in memory at one time.
      handleOneDat <- function(pm, dat, parms2){
        newdat <-  makeMAR(pm,dat,parms2)
        runMissSim(newdat,pm,parms2)
      }
      
      PMvec <- seq(parms$minPM, parms$maxPM, parms$PMstep) 

      missOut <- lapply(PMvec, FUN=handleOneDat, dat=cullDat, parms2=parms)
      
     ##  list(row.ControlOut=conOut, row.missing.out=missOut)
     
      save(conOut, file=paste("conOut-run-",runNumber,"-omit-",x,"-", 10000*runif(1),".RData", sep=""))
      save(missOut, file=paste("missOut-run-",runNumber,"-omit-",x,"-",10000*runif(1),".RData", sep=""))
      rm(conOut)
      rm(missOut)
      gc(TRUE)
      mynothing <- list() ## Return nothing
    }# end rowTask()




  ## Runs rowTask() over all samplesizes to get an entire replication:

repTask <- function(x, parms2)
    {
      samInc <- parms2$samInc
      
      freshDat <- simData(parms2)

      ## lapply(samInc, FUN=rowTask, dat=freshDat, parms=parms2)
      for (i in samInc) {
        rowTask(i, dat=freshDat, runNumber=x, parms=parms2)
      }
    }# end repTask()

  repTask(runNumber, parms) #execute repTask() as the primary task of go.baby.go.fun()

  mynothing <- list()
### return nothing
}# end goBabyGo()





##################################### Begin Parallel Runs (snowFT) ###################################

require(snowFT)

mySeeds <- rep(235711,6)

rp <- 1 #c(1:10)

cnt <- 3

parameters <- list()
parameters$samInc <- 10 #seq(0,400,10)
parameters$marPred1 <- "c1"
parameters$marPred2 <- "c2"
parameters$lenScale <- 10
parameters$minPM <- .02
parameters$maxPM <- .06
parameters$PMstep <- .02
parameters$imps <- 10
parameters$nobs <- 500
parameters$mfK <- .05
parameters$mod1 <- "ConA =~ NA*a1 + a2 + a3 + a4 + a5
                    ConB =~ NA*b1 + b2 + b3 + b4 + b5"



## Let's run the bugger!!! ##

runTime <- system.time(

performParallel(count=cnt, x=rp, fun=goBabyGo, seed=mySeeds, cltype="MPI", parms=parameters)


            )

save(runTime,file="repTime.RData")

