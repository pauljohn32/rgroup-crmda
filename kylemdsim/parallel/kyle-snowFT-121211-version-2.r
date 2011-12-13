
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
    Covs <- rmvnorm(nobs,c(0,4),matrix(c(1,0,0,3),2,2))
    preds <- cbind(eta,male,white,Covs)
    colnames(preds) <- c("F1","F2","male","white","c1","c2")
    errors <- rmvnorm(nobs,rep(0,10),theta)
    dat <- preds %*% t(lambda) + errors
    newDat <- cbind(dat,preds[,3:6])
   
    colnames(newDat) <- c(paste("a", 1:5, sep=""),paste("b", 1:5, sep=""),"male","white","c1","c2")
    newDat
  }

#newDat <- simData(parameters)
#missDat <- makeMAR(.2,newDat,parameters)
#sum(is.na(missDat))/(parms$nobs*parms$lenScale)
  
makeMAR <- function(pm,dat,parms)
  {
    lenScale <- parms$lenScale
    marPred1 <- parms$marPred1

        Y <- runif(lenScale*.5,0,.25*pm)

        Z <- sample(c(Y+pm,-Y+pm),replace=F)

        fun1 <- function(x,dat) pnorm(dat,mean(dat),sd(dat)) <= x
        fun2 <- function(x,dat) pchisq(dat,df=1) <= x

        R1 <- sapply(Z[1:(length(Z)*.5)],fun1,dat=dat[,marPred1])
        R2 <- sapply(Z[((length(Z)*.5)+1):length(Z)],fun2,dat=dat[,marPred1]^2)

        R <- cbind(cbind(R1,R2)[,sample(dim(cbind(R1,R2))[2],replace=F)],
                   matrix(FALSE,dim(dat)[1],(dim(dat)[2]-dim(cbind(R1,R2))[2])))
    
        dat[R] <- NA
        
    dat

  }# End makeMAR()

#imputedDat <- imputeStack(missDat,parameters)
  
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


#missOut <- fitMissinModels(imputedDat,parameters)

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
  
          rawFullFit <- matrix(fitMeasures(fullMod),1,20)
          rawFullCoef <- matrix(coef(fullMod),1,21)
          rawFullLambdaSE <- matrix(inspect(fullMod,"se")[[1]],10,2)
          rawFullThetaSE <- matrix(diag(inspect(fullMod,"se")[[2]]),1,10)
          rawFullPsiSE <- inspect(fullMod,"se")[[3]][1,2]
          rawFullLambdaDX <- matrix(inspect(fullMod,"dx")[[1]],10,2)
          rawFullThetaDX <- matrix(inspect(fullMod,"dx")[[2]],10,10)
          rawFullPsiDX <- matrix(inspect(fullMod,"dx")[[3]],2,2)
            
          rawResFit <- matrix(fitMeasures(resMod),1,20)
          rawResCoef <- matrix(coef(resMod),1,20)
          rawResLambdaSE <- matrix(inspect(resMod,"se")[[1]],10,2)
          rawResThetaSE <- matrix(diag(inspect(resMod,"se")[[2]]),1,10)
          rawResLambdaDX <- matrix(inspect(resMod,"dx")[[1]],10,2)
          rawResThetaDX <- matrix(inspect(resMod,"dx")[[2]],10,10)
          rawResPsiDX <- matrix(inspect(resMod,"dx")[[3]],2,2)

          list(rawFullOut=list(rawFullFit=rawFullFit, rawFullCoef=rawFullCoef, rawFullSE=list(rawFullLambdaSE,rawFullThetaSE,rawFullPsiSE), rawFullDX=list(rawFullLambdaDX,rawFullThetaDX,rawFullPsiDX)),
               
      rawResOut=list(rawResFit=rawResFit, rawResCoef=rawResCoef, rawResSE=list(rawResLambdaSE,rawResThetaSE,NA), rawResDX=list(rawResLambdaDX,rawResThetaDX,rawResPsiDX)))

        }

      rawMissOut <- lapply(rawCovs,FUN=fitRawMods,parms=parms)

      smFullFit <- matrix(fitMeasures(smFullMod),1,20)
      smFullCoef <- matrix(coef(smFullMod),1,21)
      smFullLambdaSE <- matrix(inspect(smFullMod,"se")[[1]],10,2)
      smFullThetaSE <- matrix(diag(inspect(smFullMod,"se")[[2]]),1,10)
      smFullPsiSE <- inspect(smFullMod,"se")[[3]][1,2]
      smFullLambdaDX <- matrix(inspect(smFullMod,"dx")[[1]],10,2)
      smFullThetaDX <- matrix(inspect(smFullMod,"dx")[[2]],10,10)
      smFullPsiDX <- matrix(inspect(smFullMod,"dx")[[3]],2,2)
            
      smResFit <- matrix(fitMeasures(smResMod),1,20)
      smResCoef <- matrix(coef(smResMod),1,20)
      smResLambdaSE <- matrix(inspect(smResMod,"se")[[1]],10,2)
      smResThetaSE <- matrix(diag(inspect(smResMod,"se")[[2]]),1,10)
      smResLambdaDX <- matrix(inspect(smResMod,"dx")[[1]],10,2)
      smResThetaDX <- matrix(inspect(smResMod,"dx")[[2]],10,10)
      smResPsiDX <- matrix(inspect(smResMod,"dx")[[3]],2,2)

      list(smMissOut=list(smFullOut=list(smFullFit=smFullFit, smFullCoef=smFullCoef, smFullSE=list(smFullLambdaSE,smFullThetaSE,smFullPsiSE), smFullDX=list(smFullLambdaDX,smFullThetaDX,smFullPsiDX)),
               
      smResOut=list(smResFit=smResFit, smResCoef=smResCoef, smResSE=list(smResLambdaSE,smResThetaSE,NA), smResDX=list(smResLambdaDX,smResThetaDX,smResPsiDX))),

      rawMissOut=rawMissOut)

    }# End fitMissinModels() 


#conOut <- fitControlModel(newDat,parameters)
  
fitControlModel <- function(dat4,parms4)
    {
      require(lavaan)

      nobs <- parms4$nobs
      lenScale <- parms4$lenScale
      mod1 <- parms4$mod1

      conCov <- cov(dat4[,1:lenScale])
      
      fullMod <- cfa(mod1, sample.cov=conCov, sample.nobs=nobs,std.lv=T)	

      resMod <- cfa(mod1, sample.cov=conCov, sample.nobs=nobs,std.lv=T,orthogonal=T)

      conFullFit <- matrix(fitMeasures(fullMod),1,20)
      conFullCoef <- matrix(coef(fullMod),1,21)
      conFullLambdaSE <- matrix(inspect(fullMod,"se")[[1]],10,2)
      conFullThetaSE <- matrix(diag(inspect(fullMod,"se")[[2]]),1,10)
      conFullPsiSE <- inspect(fullMod,"se")[[3]][1,2]
      conFullLambdaDX <- matrix(inspect(fullMod,"dx")[[1]],10,2)
      conFullThetaDX <- matrix(inspect(fullMod,"dx")[[2]],10,10)
      conFullPsiDX <- matrix(inspect(fullMod,"dx")[[3]],2,2)
            
      conResFit <- matrix(fitMeasures(resMod),1,20)
      conResCoef <- matrix(coef(resMod),1,20)
      conResLambdaSE <- matrix(inspect(resMod,"se")[[1]],10,2)
      conResThetaSE <- matrix(diag(inspect(resMod,"se")[[2]]),1,10)
      conResLambdaDX <- matrix(inspect(resMod,"dx")[[1]],10,2)
      conResThetaDX <- matrix(inspect(resMod,"dx")[[2]],10,10)
      conResPsiDX <- matrix(inspect(resMod,"dx")[[3]],2,2)


      controlOut=list(conFullOut=list(conFullFit=conFullFit, conFullCoef=conFullCoef, conFullSE=list(conFullLambdaSE,conFullThetaSE,conFullPsiSE), conFullDX=list(conFullLambdaDX,conFullThetaDX,conFullPsiDX)),
      conResOut=list(conResFit=conResFit, conResCoef=conResCoef, conResSE=list(conResLambdaSE,conResThetaSE,NA), conResDX=list(conResLambdaDX,conResThetaDX,conResPsiDX)))

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

      list(list(hypotheticalPM=pm, empiricalPM=pctmissing3), cellMissOut=yourMissinOut)
      
    }# end runMissSim()

  
  ##
  ##
  ## Aggregate the two above functions to run a single row of the results matrix:
  ##
  ##


#rowOut <- rowTask(parameters$samInc,newDat,rp,parameters)
  
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

rp <- c(1:4)

cnt <- 2

parameters <- list()
parameters$samInc <- seq(0,30,10)
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


#goBabyGo(rp,parameters)

## Let's run the bugger!!! ##

runTime <- system.time(

performParallel(count=cnt, x=rp, fun=goBabyGo, seed=mySeeds, cltype="MPI", parms=parameters)


            )

save(runTime,file="repTime.RData")

#load("missOut-run-4-omit-30-7113.40173370847.RData")
