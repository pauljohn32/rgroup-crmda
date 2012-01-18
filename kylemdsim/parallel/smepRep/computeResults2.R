rm(list=ls(all=T))

stripMissFit <- function(i)
{
  load(paste("missOut-run-",i,"-omit-375.RData",sep=""))

  smFullFit <- missOut[[3]]$cellMissOut$smMissOut$smFullOut$smFullFit
  smResFit <- missOut[[3]]$cellMissOut$smMissOut$smResOut$smResFit

  impMatFunRes <- function(j)
    {
      missOut[[3]]$cellMissOut$rawMissOut[[j]]$rawResOut$rawResFit
    }
  
  impMatFunFull <- function(j)
    {
      missOut[[3]]$cellMissOut$rawMissOut[[j]]$rawFullOut$rawFullFit
    }

  resImpMat <- sapply(c(1:100),FUN=impMatFunRes)
  fullImpMat <- sapply(c(1:100),FUN=impMatFunFull)
  
  naiveFullFit <- apply(fullImpMat,1,FUN=mean)
  naiveResFit <- apply(resImpMat,1,FUN=mean)

  list(smFullFit=smFullFit,smResFit=smResFit,naiveFullFit=naiveFullFit,naiveResFit=naiveResFit)
  
}


stripControlFit <- function(i)
{
  load(paste("conOut-run-",i,"-omit-375.RData",sep=""))

  conFullFit <- conOut$conFullOut$conFullFit
  conResFit <- conOut$conResOut$conResFit

  list(conFullFit=conFullFit,conResFit=conResFit)

}

repVec <- c(1:1000)

outMat <- matrix(data=NA,nrow=length(repVec),ncol=120,dimnames=list(NULL,rep(c("chisq","df","pvalue","baseline.chisq","baseline.df","baseline.pvalue","cfi","tli","logl","unrestricted.logl","npar","aic","bic","ntotal","bic2","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr"),6)))


missinOut <- lapply(repVec,FUN=stripMissFit)
controlOut <- lapply(repVec,FUN=stripControlFit)

for(i in 1:length(repVec))
  {
    outMat[i,1:20] <- controlOut[[i]]$conFullFit
    outMat[i,21:40] <- controlOut[[i]]$conResFit
    outMat[i,41:60] <- missinOut[[i]]$smFullFit
    outMat[i,61:80] <- missinOut[[i]]$smResFit
    outMat[i,81:100] <- missinOut[[i]]$naiveFullFit
    outMat[i,101:120] <- missinOut[[i]]$naiveResFit 
  }



outMat10PM <- outMat
outMat30PM <- outMat
outMat50PM <- outMat

save(outMat10PM,file="outMat-N125-PM10.RData")
save(outMat30PM,file="outMat-N125-PM30.RData")
save(outMat50PM,file="outMat-N125-PM50.RData")
