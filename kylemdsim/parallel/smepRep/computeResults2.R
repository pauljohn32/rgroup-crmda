rm(list=ls(all=T))
#i <- 3
stripMissFit <- function(i)
{
  load(paste("missOut-run-",i,"-N-1000.RData",sep=""))

  smFullFit <- missOut[[1]]$cellMissOut$smMissOut$smFullOut$smFullFit
  smResFit <- missOut[[1]]$cellMissOut$smMissOut$smResOut$smResFit

  impMatFunRes <- function(j)
    {
      missOut[[1]]$cellMissOut$rawMissOut[[j]]$rawResOut$rawResFit
    }
  
  impMatFunFull <- function(j)
    {
      missOut[[1]]$cellMissOut$rawMissOut[[j]]$rawFullOut$rawFullFit
    }

  resImpMat <- sapply(c(1:100),FUN=impMatFunRes)
  fullImpMat <- sapply(c(1:100),FUN=impMatFunFull)
  
  naiveFullFit <- apply(fullImpMat,1,FUN=mean)
  naiveResFit <- apply(resImpMat,1,FUN=mean)

  list(smFullFit=smFullFit,smResFit=smResFit,naiveFullFit=naiveFullFit,naiveResFit=naiveResFit)
  
}


stripControlFit <- function(i)
{
  load(paste("conOut-run-",i,"-N-1000.RData",sep=""))

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


### Make some Plots ###

load("outMat-N500-PM50.RData")

#write.csv(outMat10PM,file="outMat-N500-PM10.csv")

out <- outMat50PM

results <- matrix(NA,1000,51)

#Change in Chi Squared:
results[,1] <- out[,21]-out[,1] #control
results[,2] <- out[,61]-out[,41] #sm
results[,3] <- out[,101]-out[,81] #naive

### Raw Fit Indices ###
#Chi Squared:
results[,4] <- out[,1]
results[,5] <- out[,21]
results[,6] <- out[,41]
results[,7] <- out[,61]
results[,8] <- out[,81]
results[,9] <- out[,101]

#CFI:
results[,10] <- out[,7]
results[,11] <- out[,27]
results[,12] <- out[,47]
results[,13] <- out[,67]
results[,14] <- out[,87]
results[,15] <- out[,107]

#TLI:
results[,16] <- out[,8]
results[,17] <- out[,28]
results[,18] <- out[,48]
results[,19] <- out[,68]
results[,20] <- out[,88]
results[,21] <- out[,108]

#RMSEA:
results[,22] <- out[,16]
results[,23] <- out[,36]
results[,24] <- out[,56]
results[,25] <- out[,76]
results[,26] <- out[,96]
results[,27] <- out[,116]

#SRMR:
results[,28] <- out[,20]
results[,29] <- out[,40]
results[,30] <- out[,60]
results[,31] <- out[,80]
results[,32] <- out[,100]
results[,33] <- out[,120]

#logLikelihood:
results[,34] <- out[,9]
results[,35] <- out[,29]
results[,36] <- out[,49]
results[,37] <- out[,69]
results[,38] <- out[,89]
results[,39] <- out[,109]

### Bias ###
#Bias in Fit Indices:
results[,40] <- out[,41]-out[,1] #sm chi
results[,41] <- out[,81]-out[,1] #naive chi
results[,42] <- out[,47]-out[,7] #sm cfi
results[,43] <- out[,87]-out[,7] #naive cfi
results[,44] <- out[,48]-out[,8] #sm tli
results[,45] <- out[,88]-out[,8] #naive tli
results[,46] <- out[,56]-out[,16] #sm rmsea
results[,47] <- out[,96]-out[,16] #naive rmsea
results[,48] <- out[,60]-out[,20] #sm srmr
results[,49] <- out[,100]-out[,20] #naive srmr

# Bias in Chi Squared Difference:
results[,50] <- results[,2]-results[,1]
results[,51] <- results[,3]-results[,1]




#########  Watch Out Buddy!!!! ############


results500pm10 <- results
results500pm30 <- results
results500pm50 <- results


results250pm10 <- results
results250pm30 <- results
results250pm50 <- results


results125pm10 <- results10
results125pm30 <- results30
results125pm50 <- results50

result10Vec125 <- apply(results125pm10,2,mean)
result30Vec125 <- apply(results125pm30,2,mean)
result50Vec125 <- apply(results125pm50,2,mean)

result10Vec250 <- apply(results250pm10,2,mean)
result30Vec250 <- apply(results250pm30,2,mean)
result50Vec250 <- apply(results250pm50,2,mean)

result10Vec500 <- apply(results500pm10,2,mean)
result30Vec500 <- apply(results500pm30,2,mean)
result50Vec500 <- apply(results500pm50,2,mean)

result10Vec2 <- apply(results10,2,sd)
result30Vec2 <- apply(results30,2,sd)
result50Vec2 <- apply(results50,2,sd)

averagedResults125 <- rbind(result10Vec125,result30Vec125,result50Vec125)
averagedResults250 <- rbind(result10Vec250,result30Vec250,result50Vec250)
averagedResults500 <- rbind(result10Vec500,result30Vec500,result50Vec500)


averagedResults2 <- rbind(result10Vec2,result30Vec2,result50Vec2)

### Calculate Standardized Bias ###
bm <- cbind(averagedResults[,34:45],averagedResults2[,c(1,4,10,16,22,28)])

smStdBias <- cbind(bm[,1]/bm[,14],bm[,3]/bm[,15],bm[,5]/bm[,16],bm[,7]/bm[,17],bm[,9]/bm[,18],bm[,11]/bm[,13])
naiveStdBias <- cbind(bm[,2]/bm[,14],bm[,4]/bm[,15],bm[,6]/bm[,16],bm[,8]/bm[,17],bm[,10]/bm[,18],bm[,12]/bm[,13])


llPlot125 <- plot(c(10,30,50),averagedResults125[,36],pch=22,col="red",ylim=c(-2400,-2100),main="logLikelihood by method and percent missing",xlab="Percent Missing",ylab="logLikelihood",type="b")
llPlot125 <- points(c(10,30,50),averagedResults125[,38],pch=22,col="blue",type="b")
llPlot125 <- points(c(10,30,50),averagedResults125[,34],pch=22,col="green",type="b")

               
llPlot250 <- plot(c(10,30,50),averagedResults250[,36],pch=24,lty=2,ylim=c(-4800,-4500),main="logLikelihood by method and percent missing",xlab="Percent Missing",ylab="logLikelihood",col="red",type="b")
llPlot250 <- points(c(10,30,50),averagedResults250[,38],pch=24,lty=2,col="blue",type="b")
llPlot250 <- points(c(10,30,50),averagedResults250[,34],pch=24,lty=2,col="green",type="b")

               
llPlot500 <- plot(c(10,30,50),averagedResults500[,36],ylim=c(-10000,-9000),main="logLikelihood by method and percent missing",xlab="Percent Missing",ylab="logLikelihood",lty=3,col="red",type="b")
llPlot500 <- points(c(10,30,50),averagedResults500[,38],lty=3,col="blue",type="b")
llPlot500 <- points(c(10,30,50),averagedResults500[,34],lty=2,col="green",type="b")


chiPlot <- plot(c(10,30,50),averagedResults[,6],col="red",ylim=c(100,900),main="Chi Squared by method and percent missing",xlab="Percent Missing",ylab="chi squared",type="b")
chiPlot <- points(c(10,30,50),averagedResults[,8],col="blue",type="b")
chiPlot <- points(c(10,30,50),averagedResults[,4],col="green",type="b")

cfiPlot <- plot(c(10,30,50),averagedResults[,12],col="red",ylim=c(.8,1),main="CFI by method and percent missing",xlab="Percent Missing",ylab="CFI",type="b")
cfiPlot <- points(c(10,30,50),averagedResults[,14],col="blue",type="b")
cfiPlot <- points(c(10,30,50),averagedResults[,10],col="green",type="b")

tliPlot <- plot(c(10,30,50),averagedResults[,18],col="red",ylim=c(.8,1),main="TLI by method and percent missing",xlab="Percent Missing",ylab="TLI",type="b")
tliPlot <- points(c(10,30,50),averagedResults[,20],col="blue",type="b")
tliPlot <- points(c(10,30,50),averagedResults[,16],col="green",type="b")

rmseaPlot <- plot(c(10,30,50),averagedResults[,24],col="red",ylim=c(0,.14),main="RMSEA by method and percent missing",xlab="Percent Missing",ylab="RMSEA",type="b")
rmseaPlot <- points(c(10,30,50),averagedResults[,26],col="blue",type="b")
rmseaPlot <- points(c(10,30,50),averagedResults[,22],col="green",type="b")

srmrPlot <- plot(c(10,30,50),averagedResults[,30],col="red",ylim=c(0,.12),main="SRMR by method and percent missing",xlab="Percent Missing",ylab="SRMR",type="b")
srmrPlot <- points(c(10,30,50),averagedResults[,32],col="blue",type="b")
srmrPlot <- points(c(10,30,50),averagedResults[,28],col="green",type="b")

chiDiffPlot <- plot(c(10,30,50),averagedResults[,2],col="red",ylim=c(0,100),main="Chi Squared Difference by method and percent missing",xlab="Percent Missing",ylab="Chi Squared Difference",type="b")
srmrPlot <- points(c(10,30,50),averagedResults[,3],col="blue",type="b")
srmrPlot <- points(c(10,30,50),averagedResults[,1],col="green",type="b")



### Plot SD's ###
chiPlotSD <- plot(c(10,30,50),averagedResults2[,6],col="red",ylim=c(20,50),main="Between-replication SD of Chi Squared by method and percent missing",xlab="Percent Missing",ylab="SD",type="b")
chiPlot <- points(c(10,30,50),averagedResults2[,8],col="blue",type="b")
chiPlot <- points(c(10,30,50),averagedResults2[,4],col="green",type="b")

cfiPlotSD <- plot(c(10,30,50),averagedResults2[,12],col="red",ylim=c(0,.01),main="Between-replication SD of CFI by method and percent missing",xlab="Percent Missing",ylab="SD",type="b")
cfiPlot <- points(c(10,30,50),averagedResults2[,14],col="blue",type="b")
cfiPlot <- points(c(10,30,50),averagedResults2[,10],col="green",type="b")

tliPlotSD <- plot(c(10,30,50),averagedResults2[,18],col="red",ylim=c(0,.01),main="Between-replication SD of TLI by method and percent missing",xlab="Percent Missing",ylab="SD",type="b")
tliPlot <- points(c(10,30,50),averagedResults2[,20],col="blue",type="b")
tliPlot <- points(c(10,30,50),averagedResults2[,16],col="green",type="b")

rmseaPlotSD <- plot(c(10,30,50),averagedResults2[,24],col="red",ylim=c(0,.01),main="Between-replication of RMSEA by method and percent missing",xlab="Percent Missing",ylab="SD",type="b")
rmseaPlot <- points(c(10,30,50),averagedResults2[,26],col="blue",type="b")
rmseaPlot <- points(c(10,30,50),averagedResults2[,22],col="green",type="b")

srmrPlot <- plot(c(10,30,50),averagedResults2[,30],col="red",ylim=c(0,.01),main="Between-replication of SRMR by method and percent missing",xlab="Percent Missing",ylab="SD",type="b")
srmrPlot <- points(c(10,30,50),averagedResults2[,32],col="blue",type="b")
srmrPlot <- points(c(10,30,50),averagedResults2[,28],col="green",type="b")

chiDiffPlotSD <- plot(c(10,30,50),averagedResults2[,2],col="red",ylim=c(0,50),main="Between-replication of Chi Squared Difference by method and percent missing",xlab="Percent Missing",ylab="SD",type="b")
srmrPlot <- points(c(10,30,50),averagedResults2[,3],col="blue",type="b")
srmrPlot <- points(c(10,30,50),averagedResults2[,1],col="green",type="b")
