
load("fitResultsList.RData")

averagedResults1000 <- fitResults$averagedResults1000
averagedResults500 <- fitResults$averagedResults500
averagedResults250 <- fitResults$averagedResults250
averagedResults125 <- fitResults$averagedResults125

#### Plottin some McGuffins ####

## logLikelihood ##

#N=125:
plot(c(10,30,50),averagedResults125[,36],col="red",ylim=c(-2400,-2200),main="logLikelihood by method and percent missing",xlab="Percent Missing",ylab="logLikelihood",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults125[,38],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults125[,34],pch=22,col="green",lty=6,type="b")
legend(10,-2350,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=250:
plot(c(10,30,50),averagedResults250[,36],ylim=c(-4800,-4600),main="logLikelihood by method and percent missing",xlab="Percent Missing",ylab="logLikelihood",col="red",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults250[,38],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults250[,34],col="green",lty=6,type="b")
legend(10,-4750,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=500:
plot(c(10,30,50),averagedResults500[,36],ylim=c(-9800,-9200),main="logLikelihood by method and percent missing (N=500)",xlab="Percent Missing",ylab="logLikelihood",col="red",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults500[,38],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults500[,34],col="green",lty=6,type="b")
legend(10,-9700,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=1000
plot(c(10,30,50),averagedResults1000[,36],ylim=c(-19600,-18600),main="logLikelihood by method and percent missing (N=1000)",xlab="Percent Missing",ylab="logLikelihood",pch=22,lty=3,col="red",type="b")
points(c(10,30,50),averagedResults1000[,38],pch=25,lty=5,col="blue",type="b")
points(c(10,30,50),averagedResults1000[,34],lty=6,col="green",type="b")
legend(10,-19400,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))


## Chi Squared ##

#N=1000:
plot(c(10,30,50),averagedResults1000[,6],col="red",ylim=c(600,1200),main="Chi Squared by method and percent missing (N=1000)",xlab="Percent Missing",ylab="Chi Squared",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults1000[,8],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults1000[,4],col="green",lty=6,type="b")
legend(10,700,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=500:
plot(c(10,30,50),averagedResults500[,6],col="red",ylim=c(200,800),main="Chi Squared by method and percent missing (N=500)",xlab="Percent Missing",ylab="Chi Squared",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults500[,8],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults500[,4],col="green",lty=6,type="b")
legend(10,300,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=250:
plot(c(10,30,50),averagedResults250[,6],col="red",ylim=c(200,800),main="Chi Squared by method and percent missing (N=250)",xlab="Percent Missing",ylab="Chi Squared",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults250[,8],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults250[,4],col="green",lty=6,type="b")
legend(10,800,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=125:
plot(c(10,30,50),averagedResults125[,6],col="red",ylim=c(200,800),main="Chi Squared by method and percent missing (N=125)",xlab="Percent Missing",ylab="Chi Squared",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults125[,8],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults125[,4],col="green",lty=6,type="b")
legend(10,800,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))


## CFI ##

#N=1000:
plot(c(10,30,50),averagedResults1000[,12],col="red",ylim=c(.8,1),main="CFI by method and percent missing (N=1000)",xlab="Percent Missing",ylab="CFI",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults1000[,14],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults1000[,10],col="green",lty=6,type="b")
legend(10,.85,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=500:
plot(c(10,30,50),averagedResults500[,12],col="red",ylim=c(.8,1),main="CFI by method and percent missing (N=500)",xlab="Percent Missing",ylab="CFI",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults500[,14],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults500[,10],col="green",lty=6,type="b")
legend(10,.85,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=250:
plot(c(10,30,50),averagedResults250[,12],col="red",ylim=c(.8,1),main="CFI by method and percent missing (N=250)",xlab="Percent Missing",ylab="CFI",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults250[,14],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults250[,10],col="green",lty=6,type="b")
legend(10,.85,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=125:
plot(c(10,30,50),averagedResults125[,12],col="red",ylim=c(.8,1),main="CFI by method and percent missing (N=125)",xlab="Percent Missing",ylab="CFI",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults125[,14],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults125[,10],col="green",lty=6,type="b")
legend(10,.85,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))


## TLI ##

#N=1000:
plot(c(10,30,50),averagedResults1000[,18],col="red",ylim=c(.8,1),main="TLI by method and percent missing (N=1000)",xlab="Percent Missing",ylab="TLI",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults1000[,20],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults1000[,16],col="green",lty=6,type="b")
legend(10,.85,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=500:
plot(c(10,30,50),averagedResults500[,18],col="red",ylim=c(.8,1),main="TLI by method and percent missing (N=500)",xlab="Percent Missing",ylab="TLI",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults500[,20],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults500[,16],col="green",lty=6,type="b")
legend(10,.85,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=250:
plot(c(10,30,50),averagedResults250[,18],col="red",ylim=c(.8,1),main="TLI by method and percent missing (N=250)",xlab="Percent Missing",ylab="TLI",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults250[,20],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults250[,16],col="green",lty=6,type="b")
legend(10,.85,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=125:
plot(c(10,30,50),averagedResults125[,18],col="red",ylim=c(.8,1),main="TLI by method and percent missing (N=125)",xlab="Percent Missing",ylab="TLI",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults125[,20],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults125[,16],col="green",lty=6,type="b")
legend(10,.85,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))


## RMSEA ##

#N=1000:
plot(c(10,30,50),averagedResults1000[,24],col="red",ylim=c(0,.14),main="RMSEA by method and percent missing (N=1000)",xlab="Percent Missing",ylab="RMSEA",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults1000[,26],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults1000[,22],col="green",lty=6,type="b")
legend(10,.03,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=500:
plot(c(10,30,50),averagedResults500[,24],col="red",ylim=c(0,.14),main="RMSEA by method and percent missing (N=500)",xlab="Percent Missing",ylab="RMSEA",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults500[,26],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults500[,22],col="green",lty=6,type="b")
legend(10,.03,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=250:
plot(c(10,30,50),averagedResults250[,24],col="red",ylim=c(0,.14),main="RMSEA by method and percent missing (N=250)",xlab="Percent Missing",ylab="RMSEA",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults250[,26],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults250[,22],col="green",lty=6,type="b")
legend(10,.03,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=125:
plot(c(10,30,50),averagedResults125[,24],col="red",ylim=c(0,.14),main="RMSEA by method and percent missing (N=250)",xlab="Percent Missing",ylab="RMSEA",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults125[,26],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults125[,22],col="green",lty=6,type="b")
legend(10,.03,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))


## SRMR ##

#N=1000:
plot(c(10,30,50),averagedResults1000[,30],col="red",ylim=c(0,.1),main="SRMR by method and percent missing (N=1000)",xlab="Percent Missing",ylab="SRMR",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults1000[,32],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults1000[,28],col="green",lty=6,type="b")
legend(10,.1,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=500:
plot(c(10,30,50),averagedResults500[,30],col="red",ylim=c(0,.1),main="SRMR by method and percent missing (N=500)",xlab="Percent Missing",ylab="SRMR",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults500[,32],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults500[,28],col="green",lty=6,type="b")
legend(10,.1,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=250:
plot(c(10,30,50),averagedResults250[,30],col="red",ylim=c(0,.1),main="SRMR by method and percent missing (N=250)",xlab="Percent Missing",ylab="SRMR",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults250[,32],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults250[,28],col="green",lty=6,type="b")
legend(10,.1,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=125:
plot(c(10,30,50),averagedResults125[,30],col="red",ylim=c(0,.1),main="SRMR by method and percent missing (N=125)",xlab="Percent Missing",ylab="SRMR",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults125[,32],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults125[,28],col="green",lty=6,type="b")
legend(10,.02,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))


## Chi Squared Difference ##

#N=1000:
plot(c(10,30,50),averagedResults1000[,2],col="red",ylim=c(300,400),main="Chi Squared Difference by method and percent missing (N=1000)",xlab="Percent Missing",ylab="Chi Squared Difference",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults1000[,3],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults1000[,1],col="green",lty=6,type="b")
legend(10,400,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=500:
plot(c(10,30,50),averagedResults500[,2],col="red",ylim=c(100,200),main="Chi Squared Difference by method and percent missing (N=500)",xlab="Percent Missing",ylab="Chi Squared Difference",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults500[,3],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults500[,1],col="green",lty=6,type="b")
legend(10,120,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

#N=250:
plot(c(10,30,50),averagedResults250[,2],col="red",ylim=c(0,100),main="Chi Squared Difference by method and percent missing (N=250)",xlab="Percent Missing",ylab="Chi Squared Difference",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults250[,3],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults250[,1],col="green",lty=6,type="b")
legend(10,20,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))


#N=125:
plot(c(10,30,50),averagedResults125[,2],col="red",ylim=c(0,100),main="Chi Squared Difference by method and percent missing (N=125)",xlab="Percent Missing",ylab="Chi Squared Difference",pch=22,lty=3,type="b")
points(c(10,30,50),averagedResults125[,3],col="blue",pch=25,lty=5,type="b")
points(c(10,30,50),averagedResults125[,1],col="green",lty=6,type="b")
legend(10,100,c("SupMat","Naive","Control"),col=c("red","blue","green"),pch=c(22,25,1),lty=c(3,5,6))

