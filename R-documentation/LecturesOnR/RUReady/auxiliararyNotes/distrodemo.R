###################################################
### chunk number 2: Roptions
###################################################
options(width=60, continue="  ") 
###Leave less white space at top
options(SweaveHooks=list(fig=function() par(mar=c(5.1, 4.1, 0.5, 2.1))))
###Sweave appears to ignore following settings 2010-03-20
ps.options(horizontal=F, onefile=F, family="Times",    paper="special", height=4, width=6 ) 
pdf.options(onefile=F,family="Times",paper="special",height=4,width=6)
options(papersize="special")




###################################################
### chunk number 3: fig1
###################################################
var1 <- rnorm(n=1500, mean=50, sd=20)
hist(x=var1, prob=T, breaks= 20, xlim=c(-10,110), ylim=c(0,0.03), xlab="A Random Sample from N(10,400)", ylab="Proportion of Observations", main="")
den1 <- density(var1)
lines(den1, lty=2, col="red")
legend("topleft",legend=c(paste("mean=",round(mean(var1),3)),paste("sd=",round(sd(var1),3))))


###################################################
### chunk number 4: fig2
###################################################
plot(den1, xlim=c(-10,110), ylim=c(0,0.03), xlab="Possible Values", type="l", lty=2, col="red",main="")
possValues <- seq(-10,110)
trueProbs <- dnorm(possValues, mean=50, sd=20)
lines(possValues, trueProbs, lty=1, col="black")
legend("topright", legend=c("true under N(50,400)","observed in sample"),lty=c(1,2),col=c("black","red"))


###################################################
### chunk number 5: fig3
###################################################
samp <- replicate(1000, mean(rnorm(n=1500, mean=50, sd=20)))
hist(samp, prob=T, breaks=20, ylim=c(0,1), xlab="Normal Sample Means", main="")
legend("topleft",legend=c(paste("mean of means=",round(mean(samp),3)),paste("sd of means=",round(sd(samp),3))))


###################################################
### chunk number 6: fig4
###################################################
hist(samp, prob=T, breaks=20, xlab="Normal Sample Means", xlim=c(-10,110), ylim=c(0,1), main="")
legend("topleft",legend=c(paste("mean of means=",round(mean(samp),3)), paste("sd of means=",round(sd(samp),3))))


###################################################
### chunk number 7: fig5
###################################################
var1 <- rexp(n=1500, rate=1/50)
hist(x=var1, prob=T, breaks= 20, xlim=c(-10,300), ylim=c(0,0.03), xlab="An Exponential Random Sample", ylab="Proportion of Observations", main="")
den1 <- density(var1)
lines(den1, lty=2, col="red")
legend("topleft",legend=c(paste("mean=",round(mean(var1),3)),paste("sd=",round(sd(var1),3))))


###################################################
### chunk number 8: fig6
###################################################
samp <- replicate(1000, mean(rexp(n=1500, rate=1/50)))
hist(samp, prob=T, breaks=20, ylim=c(0,0.5), xlab="Sample Means from Exponentials", main="")
legend("topleft",legend=c(paste("mean of means=",round(mean(samp),3)),paste("sd of means=",round(sd(samp),3))))



