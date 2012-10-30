RNGkind("Mersenne-Twister")

set.seed(12345)

mvec <- newCount <- oldCount <- vector("integer", 10000)

oldCount[1] <- 0; 

invisible(rgamma(1,shape=1))

newCount[1] <- .Random.seed[2]

mvec[1] <- newCount[1] - oldCount[1]

for (i in 2:10000) { 

invisible(rgamma(1, shape = 1)) 

oldCount[i] <- newCount[i-1]; newCount[i] <- .Random.seed[2]; 

if(newCount[i] > oldCount[i]) mvec[i] <- newCount[i] - oldCount[i] 

else mvec[i] <- newCount[i] - oldCount[i] + 624 

}

table(mvec)

