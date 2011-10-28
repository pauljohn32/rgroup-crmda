### Paul Johnson 2011-11-13
### mei-sim-1.R
## adapted from
### Paul Johnson 2010-11-08
### mirt-sim-1.R
### Request from Rose Zheng



### Create some D dimensional MIRT data.
### The individual ability parameters (called "thetas")




library(mvtnorm)

N <- 1000 # respondents

D <- 5 # dimensions of individual ability (elements in theta)
npb <- 10 # num of items "per sub block" on dimensions 2:D
M <- (D-1)*npb # total items

### First, create D-dimensional multivariate normal data.
### MVN(meanTheta, covTheta)
### Correlation "rhoTheta" and standard deviation customizable
meanTheta <- rep(0, D) ## mean 0
sdTheta <- rep(1, D) ##standard deviation of dimension
rhoTheta <- diag(D)  ##correlation, initially Uncorrelated between dimensions
covTheta  <- rhoTheta *  sdTheta %o% sdTheta
theta    <- rmvnorm(N, mean = meanTheta, sigma = covTheta,
             method= "chol")


########################################
# difficulty is standard normal
diffp <- rnorm(M, 0, 1)

###discrimination parameters log normal in D columns
discraw <- matrix(rlnorm(D*M, m=0, sd=1), ncol=D)

##Create a "mask" to generate the bi-factor structure
## one column of 1's, others grouped like
##  1 1 0 0 0 0
##  1 1 0 0 0 0
##  1 0 1 0 0 0
##  1 0 1 0 0 0
##  1 0 1 0 0 0
## and so forth. Must specify which rows have 1 on which column.

blotterMatrix <- matrix(0, nrow=M, ncol=D-1)

for ( i in 1:(D-1)){
  blotterMatrix[(1 + (i-1)*npb): (i*npb), i] <- 1
              }

##Put 1's in first column for factor that applies to all items
blotterMatrix <- cbind(1, blotterMatrix)

### Create discp, the final matrix of discrimination parameters.
### blot out the discrimination parameters we don't want
discp <- discraw * blotterMatrix

# guessing is uniform on [min,max]
# guesp <- runif(M, min=0.1178, max=0.3580)

## Calculate the "linear predictor" eta
eta <- discp %*% t(theta) - diffp
## inverse link
invlink <- 1/(1+exp(-eta))
## draw random items
items <- apply(invlink, 2, function(col) rbinom(n= M, pr=col, size=1)) 
items <- t(items) ## transpose to put in usual format

### Sanity Check: Doublecheck difficulty against average scores 
plot(diffp, colMeans(items))



