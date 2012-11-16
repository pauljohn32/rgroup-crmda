### Paul Johnson 2011-10-28
### Adapted from mirt-sim-1.R
### Request from Rose Zheng


### Create some D dimensional MIRT data.
### The individual ability parameters (called "thetas")


library(mvtnorm)

calcCovTheta <- function(sdTheta, rhoTheta){
  ct  <- rhoTheta *  sdTheta %o% sdTheta
}
## 2012-11-16. Was suspicious of this, but result same as
## the more theoretically obvious:
## diag(sdTheta) %*% rhoTheta %*% diag(sdTheta)


simProbabilityCorrect <- function (N=0, M=0, npb=0, meanTheta, covTheta ){
  ## Get length from meanTheta
  D <- length(meanTheta)
  if (!all.equal (dim(covTheta), c(D,D))) stop("dimensions of mean and sd theta don't match")

  ## Create "theta" matrix of individual abilities on D dimensions
  theta <- rmvnorm(N, mean = meanTheta, sigma = covTheta,  method= "chol")
  ## difficulty is standard normal
  diffp <- rnorm(M, 0, 1)
  ##discrimination parameters log normal in D columns
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

  ## Create discp, the final matrix of discrimination parameters.
  ## blot out the discrimination parameters we don't want
  discp <- discraw * blotterMatrix

  ## guessing is uniform on [min,max]
  ## guesp <- runif(M, min=0.1178, max=0.3580)

  ## Calculate the "linear predictor" eta
  eta <- discp %*% t(theta) - diffp
  ## inverse link
  invlink <- 1/(1+exp(-eta))
  list(probCorrect=invlink, discrimination=discp, difficulty=diffp)
} ## Returns 3 pieces

addGuessing <- function(probabilityCorrect=NULL, cmin=0.0, cmax=0.2){
  ## PC=probability correct allows for guessing parameter
  ## Guessing C[i] is guessing param for each question
  M <- dim(probabilityCorrect)[1]
  C <- runif(M, min = cmin, max = cmax)
  PC <-  C + (1-C) * probabilityCorrect
}

## Justify previous calculation to self,
## Fiddle with some matrices
## XM <- matrix(2, nrow=3, ncol=7)
## XM
## XC * XM
## XC <- c(0.1, .2, .3)
## XC * XM
## A <- c(-1, -2, -3)
## A * XC * XM
## XC + (1-XC) * XM



## probabilityCorrect should be M X N (M items, N respondents)
simItems <- function(probabilityCorrect){
  if(!is.matrix(probabilityCorrect)) stop("probabilityCorrect is not a matrix")
  matdim <- dim(probabilityCorrect)
  ## draw random items column-by-column
  items <- apply(probabilityCorrect, 2, function(col) rbinom(n= matdim[1], pr=col, size=1))

  ## Transpose so item responses are delivered N X M
  items <- t(items)
}



###########################################################
############## Usage begins with parameters ###############
###########################################################
N <- 1000 # respondents
Cmax <- 0.2 # maximum guessing parameter
D <- 5 # dimensions of individual ability (elements in theta)
npb <- 10 # num of items "per sub block" on dimensions 2:D
M <- (D-1)*npb # total items

### The following are User judgments to create multivariate normal data
### MVN(meanTheta, covTheta)
meanTheta <- rep(0, D) ## mean 0
sdTheta <- rep(1, D) ##standard deviation of dimension
rhoTheta <- diag(D)  ##correlation, initially Uncorrelated between dimensions
covTheta  <- calcCovTheta(sdTheta, rhoTheta)


##for(i in 1:2000){
## just run one time for testing
  i <- 1
  pc <- simProbabilityCorrect(N=N, M=M, npb=npb, meanTheta, covTheta)
  pcGuessing <- addGuessing(pc$probCorrect, cmin = 0.0, cmax = Cmax)
  itemsGuessing <- simItems(pcGuessing)

  itemsNoGuessing <- simItems(pc$probCorrect)


  ## Saves to a raw text format, easier for other programs to read
  fn <- paste("items-",i,".txt", sep="")
  write.table(itemsGuessing, file="mei-dat-1000.txt", row.names=FALSE)
##}
### Sanity Check: Doublecheck difficulty against average scores

## Rose: I bet this is the one that made your computer seem to freeze
##
  plot(pc$difficulty, colMeans(itemsGuessing))
  plot(pc$difficulty, colMeans(itemsNoGuessing))

