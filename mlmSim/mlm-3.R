## Paul Johnson <pauljohn@ku.edu>
## 2012-08-26

## mlm-3. This is step 3 the my R programming tutorial. I'm building
## up to a simulation of a lot of datasets and multi-level models.
## Now abstract and create some funtions to automate work. Here, the
## functions are at the top. Scroll down to "Demonstration starts
## here!" for the usage.

## Here's what needs to be generated

## Y = X * beta + Z * b + E
##
## In the simple case, say 3 observations per cluster. I'll number
## the observations consecutively here, I think it helps to see
## the pattern

## cluster 1:
## Y1 = beta0 + X1 beta1 + b1 + E1
## Y2 = beta0 + X2 beta1 + b1 + E2
## Y3 = beta0 + X3 beta1 + b1 + E3
## cluster 2:
## Y4 = beta0 + X4 beta1 + b2 + E4
## Y5 = beta0 + X5 beta1 + b2 + E5
## Y6 = beta0 + X6 beta1 + b2 + E6
## cluster 3:
## Y7 = beta0 + X7 beta1 + b3 + E7
## Y8 = beta0 + X8 beta1 + b3 + E8
## Y9 = beta0 + X9 beta1 + b4 + E9

## See the main idea? b1, b2, b3, and E1,...E9 are random. The
## interesting part here is that a common random effect is applied within
## each cluster.

## Now insert cluster-dependent effects of the predictor X.

## cluster 1:
## Y1 = beta0 + X1 beta1 + b1 + b5 X1 + E1
## Y2 = beta0 + X2 beta1 + b1 + b5 X2 + E2
## Y3 = beta0 + X3 beta1 + b1 + b5 X3 + E3
## cluster 2:
## Y4 = beta0 + X4 beta1 + b2 + b6 X4 + E4
## Y5 = beta0 + X5 beta1 + b2 + b6 X5 + E5
## Y6 = beta0 + X6 beta1 + b2 + b6 X6 + E6
## cluster 3:
## Y7 = beta0 + X7 beta1 + b3 + b7 X7 + E7
## Y8 = beta0 + X8 beta1 + b3 + b7 X8 + E8
## Y9 = beta0 + X9 beta1 + b3 + b7 X9 + E9

## This adds random effects b5, b6, b7, to the 3 clusters, and their
## effects depend on the value of X within the cluster.

## We need to create more columns of Xs, we need to decide how they are
## correlated with each other and whether their effects are linked to cluster
## membership.




## vech2Corr is a convenience function for creating correlation matrices
## from a vector of the lower triangular values.
##' Convert the vech (column of strictly lower trianglar values from a matrix) into a correlation matrix.
##'
##' Similar functions exist in many packages, see  \code{vec2sm} in corpcor, \code{xpnd} in MCMCpack
##' @param vech A vector of values to be placed into the strictly lower triangle of a matrix. All values must be in the [0,1] interval (because they are correlations).
##' @return A symmetric correlation matrix, with 1's on the diagonal.
##' Paul Johnson <pauljohn@@ku.edu>
vech2Corr <- function(vech){
    ##compute number of rows from vech. diag not in the vech!
    n = (sqrt(1 + 8 * length(vech)) + 1)/2
    if (!as.integer(n) == n) stop(deparse(substitute(vech)), " must have the correct number of elelemnts to fill in a strictly lower triangle in a square matrix.")
    if(any(vech > 1 | vech < -1)) stop("All values in ", deparse(substitute(vech)), " must be in the interval [-1,1]")
    X <- matrix(NA, nrow = n, ncol = n)
    X[lower.tri(X, diag = FALSE)] <- vech
    X[upper.tri(X)] <- t(X)[upper.tri(X)]
    diag(X) <- 1
    X
}



##  genMLMData
##' Generate simulated regression data with various types of mixed
##' effects.  This creates a matrix X of MVN values, generates "random
##' effects" columns for each of the regression coefficients. The
##' dimensions of X are determined implicitly from the parameters that
##' the user supplies. The output includes a data frame with several possible dependent variables, one that has NO cluster-level effects, and others which include the cluster-level random intercepts and random slopes.
##'
##' The user supplies the "true" beta coefficients, the slopes in
##' y = cbind(1, X) %*% beta =  beta[1] + beta[2]X[,1]...
##'
##' The function deduces the required width of X from beta. beta is
##' the vector of "true" regression coefficients, (b0, b1, b2, ...,
##' bk), where b0 is the intercept and the others are slope
##' coefficients. X does not include a column of 1's for intercepts.
##' Thus X is (length(beta) - 1) columns wide. The number of rows in X
##' is randomly determined, it reflects M, Nmin, and Nvar. Thats M
##' clusters of observations, Nmin is minimum size of sample within
##' cluster, and Nvar, variation in cluster size (put to 0 for
##' balanced). To select a balanced sample with exactly N units in
##' each cluster, specify M, Nmin, and Nvar=0.
##'
##' The user can specify the "true" correlation matrix for the columns
##' of X, as well as a "true" correlation matrix for the columns of
##' the random effects that perturb the intercepts and slopes.
##'
##' The mixed effects are multivariate normal with expected value 0
##' and a covariance matrix that is derived from the user's input. The
##' user should specify a vector of random effect standard deviations
##' in the argument STDEb and a covariance matrix for those random
##' effects, bcorr.  The length of STDEb must equal the row and column
##' length of bcorr.
##'
##' The user can input the information for Xcorr and bcorr in a
##' flexible way. A full, symmetric correlation matrix will be
##' accepted, but the user can also supply a single value, which is
##' assumed to be the common correlation among all of the variables,
##' or a vech, the strictly lower triangle values from a matrix.  The
##' function will convert those values into the required correlation
##' matrix.
##'
##' @param M Optional. Integer, number of clusters, default = 100.
##' @param Nmin Optional. Integer, minimum number of observations per cluster, default = 20.
##' @param Nvar Optional.  Real number, variance of cluster size (implemented as
##' Poisson random), default = 0 (representing balanced data)
##' @param beta Required. Vector of real numbers, true beta for
##' fixed effects intercept and X's. d values must be specified
##' @param Xmeans Optional. Real number, possibly a vector of real numbers. Determines the means of the columns of the X.  One
##' number (common mean to all X columns) or a vector of means
##' (length(beta) - 1), default is 200.
##' @param Xsds Optional.  Real number, either single value or a vector. Standard deviations of X columns, length = (d-1)
##' @param Xcorr Correlation matrix of X columns. If the length of
##' beta is d, this should include enough information for genMLMData
##' to construct a (d-1)x(d-1) correlation matrix. The user can supply
##' a correlation matrix, of course. However, simpler input strategies
##' are allowed. If no value is specified for Xcorr, the function
##' assumes the columns of X are uncorrelated. If a single value is
##' specified, that will be the common correlation between all of the
##' columns. If a vector with the "right number" of values is supplied
##' to fill up the strictly lower triangle of a matrix, (commonly
##' called a vech), then the matrix will be constructed from it.

##' @param STDEE standard deviation of the error term
##' @param STDEb standard deviation of random effects--perturbations
##' applied to b's within the clusters
##' @param bcorr Correlation among random effects. If not specified, all random effects are uncorrelated. If a single value is specified, that is assumed to be the common correlation among all columns. The user may also supply a symmetric matrix of correlation values or a vech, a vector of values to fill in the strictly lower triangle of the bcorr matrix.
##' @param seed  Random generator starting point. If omitted, current R seed is used.
##' @return A list with:
##' 1. dat: a data frame with columns
##' Midx(cluster indicator), Iidx (individual indicator within cluster),
##' X columns,  MVN predictors
##' y1: ordinary "homoskedastic" regression (no grouping effect)
##' y2: Includes clustered "random intercepts"
##' y3: Includes "random intercepts" and "random slope" for 1 predictor (x1)
##' y4: Includes all random effects for all columns of X
##' 2. reffects. A list of random effects data frames. One random effect for the intercept (additive random effect within the clusters), plus one for each column of the predictor matrix, is supplied. This is declared as an object of type "ranef.mer" so that plotting functions from lme4 will work treat it as if it is random effects that were estimated.
##' @author Paul Johnson <pauljohn@@ku.edu>
genMLMData <- function(M = 100, Nmin = 20, Nvar = 5,
                       beta,
                       Xmeans = 200,
                       Xsds = 40,
                       Xcorr = 0,
                       STDEE = 15, STDEb = rep(0, length(beta)),
                       bcorr = 0,
                       seed) {
    if (missing(beta)) stop("a vector of beta coefficients must be suppiled in order to manufacture a model, such as y = beta[1] + beta[2] * x1 + beta[3] * x2 + ...")
    d <- length(beta)

    cl <- match.call()

    ## check x is of length x, or use a single value x to create a
    ## vector of length d
    checkVec <- function(x = NULL, d = NULL){
        if (length(x) == 1) {
            x <- rep(x, d) #assign same for all
        } else if (length(x) > d || length(x) < d){
            msg <- paste(deparse(substitute(x)), "has too", ifelse(length(x) > d, "many","few"), "elements. Please specify", d, "elements.  Or just specify 1, we will use that one value to manufacture a vector for you.")
            stop(msg)
        }
        x
    }

    ## Correlation matrices: check and create.
    ## Check X and do the right thing. If X is a matrix, check that it
    ## is a valid correlation matrix. If its a single value, use that
    ## to fill up a matrix. If itis a vector, try to use it as a vech
    ## to fill the lower triangle.
    checkMat <- function(X, d) {
        if (is.matrix(X)){
            stopifnot (isSymmetric(X))
            stopifnot (dim(X)[1] == d)
        } else if (length(X) == 1) {
            if ( X < -1 | X > 1 ) stop(paste("The value of of a correlation should be in [-1,1]"))
            X <- matrix(X, nrow = d, ncol = d)
            diag(X) <- 1.0
        } else if (is.vector(X)){
            X <- vech2Corr(X)
        } else {
            stop(paste("genMLMData cannot understand the value supplied for argument", deparse(substitute(X)),".\n That should be either a", d, " x ", d, "symmetric matrix, \n or a vech of the strictly lower triangular part of a matrix, or \n one single value, which we will use to fill up a matrix."))
        }
        X
    }

    Xmeans <- checkVec(Xmeans, d-1)
    Xsds <- checkVec(Xsds, d-1)

    if(!missing(seed)) set.seed(seed) ## replication

    Xcorr <- checkMat(Xcorr, d-1)
    bcorr <- checkMat(bcorr, d)

    require(MASS) ## for rmvnorm

    N = Nmin + rpois(M, lambda = Nvar) ## Cluster sizes

    ## Create Midx and Iidx indexes for book keeping.
    ## Midx, "M index" shows with which cluster an observation belongs.
    Midx <- unlist(lapply(1:M, function(x) rep(x, each=N[x])))
    ## Iidx, "I index" indexes cluster members.
    Iidx <- unlist(lapply(1:M, function(x) seq(1, N[x], by=1)))

    ## draw n observations, given Xmeans, Xsds, Xcorr
    genX <- function(n, Xmeans, Xsds, Xcorr){
        Xsigma <- diag(Xsds) %*% Xcorr %*% diag(Xsds)
        X.mat <- mvrnorm(n = n, mu = Xmeans, Sigma = Xsigma)
        dimnames(X.mat)[[2]] <- paste("x", 1:(d-1), sep = "")
        X.mat
    }
    ## For now, X is not cluster dependent, so just draw all N
    ## from same distribution
    X.mat <- genX(sum(N), Xmeans = Xmeans, Xsds = Xsds, Xcorr = Xcorr)
    ##   FIXED Part   +  RANDOM PART
    E <-  rnorm(sum(N), m = 0, s = STDEE)
    y1 <- cbind(1, X.mat) %*% beta + E

    ## Layer on additive group level error in y2 and y3
    bsigma <- diag(STDEb) %*% bcorr %*% diag(STDEb)
    reffects <- mvrnorm(M, mu = rep(0, (d)), Sigma = bsigma)
    colnames(reffects) <- paste("x", 0:(d-1), sep="")
    ## Note I'm calling an intercept random effect reffect[ , "x0"]

    ## In y2, include random intercept
    y2 <- y1 + reffects[ ,"x0"][Midx]

    ## In y3, add in random slope effect, just for one variable
    y3 <- y2 +  X.mat[, "x1"] * reffects[ ,"x1"][Midx]

    ## In y4, add in all random slope effects
    y4 <- y2 + rowSums(X.mat * reffects[Midx, -1])

    dat <- data.frame(Midx, Iidx, y1, y2, y3, y4, as.data.frame(X.mat))
    rownames(dat) <- paste(dat$Midx, dat$Iidx, sep=".") ##may help bookkeeping later
    reffects <- list(Midx = as.data.frame(reffects))
    class(reffects) <- "ranef.mer"
    invisible(list(dat = dat,
                   reffects = reffects, call = cl,
                   true = list(beta = beta, Xmeans = Xmeans, Xsds = Xsds, Xcorr = Xcorr, STDEE = STDEE, STDEb = STDEb, bcorr = bcorr )))
}


##' This is a plot function for objects produced by the lmList
##' function in the lme4 package.
##'
##' This is like termplot, but for clustered data. Choose your iv and
##' dv names then watch the technicolor fun!  It should scan through
##' the set of fitted models and plot each one.  The observations and
##' lines for each cluster of observations are color-coded.
##'
##' @param regs An lmList object, including one regression for each
##' subgroup of observations
##' @param data The full data frame from which the regressions are
##' estimated.
##' @param iv The name of the variable that is to be used on the X
##' axis in plots.
##' @param cluster The name of the variable that designates the
##' clusters in data
##' @param col An optional color vector. The default uses the rainbow
##' function to asisgn colors
##' @return The "newdata" objects that are used to calculate predicted
##' values in each separate sub-unit regression
##' @author Paul Johnson <pauljohn@@ku.edu>
spaghettiPlot.lmList <- function(regs, data, iv, cluster = "Midx", col
= rainbow(length(regs))){
    ## argument check
    stopifnot(class(regs) == "lmList")
    dv <- as.character(formula(terms(regs[[1]]))[[2]]) ## get dv name
    if (sum(!c(dv, iv) %in% colnames(data)) > 0) stop("dv and iv must be column names in data")
    plot(data[, dv] ~ data[ , iv], col = col[data[ ,cluster]], lwd = 0.6, main = "Regression Within Clusters", xlab = iv, ylab = dv )
    newdat <- vector("list", length(regs))
    for( i in seq_along(regs) ){
        imod <- regs[[i]]
        imf <- model.frame(imod) #data set for cluster i
        ivrange <- range(imf[, iv]) ## use cluster-specific ranges this time
        newdf <- data.frame( iv = ivrange, lapply(imf[ , -which(colnames(imf) %in% c(dv,iv))], mean, na.rm = TRUE))
        colnames(newdf)[1] <- iv  ##pita!
        newdf$pred <- predict( imod, newdata = newdf)
        lines(ivrange, newdf$pred, col = col[i])
        newdf$Midx <- i
        newdat[[i]] <- newdf
    }
    newdata <- do.call("rbind", newdat)
    invisible(newdata)
}



Mrdiag <- function(RE, data, id = "Midx", col = rainbow(nrow(RE))){
    if(missing(RE) || missing(data)) stop("Must specify RE and data")
    par.orig <- par(no.readonly = TRUE)

    RE <- zapsmall(RE, digits = 5)
    ##purge columns that contain only one unique value
    RE <- RE[ , which(lapply(apply(RE, 2,  unique), length) > 1)]
    REnam <- colnames(RE)
    RE.ncol <- length(REnam)
    dev.new()
    par(mfrow = c( RE.ncol - 1, 2)) ##ignore intercept

    for (i in 2:RE.ncol){
        plot(RE[, 1], RE[ , REnam[i]], xlab = paste("Random Effect on: ", id), ylab = paste("Random Effect on: ", REnam[i], sep=""),  main = "True Random Effects", col = col)


        plot(RE[ , 1][dat$Midx], data[ , REnam[i]] * RE[ , REnam[i]][data[, id]], col = col[data[ ,id]], main = "Marginal Random Effects", xlab = paste("Random Effect on: ", id), ylab = paste("Marginal Slope Random Effect (b*", REnam[i], ")", sep=""))
    }
    par(par.orig)
    require(rockchalk)
    list(summarize = rockchalk::summarizeNumerics(RE), cor = cor(RE))
}

## Just started 2012-10-16
analyzeMLMData <- function(dat, id){
    ## Start with regressions that ignore the clustering
    ## y1 has no random effects, m1.1 should be OK
    m1.1s <- summary(lm(y1 ~ x1 + x2 + x3, dat, model = FALSE))
    m1.2s <- summary(lm(y2 ~ x1 + x2 + x3, dat, model = FALSE))
    m1.3s <- summary(lm(y3 ~ x1 + x2 + x3, dat, model = FALSE))
    colnames(dat)[which(colnames(gd$dat)== id)] <- "index"
    m2.1s <- summary(lm(y3 ~ x1 + x2 + x3 + index, data=dat, model = FALSE))


    m3list <- lmList(y3 ~ x1 + x2 + x3 | index, data = dat, pool = FALSE)

    ## The fixed effects "dummy variable" regression with ONLY THE
    ## "correct" interaction included
     m2.2 <- lm(y3 ~  x2 + x3 + x1 * as.factor(index), data=dat, model = FALSE)
     m2.2s <- summary(m2.2)



##     m3list.newdat$m2.2 <-predict(m2.2, newdata = m3list.newdat)

## ## The fixed effects "dummy variable" regression with interaction
## ## for all of the x's.
    m2.3 <- lm(y3 ~  (x2 + x3 + x1) * as.factor(Midx), data=dat)
    m2.3s <- summary(m2.3)
## m3list.newdat$m2.3 <-predict(m2.3, newdata = m3list.newdat)


    list(m1.1s, m1.2s, m1.3s, m2.1s, m2.2s, m2.3s)

## ## If a cluster has 3 or fewer cases, this warning will appear
## ## Warning message:
## ## In predict.lm(m3list[[i]], newdata = data.frame(x1 = x1range, x2 = mean(m3mf$x2),  :
## ##   prediction from a rank-deficient fit may be misleading


## ## Draw new graphs on a new device, so we can compare
## dev.new()

## ##
## ## Estimate this as a mixed-effects model with lme4

## ## mm2: Just the random intercept, no random slope
## mm2 <- lmer( y2 ~ x1 + x2 + x3 + (1 | Midx), data=dat)
## summary(mm2)

}



###############################
### "Demonstration starts here!"
###############################

library(rockchalk) ## for summarize
library(lme4) ## for lmer and lmList


    ## Create the X matrix.
Xmeans <- c(20, 30, 40)
Xsds <- c(40, 40, 40)
Xcorr <- c(0.4, 0.3, 0.0) ## just the strictly lower triangle values

## The true fixed effect values of the regression intercept and slopes
beta <- c(0.4, 0.3, -0.1, -1.1)

STDEE <- 5
## Correlations among random effects for columns, first is intercept.
STDEb <- rep(0, length(beta)) ##default random effects 0
STDEb[1] <- 10 ## the cluster-level intercept random effect,
STDEb[2] <- .705  ## x1 slope disturbance
STDEb[3] <- 1.5 ##x2 disturbance
## Create a symmetric correlation matrix from the vech
bcorr <- c(0.2, 0.2, 0, 0, 0, 0)

M <- 4  ## number of clusters
Nmin <- 1000 ## number of members in smallest cluster
Nvar <- 0 ## variance in number of members of clusters


gd <- genMLMData(M = M, Nmin = Nmin, Nvar = Nvar, beta = beta,
              Xmeans = Xmeans, Xsds = Xsds,  Xcorr = Xcorr,
              STDEE = STDEE, STDEb = STDEb, bcorr = bcorr, seed = 441123)
dat <- gd$dat
reffects <- gd$reffects

## Inspect the "true random effects"
dotplot(reffects) ## uses dotplot.ranef.mer from lme4
Mrdiag(reffects[[1]], dat)



## get M unique
## mycolors <- gray.colors(M)
## More colorful mycolors ?
mycolors <- rainbow(M)

## Fit separate linear models, one for each cluster
m3list <- lmList(y3 ~ x1 + x2 + x3 | Midx, data=dat, pool = FALSE)

## Plot those. Fun!
m3list.newdat <- spaghettiPlot.lmList(m3list,  iv="x1", cluster = "Midx",  dat = dat, col = mycolors)


## Run the following a few times, changing M and Nmin
gd2 = genMLMData(M=50, Nmin = 20, Nvar = 0, Xmeans = c(10, 20, 100),
              Xcorr = Xcorr, beta = beta,
              STDEE = 159, STDEb = STDEb, bcorr = bcorr)
Mrdiag(gd2$reffects[[1]], data=gd2$dat)





beta <- c(0.4, 0.3, -0.1, -1.1)
STDEb <- rep(0, length(beta))
STDEb[1] <- 10 ## the intercept,
STDEb[2] <- .5  ## x1 slope disturbance
STDEb[3] <- .2
## Create a symmetric correlation matrix from the vech
bcorr <- vech2Corr(c(0.2, 0.1, 0.5, 0, 0, 0))
diag(bcorr) <- 1
bcorr

M <- 40
Nmin <- 20
Nvar <- 0 ## balanced data

gd <- genMLMData(M = M, Nmin = Nmin, Nvar = Nvar, beta = beta,
              Xmeans = c(10, 20, 100), Xcorr = Xcorr,
              STDEE = 159, STDEb = STDEb, bcorr = bcorr, seed = 441123)
dat <- gd$dat
reffects <- gd$reffects

m3list <- lmList(y3 ~ x1 + x2 + x3 | Midx, data=gd$dat, pool = FALSE)

m3list.newdat <- spaghettiPlot.lmList(m3list,  iv="x1", cluster = "Midx",  dat = dat)
Mrdiag(gd$reffects[[1]], data=gd$dat)





























## bcorr <- vec2sm(c(0.99,0,0,0,0, 0))
## diag(bcorr) <- 1
## ##leave more defaults at work
## gd <- genData( M=233, Nmin = 10, Nvar = 0, Xcorr = Xcorr, STDEE = 15,
##               STDEb = c(0.5, 0.1, 0, 0), bcorr = bcorr, seed=234)
## dat <- gd$dat
## reffects <- gd$reffects

## m3list <- lmList(y3 ~ x1 + x2 + x3 | Midx, data=dat, pool = FALSE)

## ## Plot those. Fun!
## spaghettiLMplot(m3list,  iv="x1", cluster = "Midx",  dat= dat)

## ## Look over the random effects, row by row.
## Mrdiag(reffects)

## Mrdiag(reffects, "b2")

## Start with regressions that ignore the clustering
## y1 has no random effects, m1.1 should be OK
m1.1 <- lm(y1 ~ x1 + x2 + x3, data=dat)
summary(m1.1)

## y2 has random intercepts
m1.2 <- lm(y2 ~ x1 + x2 + x3, data=dat)
summary(m1.2)

## y3 has random intercepts and random slopes
m1.3 <- lm(y3 ~ x1 + x2 + x3, data=dat)
summary(m1.3)

## The fixed effects "dummy variable" regression.
m2.1 <- lm(y3 ~ x1 + x2 + x3 + as.factor(Midx), data=dat)
summary(m2.1)
m3list.newdat$m2.1 <-predict(m2.1, newdata = m3list.newdat)


## The fixed effects "dummy variable" regression with ONLY THE
## "correct" interaction included
m2.2 <- lm(y3 ~  x2 + x3 + x1 * as.factor(Midx), data=dat)
summary(m2.2) ## cool
m3list.newdat$m2.2 <-predict(m2.2, newdata = m3list.newdat)

## The fixed effects "dummy variable" regression with interaction
## for all of the x's.
m2.3 <- lm(y3 ~  (x2 + x3 + x1) * as.factor(Midx), data=dat)
summary(m2.3)
m3list.newdat$m2.3 <-predict(m2.3, newdata = m3list.newdat)

## If a cluster has 3 or fewer cases, this warning will appear
## Warning message:
## In predict.lm(m3list[[i]], newdata = data.frame(x1 = x1range, x2 = mean(m3mf$x2),  :
##   prediction from a rank-deficient fit may be misleading


## Draw new graphs on a new device, so we can compare
dev.new()

##
## Estimate this as a mixed-effects model with lme4

## mm2: Just the random intercept, no random slope
mm2 <- lmer( y2 ~ x1 + x2 + x3 + (1 | Midx), data=dat)
summary(mm2)
m3list.newdat$mm2 <- predict(mm2, newdata = m3list.newdat)

mm2VarCorr <- VarCorr(mm2)
mm2VarCorr

cor(fitted(mm2), dat$y2)

## Both random intercept and random slope, not correlated with each other
## Depending on bcorr, this may be a 'wrong model'.
mm3 <- lmer( y3 ~ x1 + x2 + x3 + (1|Midx) + (0 + x1 | Midx), data=dat, verbose=1)
summary(mm3)
mm3VarCorr <- VarCorr(mm3)
mm3VarCorr

cor(fitted(mm3), dat$y3)



## Yank out the standard deviation estimates. idiom stolen from formatVC in lmer code.
mm3reStdDev <- c(lapply(mm3VarCorr, attr, "stddev"), list(Residual = attr(mm3VarCorr, "sc")))
mm3reStdDev


mm3reStdDev[["Midx"]]["(Intercept)"] - STDEb0
##mm3reStdDev[["Midx"]]["x1"] - STDEb1
## Can't figure how to get STD(b1)
mm3reStdDev[["Residual"]] - STDEE

## The "right" model allows the random effects to be correlated (Supposing
## bcorr not 0).
mm4 <- lmer( y3 ~ x1 + x2 + x3 + (x1 | Midx), data=dat)
summary(mm4)
mm4VarCorr <- VarCorr(mm4)
mm4VarCorr

cor(fitted(mm4), dat$y3)

plot(fitted(mm4), dat$y3, col=dat$Midx)



## Yank out the standard deviation estimates
mm4reStdDev <- c(lapply(mm4VarCorr, attr, "stddev"), list(Residual = attr(mm4VarCorr, "sc")))
mm4reStdDev


## Ever do summary stats on the predicted random effects?
mm4ranef <- ranef(mm4, postVar = TRUE) ## a ranef.mer object,  a list that includes one data frame
apply(mm4ranef[["Midx"]], 2, mean)
apply(mm4ranef[["Midx"]], 2, sd)
#summarize(mm4ranef$Midx)
summary(mm4raner$Midx)
cor(mm4ranef$Midx)

dotplot(mm4ranef)

m3newdat$mm4pred <- predict(mm4, newdata = m3newdat)

plot(y3 ~ x1, data=dat, col=mycolors[Midx], main="lmer mixed model predictions")
by(m3newdat, m3newdat$Midx, function(x) { lines(x$x1, x$mm4pred, col=mycolors[x$Midx])})


## Double-check prediction values
mm4b <- fixef(mm4) ## coef(summary(mm4))[ ,1]
mm4vnames <- names(mm4b)
##  b0 + raneff(intercept, j)
mm4inteffect <- mm4b["(Intercept)"] +  mm4ranef[[1]][m3newdat$Midx, 1]
mm4x1effect <- m3newdat[ , c("x1")] * (mm4b["x1"] + mm4ranef[[1]][m3newdat$Midx, 2] )
mm4mmpred2 <- mm4inteffect + mm4x1effect +  as.matrix(m3newdat[ ,c("x2","x3") ]) %*%  mm4b[c("x2","x3")]
m3newdat$mm4manualpred <-  mm4inteffect + mm4x1effect +  as.matrix(m3newdat[ ,c("x2","x3") ]) %*%  mm4b[c("x2","x3")]

cbind(m3newdat$mm4manualpred, m3newdat$mm4pred)

## OK, looks good
plot(y3 ~ x1, data=dat, col=mycolors[Midx], main="lmer mixed model predictions")
by(m3newdat, m3newdat$Midx, function(x) { lines(x$x1, x$mm4manualpred, col=mycolors[x$Midx])})




## Now grab estimates from lm and lmer for closer inspection.
## intercepts on M separate lm fits:
m3b0  <- sapply(m3list, function(m) coef(m)["(Intercept)"])
sd(m3b0)

### The intercept "predictions" (blups) from mm4
mm4b0 <- mm4b["(Intercept)"] + mm4ranef[[1]][, "(Intercept)"]
sd(mm4b0)

###The slope estimates from M separate lm fits:
m3x1b  <- sapply(m3list, function(m) coef(m)["x1"])
sd(m3x1b)

### The slope "predictions" (blups) from mm4
mm4x1b <- mm4b["x1"] + mm4ranef[[1]][, "x1"]
sd(mm4x1b)


## Are the mm4 b's better than lm's? Closer to expected values? Shrunken toward center?
op <- par(no.readonly = TRUE)
par(mar=c(5.1, 5.1, 4.1, 2.1))
plot(m3x1b, mm4x1b, xlab = expression(hat(b)[1] ~~from~~lm), ylab = expression(hat(b)[1] ~~from~~lmer~~mixed~~model),  main=expression(paste("Shrinkage: Scatter of ",hat(b)[1]," from lm and lmer" )))
mx1b <- lm( mm4x1b ~ m3x1b)
abline(mx1b)
summary(mx1b)
options(digits=2)
legend("bottomright", as.expression(c(bquote(widehat(hat(b)[1*j]^{lmer}) == .(coef(mx1b)[1]) + .(coef(mx1b)[2])*hat(b)[1*j]))), lty=1, lwd=1)
options(digits=6)
par(op)


## Let's compare histograms of the slope estimates
par(mfcol=c(1,2))
par(mar=c(5.1, 4.1, 6.1, 2.1), xpd=T)

## Uses rockchalk: b1breaks <- plotSeq(range(c(m3x1b, mm4x1b)), 20)
b1breaks <- seq(range(c(m3x1b, mm4x1b))[1], range(c(m3x1b, mm4x1b))[2], length.out = 20)
m3x1bhist <- hist(m3x1b, breaks=b1breaks, plot = FALSE)
m4x1bhist <- hist(mm4x1b, breaks =b1breaks, plot = FALSE)
## Uses rockchalk: ylim <- magRange(c(0, m3x1bhist$density, m3x1bhist$density),  c(1, 1.2))

ylim <- range(c(0, m3x1bhist$density, m3x1bhist$density))*c(1, 1.2)
hist(m3x1b, breaks=b1breaks, prob = TRUE, ylim=ylim, xlab=expression(paste("100 lm estimates ", hat(b)[1])), main=paste("Separate OLS (lm)"))
legend("topright", legend = as.expression(c( bquote(mean(hat(b)[1])==.(round(mean(m3x1b),2))), bquote(sd(hat(b)[1])==.(round(sd(m3x1b),2))))))

hist(mm4x1b,  breaks=b1breaks, prob = TRUE, ylim=ylim, xlab=expression(paste("lmer 'blup' estimates ", hat(b)[1])), main="Mixed Model (lmer)")
legend("topleft", legend = as.expression(c( bquote(mean(hat(b)[1])==.(round(mean(mm4x1b),2))), bquote(sd(hat(b)[1])==.(round(sd(mm4x1b),2))))))
par(op)


## Plot the estimated slopes against the "true" random effects from reffects
plot(beta[2] + reffects[ ,2], m3x1b, xlab="Cluster Slopes (True Values)", ylab="Estimates of Slopes (across Clusters)", col="gray70", xlim= c(1.04,1.04)*range(c(beta[2] + reffects[ ,2], m3x1b)), ylim=c(1.04,1.04)*range((c(beta[2] + reffects[ ,2], m3x1b))))
points(beta[2] + reffects[ ,2], mm4x1b, , col="black", pch=13)
legend("topleft", c("OLS Slopes", "Mixed Model Slopes"), pch=c(1,13), col=c("gray70","black"))



## Plot the estimated intercept effects against the "true" random effects from reffects
plot(beta[1] + reffects[ ,1], m3b0, xlab="Cluster Intercepts (True Values)", ylab="Estimates of Intercepts", col="gray70")
points(beta[1] + reffects[ ,1], mm4b0 , col="black", pch=13)
legend("topleft", c("OLS Intercepts", "Mixed Model Intercepts"), pch=c(1,13), col=c("gray70","black"))


plot(reffects[, 1], m3b0)

plot(reffects[, 1], mm4b0)



lme4:::dotplot.ranef.mer

