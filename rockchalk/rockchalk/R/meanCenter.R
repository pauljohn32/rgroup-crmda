
## Standardize the dependent variable and all predictors
## in the design frame. Does standardize dummy contrasts
## that represent factor variables
standardize.lm <- function(model){
  mt <- terms(model)
  mdata <- model.frame(model)
  y  <- mdata[, 1]
  #dm = design matrix, columns of predictors as numerically coded
  dm <- model.matrix(model)[ , -1] #no intercept
  dmnames <- colnames(dm)
  dmnamesticked <- paste("`",dmnames,"`", sep="")
  dmnamesticked <- gsub("``","`", dmnamesticked)
  dvname <- colnames(mdata)[1]
  dvnameticked <-  paste("`", dvname,"`", sep="")
  dvnameticked <- gsub("``","`", dvnameticked)
  std <- function(x) if(is.numeric(x)) scale(x) else x
  stddat <- apply(dm, 2, std)  ##standardize numeric vars
  stddat <- cbind( scale(y), stddat )
  stddat <- as.data.frame(stddat)
  colnames(stddat) <- c(dvname, dmnames)
  colnames(stddat) <- gsub("`","", colnames(stddat))
  mc <- model$call
  mc$data <- quote(stddat)
  fmla <- paste(dvnameticked, " ~ ", paste(dmnamesticked, collapse= " + "))
  mc$formula <- formula(fmla)
  res <- eval(mc)
}


##Needs an alias to standardize.lm
meanCenter.lm <- function(model, centerOnlyInteractors=TRUE, centerOnlyPredictors=TRUE, standardize=FALSE, centerContrasts = F){

  std <- function(x) {
    if( !is.numeric(x) ){
      stop("center.lm tried to center a factor variable. No Can Do!")
    } else {
      scale(x, center = TRUE, scale = standardize)
    }
  }
  
  rdf <- get_all_vars(formula(model), model$model) #raw data frame
  t <- terms(model)
  tl <- attr(t, "term.labels")
  tmdc <- attr(t, "dataClasses") ##term model data classes

  isNumeric <- names(tmdc)[ which(tmdc %in% c("numeric"))]
  isFac <-  names(tmdc)[ which(tmdc %in% c("factor"))]
  if (tmdc[1] != "numeric") stop("Sorry, DV not a single numeric column")

  ##Build "nc", a vector of variable names that "need centering"
  ##
  if (centerOnlyPredictors) {
    if (centerOnlyInteractors == FALSE){
      nc <- isNumeric[-1] #-1 excludes response
      unique(nc)
    }else{
      interactTerms <- tl[grep(":", tl)]
      nc <- unique(unlist(strsplit( interactTerms, ":")))
      nc <-  nc[which(nc %in% isNumeric)]
    }
  }else{
    if (centerOnlyInteractors == FALSE){
      nc <- isNumeric
    }else{
      interactTerms <- tl[grep(":", tl)]
      nc <- unique(unlist(strsplit( interactTerms, ":")))
      nc <- nc[which(nc %in% isNumeric)]
      nc <- c( names(tmdc)[1] , nc)
    }
  }


  mc <- model$call
  # run same model call, replacing non centered data with centered data.  
  ## if no need to center factor contrasts:
  if (!centerContrasts)
    {
      stddat <- rdf
      for (i in nc) stddat[ , i] <- std( stddat[, i])
      mc$data <- quote(stddat)
      res <- eval(mc)
      list(res, centeredVars = nc, call= match.call())
    }else{
      ##dm: design matrix, only includes intercept and predictors
      dm <- model.matrix(model, data=rdf, contrasts.arg = model$contrasts, xlev = model$xlevels)
      ##contrastIdx: indexes of contrast variables in dm
      contrastIdx <- which(attr(dm, "assign")== match(isFac, tl))
      contrastVars <- colnames(dm)[contrastIdx]
      nc <- c(nc, contrastVars)

      dm <- as.data.frame(dm)

      hasIntercept <- attr(t, "intercept")
      if (hasIntercept) dm <- dm[ , -1] # removes intercept, column 1


      
      dv <- rdf[ ,names(tmdc)[1]] #tmdc[1] is response variable name
      dm <- cbind(dv, dm)
      colnames(dm)[1] <- names(tmdc)[1] #put colname for dv

      dmnames <- colnames(dm)
      hasColon <- dmnames[grep(":", dmnames)]
      dm <- dm[ , -match(hasColon, dmnames)] ##remove vars with colons (lm will recreate)

      ##Now, standardise the variables that need standardizing
      for (i in nc) dm[ , i] <- std( dm[, i])

      cat("These variables", nc, "Are standardized in the design matrix \n")
      
   
#      dmnamesticked <- paste("`",dmnames,"`", sep="") #add tick
#      dmnamesticked <- gsub("``","`", dmnamesticked) #rm double tick
      
      fmla <- formula(paste(dmnames[1], " ~ ",  paste(dmnames[-1], collapse=" + ")))
      cat("This fitted model will use those centered variables\n")
      cat("Model-constructed interactions such as "x1:x3" are built from centered variables\n")
      mc$formula <- formula(fmla)
      mc$data <-  quote(dm)
      res <- eval(mc)
      list(res, centeredVars = nc, call= match.call())
    }
}



library(rockchalk)
N <- 100
dat <- genCorrelatedData(N=N, means=c(100,200), sds=c(20,30), rho=0.4, stde=10)
dat$x3 <- rnorm(100, m=40, s=4)
m1 <- lm(y ~ x1 + x2 + x3, data=dat)
summary(m1)
m1d <- mcDiagnose(m1)
m1s <- standardize.lm(m1)
summary(m1s)
m1sd <- mcDiagnose(m1s)

m1 <- lm(y ~ x1 * x2 + x3, data=dat)
summary(m1)
m1d <- mcDiagnose(m1)
m1s <- standardize.lm(m1)
summary(m1s)
m1sd <- mcDiagnose(m1s)



m2 <- lm(y ~ log(10+x1) + x3 + poly(x2,2), data=dat)
summary(m2)
m2d <- mcDiagnose(m2)
m2s <- standardize.lm(m2)
summary(m2s)
mcDiagnose(m2s)
standardize.lm(m2d[[1]])

lapply(m2d, standardize.lm)



N <- 100
x1 <- 50 + rnorm(N)
x2 <- log(rgamma(N, 2,1))
x3 <- rpois(N, lambda=17)
z1 <- gl(5, N/5)
dummies <- contrasts(z1)[ as.numeric(z1), ]
dimnames(dummies) <- NULL ## Avoids row name conflict in data.frame below
y3 <- x1  -.5 * x2 + 0.1 * x2^2 + dummies %*% c(0.1,-0.1,-0.2,0.2)+ 5 * rnorm(N)
dat <- data.frame(x1=x1, x2=x2, x3=x3,  z1=z1, y3 = y3)

m3 <- lm(y3 ~ x1 + poly(x2,2)  + log(x1) + z1, dat)
summary(m3)

m3 <- lm(y3 ~ x1 + x1*x3 + x2 + z1*x2, dat)



m3s <- standardize.lm(m3)
summary(m3s)

m3mc <- mcDiagnose(m3)

lapply(m3mc, standardize.lm)


lapply( lapply(m3mc, standardize.lm), summary)



m4 <- lm (y3 ~ x1 + x2 + z1 + x3, data=dat)
m4c <- center.lm(m4, centerContrasts=F)
m4c

m4c <- center.lm(m4, centerContrasts=T)
m4c


m4c <- center.lm(m4, centerOnlyInteractors=F, centerContrasts=T)
m4c




m4 <- lm (y3 ~ x1*x2 + x2*z1 + x3, data=dat)

m4c <- center.lm(m4, standardize=F, centerOnlyPredictors=F, centerContrasts=F)
m4c

m4c <- center.lm(m4, standardize=F, centerOnlyPredictors=F, centerContrasts=T)
m4c

m4c <- center.lm(m4, standardize=F, centerOnlyPredictors=F, centerOnlyInteractors=F, centerContrasts=F)
m4c

m4c <- center.lm(m4, standardize=F, centerOnlyPredictors=F, centerOnlyInteractors=F, centerContrasts=T)
m4c



summary(m4c[[1]])
print(m4c[[2]])

m4c <- center.lm(m4, standardize=F, centerOnlyInteractors =F, centerOnlyPredictors=F)
summary(m4c[[1]])
print(m4c[[2]])

m4c <- center.lm(m4, standardize=F, centerOnlyInteractors =T, centerOnlyPredictors=F)
summary(m4c[[1]])
print(m4c[[2]])

