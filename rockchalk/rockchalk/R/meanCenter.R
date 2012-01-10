##' Estimate standardized regression coefficients for all variables
##'
##' This is brain-dead standardization of all variables in the design matrix.
##' It mimics the silly output of SPSS, which standardizes all regressors,
##' even if they represent categorical variables.
##'  
##' @param model a fitted lm object 
##' @return an lm fitted with the standardized variables
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
standardize <- function(model){
  UseMethod("standardize")
}

##' @return a standardized regression object
##' @rdname standardize
##' @export
##' @method standardize lm
##' @S3method standardize lm
##' @example inst/examples/standardize-ex.R
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
  class(res) <- c("stdreg", class(model)) 
}



##' @author <pauljohn@@ku.edu>
##' @S3method summary stdreg
summary.stdreg <- function(object, ...){
  cat("All variables in the model matrix and the dependent variable were centered.\n")
  cat("Even though the variables here have the same names \n
  as their non-centered counterparts, I assure you these are \n
  centered, even variables like `x1:x2` and poly(x1,2)1. \n
  Here are the summary statistics of the variables in the design matrix. \n")
  dm <- model.matrix(object)
  dm <- dm[ , which(attr(dm, "assign") != 0)] #remove intercept, if any
  dm <- cbind( model.frame(object)[ , deparse(terms(object)[[2]])], dm)
  colnames(dm)[1] <- deparse(terms(object)[[2]])
  dmmeans <- apply(dm, 2, mean)
  dmstds <- apply(dm, 2, sd)
  summstat <- zapsmall(data.frame("mean"=dmmeans, "std.dev."=dmstds))
  print(summstat)
  NextMethod(generic = "summary", object = object, ...)
}
NULL

##' @author <pauljohn@@ku.edu>
##' @S3method print stdreg
print.stdreg <- function(x, ...){
  cat("Even though the variables here have the same names \n
       as their non-centered counterparts, I assure you these \n
       are centered.  You can run summary() to make sure. \n")
  NextMethod(generic = "print", object = x, ...)
}
NULL






##' meanCenter selectively centers or standarizes variables in a regression model.
##'
##' The defaults will cause a regression's numeric interactive
##' variables to be mean centered. If one wants all predictors to be
##' centered, the option centerOnlyInteractors should be set to FALSE. The dependent
##' variable will not be centered, unless the user explicitly requests it by
##' setting centerDV = TRUE.
##' @title meanCenter 
##' @param model a fitted regression model (presumably from lm) 
##' @param centerOnlyInteractors If false, all predictors in the
##' regression data frame are centered before the regression is
##' conducted.
##' @param centerDV Should the dependent variable be centered? 
##' @param standardize Instead of simply mean-centering the variables, should they also be "standardized" by first mean-centering and then dividing by the estimated standard deviation.
##' @param centerContrasts This function was originally intended only to center numeric variables. However, this option will ask centering of the numeric contrasts that are created in the fitting process. 
##' @export meanCenter
##' @rdname meanCenter
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @seealso \code{\link[pequod]{lmres}}
##' @example inst/examples/meanCenter-ex.R
meanCenter <- function(model, centerOnlyInteractors=TRUE, centerDV=FALSE, standardize=FALSE, centerContrasts = F){
  UseMethod("meanCenter")
}

##' @return A regression model of the same type as the input model,
##' with attributes representing the names of the centered variables.
##' @rdname meanCenter
##' @export
##' @method meanCenter default
##' @S3method meanCenter default
meanCenter.default <- function(model, centerOnlyInteractors=TRUE, centerDV=FALSE, standardize=FALSE, centerContrasts = F){

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
  if (!centerDV) {
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


      fmla <- formula(paste(dmnames[1], " ~ ",  paste(dmnames[-1], collapse=" + ")))
      cat("Model-constructed interactions such as \"x1:x3\" are built from centered variables\n")
      mc$formula <- formula(fmla)
      mc$data <-  quote(dm)
    }
  
  cat("These variables", nc, "Are centered in the design matrix \n")
  
  res <- eval(mc)
  class(res) <- c("mcreg", class(model))
  attr(res, "centeredVars") <- nc
  attr(res, "centerCall") <-  match.call()
  res
}


##' @author <pauljohn@@ku.edu>
##' @S3method summary mcreg
summary.mcreg <- function(object, ...){
  nc <- attr(object, "centeredVars")
  cat("The centered variables were: \n")
  print(nc)
  cat("Even though the variables here have the same names as their \n
  non-centered counterparts, I assure you these are centered.
  Here are the summary statistics of the variables in the design matrix. \n")
  dm <- model.matrix(object)
  dm <- dm[ , which(attr(dm, "assign") != 0)] #remove intercept, if any
  dm <- cbind( model.frame(object)[ , deparse(terms(object)[[2]])], dm)
  colnames(dm)[1] <- deparse(terms(object)[[2]])
  dmmeans <- apply(dm, 2, mean)
  dmstds <- apply(dm, 2, sd)
  summstat <- zapsmall(data.frame("mean"=dmmeans, "std.dev."=dmstds))
  print(summstat)
  mc <- attr(object, "centerCall")
  cat("These results were produced from: \n")
  print(mc)
  NextMethod(generic = "summary", object = object, ...)
}


##' @author <pauljohn@@ku.edu>
##' @S3method print mcreg
print.mcreg <- function(x, ...){
  nc <- attr(x, "centeredVars")
  cat("The centered variables were: \n")
  print(nc)
  cat("Even though the variables here have the same names \n
as their non-centered counterparts, I assure you these are centered.
  You can run summary() to make sure. \n")
  mc <- attr(x, "centerCall")
  cat("The call that requested centering was: \n")
  print(mc)
  NextMethod(generic = "print", object = x, ...)
}
NULL
