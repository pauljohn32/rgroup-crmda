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
  res
}


##' @author <pauljohn@@ku.edu>
##' @method summary stdreg
##' @S3method summary stdreg
summary.stdreg <- function(object, ...){
    dm <- model.matrix(object)
    dm <- dm[ , which(attr(dm, "assign") != 0)] #remove intercept, if any
    dm <- cbind( model.frame(object)[ , deparse(terms(object)[[2]])], dm)
    colnames(dm)[1] <- deparse(terms(object)[[2]])
    dmmeans <- apply(dm, 2, mean)
    dmstds <- apply(dm, 2, sd)
    summstat <- zapsmall(data.frame("mean" = dmmeans, "std.dev." = dmstds))
    ##summ <- c(summary(object, ...), summstat)
    summ <- NextMethod(generic = "summary", object = object, ...)
    summ$summstat <- summstat
    class(summ) <- paste("summary.", class(object), sep="")
    summ
}
NULL


##' @author <pauljohn@@ku.edu>
##' @method print stdreg
##' @S3method print stdreg
print.stdreg <- function(x, ...){
  cat("Even though the variables here have the same names \n
       as their non-centered counterparts, I assure you these \n
       are centered.  You can run summary() to make sure. \n")
  NextMethod(generic = "print", object = x, ...)
}
NULL

##' @author <pauljohn@@ku.edu>
##' @method print summary.stdreg
##' @S3method print summary.stdreg
print.summary.stdreg <- function (x, ...){
  
    cat("All variables in the model matrix and the dependent variable
were centered. The variables here have the same names as their 
non-centered counterparts, but they are centered, even constructed 
variables like `x1:x2` and poly(x1,2). We agree, that's probably 
ill-advised, but you asked for it by running standardize().\n
Observe, the summary statistics of the variables in the design matrix. \n")
    print(x$summstat)
    ##NextMethod(generic = "print", x = x, ...)
    NextMethod()
}
NULL





##' meanCenter selectively centers or standarizes variables in a regression model.
##'
##' Mean-centering has often been recommended as a way to ameliorate
##' multi-collinearity in regression models that include interaction
##' terms (Aiken and West, 1991; Cohen, et al 2002). While this claim
##' may have been mistaken (Echambadi and Hess, 2007), mean-centering
##' is still widely practiced.  This function facilitates comparison
##' of mean-centered models with others by automatically
##' re-calculating centered variables.  The defaults will cause a
##' regression's numeric interactive variables to be mean
##' centered. That is to say, if an interaction x1:x2 is present in
##' the model, then x1 and x2 are replaced by (m1-mean(m1)) and
##' (m2-mean(m2) in all of the terms in which they appear in the model
##' (the main effect and the interaction).  If one wants all
##' predictors to be centered, the option \code{centerOnlyInteractors}
##' should be set to FALSE. The dependent variable will not be
##' centered, unless the user explicitly requests it by setting
##' centerDV = TRUE. The centered variables can be standardized
##' (optionally, of course). 
##' @title meanCenter 
##' @param model a fitted regression model (presumably from lm) 
##' @param centerOnlyInteractors If false, all predictors in the
##' regression data frame are centered before the regression is
##' conducted.
##' @param centerDV Should the dependent variable be centered? 
##' @param standardize Instead of simply mean-centering the variables, should they also be "standardized" by first mean-centering and then dividing by the estimated standard deviation.
##' @export meanCenter
##' @rdname meanCenter
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @seealso \code{\link[pequod]{lmres}}
##' @references 
##' Aiken, L. S. and West, S.G. (1991). Multiple Regression: Testing and Interpreting Interactions. Newbury Park, Calif: Sage Publications.
##'
##' Cohen, J., Cohen, P., West, S. G., and Aiken, L. S. (2002). Applied Multiple Regression/Correlation Analysis for the Behavioral Sciences (Third.). Routledge Academic.
##'
##' Echambadi, R., and Hess, J. D. (2007). Mean-Centering Does Not Alleviate Collinearity Problems in Moderated Multiple Regression Models. Marketing Science, 26(3), 438-445.
##' @example inst/examples/meanCenter-ex.R
meanCenter <- function(model, centerOnlyInteractors=TRUE, centerDV=FALSE, standardize=FALSE){
  UseMethod("meanCenter")
}

##' @return A regression model of the same type as the input model,
##' with attributes representing the names of the centered variables.
##' @rdname meanCenter
##' @export
##' @method meanCenter default
##' @S3method meanCenter default
meanCenter.default <- function(model, centerOnlyInteractors=TRUE, centerDV=FALSE, standardize=FALSE){

  std <- function(x) {
    if( !is.numeric(x) ){
      stop("meanCenter tried to center a factor variable. No Can Do!")
    } else {
      as.numeric(scale(x, center = TRUE, scale = standardize))
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
  ## 
  stddat <- rdf
  for (i in nc) stddat[ , i] <- std( stddat[, i])
  mc$data <- quote(stddat)
  res <- eval(mc)
  class(res) <- c("mcreg", class(model))
  attr(res, "centeredVars") <- nc
  attr(res, "centerCall") <-  match.call()
  res
}

##' @author <pauljohn@@ku.edu>
##' @S3method summary mcreg
##' @method summary mcreg
summary.mcreg <- function(object, ...){
  nc <- attr(object, "centeredVars")
  dm <- model.matrix(object)
  dm <- dm[ , which(attr(dm, "assign") != 0)] #remove intercept, if any
  dm <- cbind( model.frame(object)[ , deparse(terms(object)[[2]])], dm)
  colnames(dm)[1] <- deparse(terms(object)[[2]])
  dmmeans <- apply(dm, 2, mean)
  dmstds <- apply(dm, 2, sd)
  summstat <- zapsmall(data.frame("mean" = dmmeans, "std.dev." = dmstds))
  summ <- NextMethod(generic = "summary", object = object, ...)
  summ$summstat <- summstat
  summ$nc <- nc
  class(summ) <- paste("summary.", class(object), sep="")
  summ$mc <- attr(object, "centerCall")
  summ
}
NULL

##' @author <pauljohn@@ku.edu>
##' @method print mcreg
##' @S3method print mcreg
print.mcreg <- function(x, ...){
  nc <- attr(x, "centeredVars")
  cat("The centered variables are: \n")
  print(nc)
  mc <- attr(x, "centerCall")
  cat("The call that requested centering was: \n")
  print(mc)
  NextMethod(generic = "print", object = x, ...)
}
NULL


##' @author <pauljohn@@ku.edu>
##' @method print summary.mcreg
##' @S3method print summary.mcreg
print.summary.mcreg <- function (x, ...){
  cat("These variables were mean-centered before any transformations were made on the design matrix.\n")
  print(x$nc)

  cat("The summary statistics of the variables in the design matrix. \n")
  print(x$summstat)
  cat("\nThe following results were produced from: \n")
  print(x$mc)
  ##NextMethod(generic = "print", x = x, ...)
  NextMethod()
}
NULL


##' @author <pauljohn@@ku.edu>
##' @method predict mcreg
##' @S3method predict mcreg
predict.mcreg <- function (object, newdata, ...){

  originalCall <- object$call
  nc <- attr(object, "centeredVars")

  call <- attr(object, "centerCall")

  standardize <- ifelse(is.null(call$standardize), FALSE, call$standardize)

  std <- function(x) {
    if( !is.numeric(x) ){
      stop("meanCenter tried to center a factor variable. No Can Do!")
    } else {
      as.numeric(scale(x, center = TRUE, scale = standardize))
    }
  }

  if(missing(newdata)) newdata <- model.frame(object)
  newmf <- newdata
  for (i in nc) newmf[ , i] <- std( newmf[, i])  

  NextMethod(object, newdata=newmf, ...)
}
NULL
