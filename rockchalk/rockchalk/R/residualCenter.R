##' Calculates a "residual-centered" interaction regression.
##'
##' Given a fitted \code{lm}, this function scans for coefficients
##' estimated from "interaction terms" by checking for colon
##' symbols. The function then calculates the "residual centered"
##' estimate of the interaction term and replaces the interaction
##' term with that residual centered estimate. It works for any
##' order of interaction, unlike other implementations of the same
##' approach. See also function \code{lmres} in package pequod.
##' @param model A fitted lm object
##' @export residualCenter
##' @rdname residualCenter
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @seealso \code{\link[pequod]{lmres}}
##' @references  Little, T. D., Bovaird, J. A.,
##' & Widaman, K. F. (2006). On the Merits of Orthogonalizing
##' Powered and Product Terms: Implications for Modeling
##' Interactions Among Latent Variables.
##' Structural Equation Modeling, 13(4), 497-519.
residualCenter <- function(model){
  UseMethod("residualCenter")
}

##' @rdname residualCenter
##' @export
##' @return a regression model of the type as the input model, with the exception that the residualCentered predictor is used in place of the original interaction. The return model includes new variable centeringRegressions: a list
##' including each of the intermediate regressions that was calculated
##' in order to create the residual centered interaction terms. These
##' latter objects may be necessary for diagnostics and to calculate
##' predicted values for hypothetical values of the inputs. If there
##' are no interactive terms, then NULL is returned.
##' @method residualCenter default
##' @S3method residualCenter default
##' @example inst/examples/residualCenter-ex.R
residualCenter.default <- function (model) 
{
  makeRCformula <- function(x) {
    nterms <- length(strsplit( x, ":")[[1]])
    dv <- paste("I(", gsub(":", "*", x), ")", sep = "")
    iv <- paste("(", gsub(":"," + ",x),")")
    if(nterms >= 3) iv <- paste(iv, "^", nterms-1, sep="")
    myformula <- paste(dv, "~", iv)
    myformula <- as.formula(myformula)
    rcterms <-  attr(terms(myformula), "term.labels")
    rcterms <- gsub(":", ".X.", rcterms)
    finalFormula <- paste(dv, "~", paste(rcterms, collapse = " + "))
  }
  
  dat <- model$model
  tmvec <- attr(terms(model), "term.labels")
  interactTerms <- tmvec[grep(":", tmvec)]
  rcRegressions <- NULL
  if (length(interactTerms)) {
    interactFormulae <- lapply(interactTerms, makeRCformula)
    interactRCNames <- gsub(":", ".X.", interactTerms)
    names(interactFormulae) <- interactRCNames
    rcRegressions <- list()
    ##recursively process name by name to build up interaction variables in dat
    for (rcName in interactRCNames){
      rcFmla <-  interactFormulae[[rcName]]
      aReg <- lm( rcFmla, dat)
      rcRegressions[[rcName]] <- aReg
      dat[ , rcName] <- as.numeric(resid(aReg))
    }   
  }
  rcvec <- gsub(":", ".X.", tmvec)
  fmla <- paste(colnames(model$model[1]), " ~ ",
                paste(rcvec, collapse = " + "))

  mc <- model$call
  mc$formula <- formula(fmla)
  mc$data <- quote(dat)
  res <- eval(mc)
  class(res) <- c("rcreg", class(model))
  res$rcRegressions <- rcRegressions
  res
}
NULL

##' predict method for rcreg objects
##'
##' Calculates predicted values of
##' residual centered interaction regressions estimated in
##' any type of regression framework (lm, glm, etc).
##' @method predict rcreg
##' @S3method predict rcreg
##' @rdname residualCenter
##' @example inst/examples/predict.rcreg-ex.R
##' @param object Fitted residual-centered regression from residualCenter
##' @param newdata A dataframe of values of the predictors for which predicted values are sought. Needs to include values for all predictors individually; need not include the interactions, since those are re-calculated inside this function.
##' @param ... Other parameters that will be passed to the predict method of the model. 
predict.rcreg <- function (object, newdata, ...){
  if ( ! c("rcreg") %in% class(object) ) stop("predict.rcreg is intended for rcreg objects, which are created by residualCenter in the rockchalk package") 
  objectTerms <- terms(object)
  dvname <- names(attr(objectTerms, "dataClasses"))[1]
  rcRegs <- object$rcRegressions
  nams <- names(rcRegs)
  ## remove user's residual centered variables, if any
  ## for(i in nams)  newdata[[i]] <- NULL
  for( i in seq_along(rcRegs) ){
     aReg <- rcRegs[[i]]
     prodVars <-  unlist(strsplit(nams[i], ".X."))
     predvals <-  predict.lm( aReg, newdata = newdata )
     actualProduct <- apply(newdata[ ,prodVars], 1, prod)
     myresids <-  actualProduct - predvals
     newdata[[nams[i]]] <- myresids
   }
  NextMethod(object, newdata=newdata, ...)
}
