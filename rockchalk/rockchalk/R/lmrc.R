##' Calculates a "residual-centered" interaction regression.
##'
##' Given a fitted lm, this function scans for coefficients
##' estimated from "interaction terms" by checking for colon
##' symbols. The function then calculates the "residual centered"
##' estimate of the interaction term and replaces the interaction
##' term with that residual centered estimate. It works for any
##' order of interaction, unlike other implementations of the same
##' approach. See also function lmres in package pequod.
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
  
