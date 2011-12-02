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
##' @return An lm object in which the interaction terms have
##' been residual centered.
##' @export
##' @author Paul E. Johnson \email{<pauljohn@@ku.edu>}
##' @references  Little, T. D., Bovaird, J. A.,
##' & Widaman, K. F. (2006). On the Merits of Orthogonalizing
##' Powered and Product Terms: Implications for Modeling
##' Interactions Among Latent Variables.
##' Structural Equation Modeling, 13(4), 497-519.
##' @examples
##' set.seed(123)
##' x1 <- rnorm(100)
##' x2 <- rnorm(100)
##' x3 <- rnorm(100)
##' x4 <- rnorm(100)
##' y <- rnorm(100)
##' dat <- data.frame(y, x1,x2,x3,x4)
##' rm(x1,x2,x3,x4,y)
##' m1 <- lm(y~ x1*x2*x3 + x4, data=dat)
##' summary(m1)
##' 
##' lmRCm1 <- lmrc(m1)
##' lmRCm1s <- summary(lmRCm1)
##' coef(lmRCm1)
##' 
##' ### lmrc as good as pequod's lmres
##' ### not run because pequod generates R warnings
##' ###
##' ### if (require(pequod)){
##' ###  pequodm1 <- lmres(y ~ x1*x2*x3 + x4, data=dat) 
##' ###  pequodm1s <- summary(pequodm1)
##' ###  coef(pequodm1s)
##' ### }
##' 
##' ### lmrc works with any number of interactions. See:
##' 
##' m2 <- lm(y~ x1*x2*x3*x4, data=dat)
##' lmRCm2 <- lmrc(m2)
##' summary(lmRCm2)
##' 
##' 
##' ### If you want to fit a sequence of models, as in pequod, can do.
##' 
##' tm <-terms(m2)
##' tmvec <- attr(terms(m2), "term.labels")
##' f1 <- tmvec[grep(":", tmvec, invert = TRUE)]
##' f2 <- tmvec[grep(":.*:", tmvec, invert = TRUE)]
##' f3 <- tmvec[grep(":.*:.*:", tmvec, invert = TRUE)]
##' 
##' ## > f1
##' ## [1] "x1" "x2" "x3" "x4"
##' ## > f2
##' ## [1] "x1"    "x2"    "x3"    "x4"    "x1:x2" "x1:x3" "x2:x3"
##' ## > f3
##' ## [1] "x1"       "x2"       "x3"       "x4"       "x1:x2"    "x1:x3"    "x2:x3"   
##' ## [8] "x1:x2:x3"
##' 
##' lmf1 <- lm(paste("y","~", paste(f1, collapse=" + ")), data=dat)
##' lmRCf1 <- lmrc(lmf1)
##' summary(lmRCf1)
##' 
##' lmf2 <- lm(paste("y","~", paste(f2, collapse=" + ")), data=dat)
##' lmRCf2 <- lmrc(lmf2)
##' summary(lmRCf2)
##' 
##' lmf3 <- lm(paste("y","~", paste(f3, collapse=" + ")), data=dat)
##' lmRCf3 <- lmrc(lmf3)
##' summary(lmRCf3)
lmrc <-
  function (model) 
{
  makeRCformula <- function(x) {
    dv <- paste("I(", gsub(":", "*", x), ")", sep = "")
    iv <- paste( gsub(":", " + ", x) )
    myformula <- paste(dv, "~", iv)
    myformula
  }
  makeRCresiduals <- function(fm, dat) {
    residuals(lm(fm, data = dat))
  }
  dat <- model$model
  tmvec <- attr(terms(model), "term.labels")
  interactTerms <- tmvec[grep(":", tmvec)]
  if (length(interactTerms)) {
    interactFormulae <- makeRCformula(interactTerms)
    rcVariables <- sapply(interactFormulae, function(x) makeRCresiduals(x, dat))
    interactTerms <- gsub(":", ".X.", interactTerms)
    tmvec <- gsub(":", ".X.", tmvec)
    colnames(rcVariables) <- interactTerms
    newdat <- cbind(dat, rcVariables)
  }
  else {
    newdat <- dat
  }
  regFormula <- paste(colnames(model$model[1]), " ~ ", paste(tmvec, 
                                                             collapse = " + "))
  rcReg <- lm(regFormula, data = newdat)
  rcReg
}
