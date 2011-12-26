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
##' @return a list containing
##'  rcReg = the residual centered regression that is
##' requested
##'  centeringRegressions = a list including each of the
##' intermediate regressions that was calculated in order to create
##' the residual centered interaction terms. These latter objects
##' may be necessary for diagnostics and to calculate predicted
##' values for hypothetical values of the inputs. If there are
##' no interactive terms, then NULL is returned.
##'  newdat = the original dataset augmented with columns
##'  representing the residual centered interaction terms
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
##' m1 <- lm(y~ x1*x2 + x4, data=dat)
##'  
##' lmRCm1 <- lmrc(m1)
##' ## Here is final fitted regression: grabs the first object
##' (lmRCm1s <- summary(lmRCm1[["rcReg"]]))
##' ## The stage 1 centering regressions can be viewed as well
##' ## lapply(lmRCm1[[2]], summary)
##' 
##' ##Verify that result manually
##' dat$x1rcx2 <- resid(lm(I(x1*x2) ~ x1 + x2, data=dat))
##' m1m <- lm(y ~ x1 + x2 + x4 + x1rcx2, data=dat)
##' summary(m1m)
##' cbind(coef(lmRCm1[["rcReg"]]), coef(m1m))
##'
##' m2 <- lm(y~ x1*x2*x3 + x4, data=dat)
##' lmRCm2 <- lmrc(m2)
##' lmRCm2s <- summary(lmRCm2[["rcReg"]])
##'
##' ##Verify that result manually
##' dat$x2rcx3 <- resid(lm(I(x2*x3) ~ x2 + x3, data=dat))
##' dat$x1rcx3 <- resid(lm(I(x1*x3) ~ x1 + x3, data=dat))
##' dat$x1rcx2rcx3 <- resid(lm(I(x1*x2*x3) ~ x1 + x2 + x3 + x1rcx2 + x1rcx3 + x2rcx3 , data=dat))
##' (m2m <- lm(y ~ x1 + x2 + x3+ x4 + x1rcx2 + x1rcx3 + x2rcx3 + x1rcx2rcx3, data=dat))
##' 
##' cbind(coef(lmRCm2[["rcReg"]]), coef(m2m))
##'
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
##' m3 <- lm(y~ x1*x2*x3*x4, data=dat)
##' lmRCm3 <- lmrc(m3)
##' summary(lmRCm3[["rcReg"]])
##'
##' ## Verify that one manually (Gosh, this is horrible to write out)
##' dat$x1rcx4 <- resid(lm(I(x1*x4) ~ x1 + x4, data=dat))
##' dat$x2rcx4 <- resid(lm(I(x2*x4) ~ x2 + x4, data=dat))
##' dat$x3rcx4 <- resid(lm(I(x3*x4) ~ x3 + x4, data=dat))
##' dat$x1rcx2rcx4 <- resid(lm(I(x1*x2*x4) ~ x1 + x2 + x4 + x1rcx2 + x1rcx4 + x2rcx4, data=dat))
##' dat$x1rcx3rcx4 <- resid(lm(I(x1*x3*x4) ~ x1 + x3 + x4 + x1rcx3 + x1rcx4 + x3rcx4, data=dat))
##' dat$x2rcx3rcx4 <- resid(lm(I(x2*x3*x4) ~ x2 + x3 + x4 + x2rcx3 + x2rcx4 + x3rcx4, data=dat))
##' dat$x1rcx2rcx3rcx4 <- resid(lm(I(x1*x2*x3*x4) ~ x1 + x2 + x3 + x4 + x1rcx2 + x1rcx3 + x2rcx3 + x1rcx4  + x2rcx4 + x3rcx4  + x1rcx2rcx3 + x1rcx2rcx4 + x1rcx3rcx4 + x2rcx3rcx4, data=dat))
##' (m3m <- lm(y ~ x1 + x2 + x3 + x4 + x1rcx2 + x1rcx3 + x2rcx3 + x1rcx4 + x2rcx4 + x3rcx4 + x1rcx2rcx3 + x1rcx2rcx4 + x1rcx3rcx4 + x2rcx3rcx4 + x1rcx2rcx3rcx4, data=dat))
##' cbind(coef(lmRCm3[["rcReg"]]),  coef(m3m))
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
##' summary(lmRCf1[["rcReg"]])
##' 
##' lmf2 <- lm(paste("y","~", paste(f2, collapse=" + ")), data=dat)
##' lmRCf2 <- lmrc(lmf2)
##' summary(lmRCf2[["rcReg"]])
##' 
##' lmf3 <- lm(paste("y","~", paste(f3, collapse=" + ")), data=dat)
##' lmRCf3 <- lmrc(lmf3)
##' summary(lmRCf3[["rcReg"]])
lmrc <-
  function (model) 
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
      dat[ , rcName] <- resid(aReg)
    }   
  }
  rcvec <- gsub(":", ".X.", tmvec)
  regFormula <- paste(colnames(model$model[1]), " ~ ", paste(rcvec, 
                                                             collapse = " + "))
  rcReg <- lm(regFormula, data = dat)
  list(rcReg=rcReg, rcRegressions=rcRegressions, newdat= dat)
}
