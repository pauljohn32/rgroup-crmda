##Paul Johnson
## 2012-04-28

## This is a proposed new & improved back end for regression users who
## want to calculate predicted values for selected values of inputs in
## regression. The end user is most likely to use the main function
## "predictOMatic", but the support functions newdata and model.data
## may be helpful in some applications.  Similar in spirit to the
## packages Effects, Zelig, rms, and others that attempt to do the
## same. This is distinguished mainly because it is more flexible for
## end users and works with a broader array of model formulas. Does
## (or will) work with troublesome formulas like log(10+x) + sin(x) +
## poly(x,2).

## I believe "model.data" may be suitable for widespread use in R
## packages like termplot that attempt to re-construct input data
## sets from fitted models.



##' Creates the newdata frame required in predict.
##'
##' If not supplied with a focus list, newdata returns a data frame
##' with one row-- the central values (means and modes) of the
##' variables in the data that was used to fit the model.  To declare
##' some variables that the user wants to focus on, the user should
##' supply a fitted model "model" and a focus list "fl" of variable
##' values. The fl list must be a named list, using names of variables
##' from the regression formula.  It is not needed to call this
##' directly if one is satisfied with the results from predictOMatic.
##' @param model Fitted regression model
##' @param fl  "focus list".  optional. names list of variables and values
##' for which to create a new data object.
##' @param emf optional. data frame used to fit model (not a model
##' frame. Instead, use output from function \code{model.data}). It is
##' UNTRANSFORMED variables ("x" as opposed to poly(x,2).1 and
##' poly(x,2).2).
##' @return A data frame
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @export
##' @seealso \code{predictOMatic}
##' @example inst/examples/predictOMatic-ex.R
newdata <- function (model = NULL, fl = NULL, emf = NULL){
    if (is.null(emf)) emf <- model.data(model = model)
    ivnames <- attr(emf, "ivnames")
    emf <- emf[ , ivnames]
    modelcv <- centralValues(emf)
    if (is.null(fl)) return(modelcv)
    if (sum(!names(fl) %in% ivnames) > 0) stop(cat(c("Error. The focus list:  fl requests variables that are not included in the original model. The names of the variables in the focus list be drawn from this list: ",  ivnames, "\n")))
    ## TODO: Consider "padding" range of fl for numeric variables so that we
    ## get newdata objects including the min and max values.

    mixAndMatch <- expand.grid(fl)
    ## TODO: Its OK to select columns this way, but no better way add names?
    unames <- colnames(modelcv)[!colnames(modelcv) %in% colnames(mixAndMatch)]
    newdf <- cbind(mixAndMatch, modelcv[  , unames])
    colnames(newdf) <- c(colnames(mixAndMatch), unames)
    newdf
}


##' Creates a "raw" (UNTRANSFORMED) data frame equivalent
##' to the input data that would be required to fit the given model.
##'
##' Unlike model.frame and model.matrix, this does not return transformed
##' variables.
##'
##' @param model A fitted regression model in which the data argument
##' is specified. This function will fail if the model was not fit
##' with the data option.
##' @return A data frame
##' @export
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @example inst/examples/model.data-ex.R
model.data <- function(model){
    fmla <- formula(model)
    allnames <- all.vars(fmla) ## all variable names
    ## indep variables, includes d in poly(x,d)
    ivnames <- all.vars(formula(delete.response(terms(model))))
    ## dat: original data frame
    datOrig <-  eval(model$call$data, environment(formula(model)))
    if (is.null(datOrig))stop("model.data: input model has no data frame")
    ## dat: almost right, but includes d in poly(x, d)
    dat <- get_all_vars(fmla, datOrig)
    ## Get rid of "d" and other "non variable" variable names that are not in datOrig:
    keepnames <- intersect(names(dat), names(datOrig))
    ## Keep only rows actually used in model fit, and the correct columns
    dat <- dat[ row.names(model$model) , keepnames]
    ## keep ivnames that exist in datOrig
    attr(dat, "ivnames") <- intersect(ivnames, names(datOrig))
    invisible(dat)
}



##' predictOMatic creates predicted values for a fitted regression model.
##'
##' If a "focus list" is supplied, predictOMatic supplies predicted
##' values only for those selected input values.
##
##' If no "focus list" is supplied, predictOMatic supplies a
##' prediction summary for each separate independent variable. That
##' is, in a model with formula y ~ x1 + x2 + x3, then separate tables of predicted values will be supplied, one for each of x1, x2, and x3.
##'
##' It may be important to make sure that diagnostic plots and
##' summaries of predictions are calculated with the exact same data
##' that was used to fit the model. The function \code{model.data} is
##' intended to facilitate that comparison. One can fit a model, use
##' model.data to get the data that was used, and then use that
##' extracted data to decide on values to be set in the focus list.
##'
##' For example, create a copy of the data from a model m1 with
##'
##' m1dat <- model.data(m1)
##'
##' and then use m1dat to select values that might be predicted in
##' a command like
##'
##' predictOMatic( m1, fl =
##' list("x1" = median(m1dat$x1), "x2"=c(1,2,3), "x3" = quantile(m1dat$x3))
##'
##' @param model A fitted regression model
##' @param fl (focus list). Optional. A named list of variables and
##' values, such as fl = list("x1" = c(1,2,3), "x2" = c("M","F"), "x3"
##' = quantile(dat$x3)). Must name only predictors that are fitted in
##' \code{model}.  Need not include all predictors in a model.
##' Predictor variables in \code{model} that are not named in fl will
##' be set to mean or mode values. See details and examples.
##' @param divider If fl is not specified, automatic selection of
##' predictor values is employed. \code{divider} determines the method of
##' selection. Should be one of c("quantile","std.dev","table"). This
##' determines whether values selected are quantile values, values
##' based on the mean plus-or-minus standard deviation values, or a
##' table of most frequently occurring values. Documentation for the
##' details can be found in the functions \code{cutByTable},
##' \code{cutByQuantile}, and \code{cutBySD}.
##' @param n If fl is not specified, automatic selection of predictor
##' values is employed. This determines the number of values for which
##' predictions are sought.
##' @param ... Optional arguments to be passed to the predict function
##' @return A data frame or a list of data frames.
##' @export
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @example inst/examples/predictOMatic-ex.R
predictOMatic <- function(model = NULL, fl = NULL, divider = "quantile", n = 3,  ...) {
    dots <- list(...)
    dotnames <- names(dots)
    ## next should give c('digits', 'alphaSort')
    nnames <- names(formals(rockchalk::summarizeNumerics))[-1L]
    ## names that need keeping if in dots:
    keepnames <- dotnames %in% nnames

    emf <- model.data(model = model)
    ivnames <- attr(emf, "ivnames")

    if(missing(fl) || is.null(fl)){
        flxxx <- list()
        nd <- lapply (ivnames, function(x) {
            if (is.numeric(emf[ ,x])) {
                divider <- match.arg(tolower(divider),
                                  c("quantile", "std.dev.","table"))
                flxxx[[x]] <- switch(divider,
                         table = rockchalk:::cutByTable(emf[,x], n),
                         quantile = rockchalk:::cutByQuantile(emf[,x], n),
                         "std.dev." = rockchalk:::cutBySD(emf[,x], n),
                         stop("unknown 'divider' algorithm"))
            } else {
                flxxx[[x]] <- names(rockchalk:::cutByTable(emf[ ,x], n))
            }
            ndnew <- newdata(model, fl=flxxx[x], emf = emf)
            fit <- predict(model, newdata = ndnew, ...)
            ndnew <- cbind(fit, ndnew)
            attr(ndnew, "flnames") <- x
            row.names(ndnew) <- names(flxxx[[x]])
            ndnew
        } )
       names(nd) <- ivnames
    }else{
        flnames <- names(fl)
        nd <- newdata(model, fl, emf = emf)
        fit <- predict(model, newdata = nd, ...)
        nd <- cbind(fit, nd)
        attr(nd, "flnames") <- flnames
    }
    invisible(nd)
}



## ## Other approaches I've wrestled with for model.data

## ## model.data.1: how its done in termplot, using carrier functions
## ## I used this in "plotCurves" of rockchalk
## ## Problem: cant handle log(10+x), can handle log(x+10)
## ## termplot has same trouble, observe:
## ## x <- rpois(100,l=6)
## ## y <- rpois(100, l=6)
## ## m1 <- lm(log(y) ~ log(10+x))
## ## termplot(m1)

## model.data.1 <- function(model = NULL) {
##     carrier <- function(term, data, enc = NULL) {
##         if (length(term) > 1L)
##             carrier(term[[2L]])
##         else eval(term, envir = data, enclos = enc)
##     }
##     carrier.name <- function(term) {
##         if (length(term) > 1L)
##             carrier.name(term[[2L]])
##         else as.character(term)
##     }
##     mt <- terms(model)
##     mt <- delete.response(mt)
##     mf <- model.frame(model) ##[ ,-1] ## -1 gets rid of DV
##     cn <- parse(text = colnames(mf))
##     varnames <- unique(unlist(lapply(cn, carrier.name)))
##     print(varnames)
##     emf <- get_all_vars(mt, data = expand.model.frame(model, varnames, na.expand=TRUE))
##     emf <- emf[ , varnames]
##     attr(emf, "varnames") <- varnames
##     emf
## }


## ## Bill Dunlap suggests this approach, r-help 2012-04-22.
## ## mt is a terms object:
## ## unique(unlist(lapply(attr(mt, "variables")[-1], all.vars)))
## ## Solves log(10+x) problem
## ## EXCEPT, as Bill warns, it
## ## includes d in poly(x2,d). That includes "d" as a variable
## ## name.
## model.data.2 <- function(model = NULL) {
##     mt <- terms(model)
##     varnames <- unique(unlist(lapply(attr(mt, "variables")[-1], all.vars)))
##     print(varnames)
##     emf <- get_all_vars(mt, data = expand.model.frame(model, varnames, na.expand=TRUE))
##     emf <- emf[ , varnames1]
##     attr(emf, "varnames") <- varnames1
##     emf
## }



## library(rockchalk)

## x1 <- rpois(100, l=6)
## x2 <- rnorm(100, m=50, s=10)
## x3 <- rnorm(100)
## y <- rpois(100, l=10)

## x1[sample(100, 5)] <- NA
## y[sample(100, 5)] <- NA

## m0 <- lm(y ~ log(10+x1) + x2)
## m0.data <- model.data(m0) # should fail

## df <- data.frame(x1, x2, x3, y)
## rm(x1,x2,x3, y)


## m0 <- lm(y ~ log(10+x1) + x2, data=df)
## m0.data <- model.data(m0) # should work
## summarize(m0.data)


## m1 <- lm(y ~ log(10+x1) + sin(x2) +x3, data=df)
## m1.data <- model.data(m1)
## summarize(m1.data)
## attr(m1.data, "ivnames")


## (newdata(m1))
## (newdata(m1, fl=list(x1=c(6, 8, 10))))
## (newdata(m1, fl=list(x1=c(6, 8, 10), x3=c(-1,0,1))))
## (newdata(m1, fl=list(x1=c(6, 8, 10), x2=quantile(m1.data$x2), x3=c(-1,0,1))))

## (m1.p1 <- predictOMatic(m1))
## (m1.p1 <- predictOMatic(m1, divider="std.dev", n=5))
## (m1.p1 <- predictOMatic(m1))


## m1.p1 <- predictOMatic(m1, fl=list(x1=c(6, 8, 10), x2=median(m1.data$x2,na.rm=TRUE)))
## predictOMatic(m1, fl=list(x1=c(6, 8, 10), x2=quantile(df$x2)))

## predictOMatic(m1, interval="confidence")


## ## repeat, test poly. poly does not allow NA
## x <- rpois(100, l=6)
## y <- rpois(100, l=10)
## df <- data.frame(x,y)
## rm(x,y)

## m1 <- lm(y ~ poly(x, 2), data=df)
## m1.data <- model.data(m1)
## summarize(m1.data)
## attr(m1.data, "ivnames")


## d <- 2
## m2 <- lm(y ~ poly(x, d), data=df)
## m2.data <- model.data(m2)
## summarize(m2.data)
## attr(m2.data, "ivnames")
## predictOMatic(m2)



## m3 <- lm(y ~ log(10+x) + poly(x, d), data=df)
## m3.data <- model.data(m3)
## summarize(m3.data)
## attr(m3.data, "ivnames")


## m4 <- lm(log(y) ~ log(d+10+x) + poly(x, 2), data=df)
## m4.data <- model.data(m4)
## summarize(m4.data)
## attr(m4.data, "ivnames")

## m4 <- lm(y ~ x*x, data=df)
## m4.data <- model.data(m4)
## summarize(m4.data)
## attr(m4.data, "ivnames")

## m4 <- lm(y ~ x + I(x^2), data=df)
## m4.data <- model.data(m4)
## summarize(m4.data)
## attr(m4.data, "ivnames")




## set.seed(12345)
## STDE <- 2
## x1 <- rnorm(100)
## x2 <- rnorm(100)
## x3 <- rnorm(100)
## x4 <- rnorm(100, m=100)
## x5 <- rpois(100, 5)
## x6 <- rgamma(100, 2,1)
## xcat1 <- gl(2,50, labels=c("M","F"))
## xcat2 <- cut(rnorm(100), breaks=c(-Inf, 0, 0.4, 0.9, 1, Inf), labels=c("R", "M", "D", "P", "G"))
## dat <- data.frame(x1, x2, x3, x4, x5, x6, xcat1, xcat2)
## rm(x1, x2, x3, x4, x5, x6, xcat1, xcat2)
## xcat1n <- with(dat, contrasts(xcat1)[xcat1, ,drop=FALSE])
## xcat2n <- with(dat, contrasts(xcat2)[xcat2, ])

## y <- with(dat, 0.03 + 0.8*x1 + 0.1*x2 + 0.7*x3 -0.1*x4 + 0.01*x5 + 1.1*x6) + xcat1n %*% c(2) + xcat2n %*% c(0.1,-2,0.3, 0.1) + STDE*rnorm(100)
## rownames(y) <- row.names(dat)
## y2 <- ifelse(rnorm(100) > 0.3, 1, 0)

## dat <- cbind(dat, y, y2)

## m1 <- lm(y ~ x1 + x2, data=dat)
## model.data(m1)


## ## regression.
## d <- 2
## m1 <- lm(log(1000+y) ~ x1 + poly(x2,2) + poly(x3,d) + log(10+x4) + exp(x4) + x6 + xcat1 + xcat2, data=dat)
## summary(m1)


## ##  has only columns and rows used in model fit
## (m1.data <- model.data(m1))
## summarize(m1.data)

## ## First, overview for values of xcat1
## newdata(m1, fl = list(xcat1 = levels(m1.data$xcat1)))

## ## mix and match all combinations of xcat1 and xcat2
## newdata(m1, fl = list(xcat1 = levels(m1.data$xcat2), xcat2 = levels(m1.data$xcat2)))

## ## Pick some particular values for focus
## newdata(m1, fl = list(x1 = c(1,2,3), xcat2 = c("M","D")))

## ## Generate a newdata frame and predictions in one step
## predictOMatic(m1, fl = list(x2 = c(0.25, 1.0), xcat2 = c("M","D")))

## predictOMatic(m1, fl = list(x2 = plotSeq(m1.data$x2, 10) , xcat2 = c("M","D")))

## predictOMatic(m1, fl = list(x2 = c(0.25, 1.0), xcat2 = c("M","D")), interval="conf")

## predictOMatic(m1, interval="conf")

## newdf <- predictOMatic(m1, fl = list(x2 = c(0.25, 1.0), xcat2 = c("M","D"), x1=plotSeq(dat$x1)))

## plot(y ~ x1, data= datc)
## by(newdf, list(newdf$x2, newdf$xcat2), function(x) {lines(x$x1, x$fit)})

## newdata(m1, fl = list(x2 = c(-1,0, 1), xcat2 = c("M","D")))

## predictOMatic(m1, fl = list(x2 = range(dat$x2), xcat2 = c("M","D")))

## newdf <- predictOMatic(m1, fl = list(x2 = quantile(dat$x2), xcat2 = c("M","D")))
## plot(y ~ x2 , data=model.frame(m1))

## lines(y ~ x2,  newdf)


## predictOMatic(m1, fl = list(x2 = c(50, 60), xcat2 = c("M","D")), interval="conf")

## ## just gets the new data
## nd <- newdata(m1, fl = list(x2 = c(50, 60), xcat2 = c("M","D")))

## pr <- predictOMatic(m1, fl = list(x2 = c(50, 60), xcat2 = c("M","D")), interval="conf")





## ##
## m2 <- glm(y2 ~ x1 + x2 + x3 + xcat1, data=dat, family=binomial(logit))
## summary(m2)
## dat2c <- extractRawDataFrame(m2)
## summarize(dat2c)


## predictOMatic(m2, divider="response")

## predictOMatic(m2, fl = list(x2 = c(-1, 1), xcat1 = c("M","F")), interval="conf", divider="response")


## predictOMatic(m2, fl = list(x2 = unique(dat2c$x2), xcat1 = c("M","F")), interval="conf", divider="response")



## ## Now examples with real data
## library(car)

## m5 <- lm(statusquo ~ region * income + sex + age, data= Chile)
## summary(m5)
## plotSlopes(m5, modx = "region", plotx = "income")

## m6 <- lm(statusquo ~ income * age + education + sex + age, data=Chile)
## summary(m6)
## plotSlopes(m6, modx = "income", plotx = "age")

## plotSlopes(m6, modx = "income", plotx = "age", plotPoints=FALSE)


## Chile$educationn <- as.numeric(Chile$education)
## m9 <- lm(statusquo ~ income * age + educationn + sex + age, data=Chile)
## summary(m9)
## plotSlopes(m9, modx = "income", plotx = "educationn")





## ###Grabs original data from environment, only works if data frame
## ### still exists in the environment, and if there was a data frame.
## dat <- eval(model$call$data, environment(formula(model)))
## ## is NULL if data is NULL.



## m1.p1 <- predictOMatic(m1)

## for (i in names(m1.p1)){
##     dns <- cbind(m1.p1[[i]][i], m1.p1[[i]]$fit)
##     colnames(dns) <- c(i, "predicted")
##     print(dns)
## }


## m1.p1[ , c("fit", attr(m1.p1, "flnames")]

