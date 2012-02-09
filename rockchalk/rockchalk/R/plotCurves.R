##' Assists creation of predicted value curves for values of a
##' moderator variable.
##'
##' I think of this as "termplot for interactions."  It creates a plot
##' of the predicted dependent variable against one of the numeric
##' predictors, \code{plotx}, for each value of a numeric or
##' categorical moderator variable, \code{modx}. The moderator
##' variable may be a factor or a numeric.  Numeric moderators are, by
##' default, divided into sections on the basis of quartiles and a
##' line is plotted for the following percentiles {0,25,50,75,100}.
##' The user can change that by either specifying a vector of values
##' for which the lines are desired, or by specifying an alternative
##' algorithm.  That is, \code{modxVals = c( 1,2,3)} would draw lines
##' for values 1,2,and 3 of the moderator, while \code{modxVals
##' ="std.dev." will create 5 lines, corresponding to {mean-2*sd,
##' mean-sd, mean, mean+sd, mean+2*sd }.
##' 
##' @param model Fitted regression object. Must have a predict method
##' @param plotx String with name of IV to be plotted on x axis
##' @param modx String for moderator variable name. May be either numeric or factor.
##' @param modxVals A vector of numeric values for which plotted lines
##' are sought.  This is intended for numeric variables referred to by
##' "modx". These are user specified "for instance" values. If
##' omitted, quantiles will be used for numeric variables. If "modx"
##' is a factor, this parameter is ignored and a predictive line is
##' drawn for each level of the factor. That is a shortcoming in the
##' current implementation.
##' @param plotPoints Should the plot include the scatterplot points along with the lines.
##' @param envir environment to search for variables. 
##' @param ... further arguments that are passed to plot.
##' @export
##' @import car
##' @return When plotSlopes runs, and modx a numeric variable, a dataframe
##' will be created to summarize the hypothesis tests for the simple slopes.
##' The return value includes that dataframe, as well as a "newdf" object that includes information on the simple slopes that were plotted.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @example  inst/examples/plotCurves-ex.R

plotCurves <-
  function (model = NULL, plotx = NULL, modx = NULL, modxVals = NULL, 
            plotPoints = TRUE, envir = environment(formula(model)), ...) 
{
  if (is.null(model)) 
    stop("plotCurves requires a fitted regression model.")
  if (is.null(plotx)) 
    stop("plotCurves requires the name of the variable to be drawn on the x axis")
  if (is.null(modx)) 
    stop("plotCurves requires the name of moderator variable for which several slopes are to be drawn")

  carrier <- function(term, data, enc = NULL) {
    if (length(term) > 1L) 
      carrier(term[[2L]])
    else eval(term, envir = data, enclos = enc)
  }
  carrier.name <- function(term) {
    if (length(term) > 1L) 
      carrier.name(term[[2L]])
    else as.character(term)
  }
  cutBySD <- function(x){
    mx <- round(mean(x, na.rm=T),2)
    sdx <- round(sd(x, na.rm=T),2)
    qs <- c(mx - 2*sdx, mx - sdx, mx, mx + sdx, mx + 2*sdx)
    suffix <- c("(m-2sd)","(m-sd)","(m)","(m+sd)","(m+2sd)")
    names(qs) <-  paste(qs, suffix)
    invisible(qs)
  }

  
  cl <- match.call()
  mf <- model.frame(model)
  tt <- terms(model)
    
  cn <- parse(text = colnames(mf))
  varnames <- unlist(lapply(cn, carrier.name))

  emf <- get_all_vars(tt, data = expand.model.frame(model, varnames, na.expand=TRUE))

  ## experimenting with another way to gather variables.
  ## data <- eval(model$call$data, envir) ##when ever needed?
  ## if (is.null(data)) 
  ##   data <- mf
  ## ## if (plotx %in% varnames) plotxVar <- emf[, plotx] else stop("plotx missing")
  ## data <- data[row.names(emf) , ]
  
  plotxVar <- carrier(parse(text = plotx), emf, enc=envir)
  modxVar <- carrier(parse(text = modx), emf, enc=envir)

  depVar <- mf[, 1]
  
  if (!is.numeric(plotxVar)) 
    stop(paste("plotCurves: The variable", plotx, "should be a numeric variable"))
  ylab <- colnames(mf)[1]
  ##ylab <- varnames[1] ## returns untransformed carrier DV
  plotyRange <- magRange(depVar, mult=c(1,1.2))
  plotxRange <- range(plotxVar, na.rm=TRUE)
  plotxSeq <- plotSeq(plotxRange, l = 40)

  if (is.factor(modxVar)) { ## modxVar is a factor
    if (is.null(modxVals)) {
      modxVals <- levels(modxVar)
    } else if (!modxVals %in% levels(modxVar)) stop("modxVals includes non-observed levels of modxVar")
  } else {                  ## modxVar is not a factor
    modxRange <- range(modxVar, na.rm=TRUE)
    if (is.null(modxVals)) {
      modxVals <- quantile(modxVar, na.rm = TRUE)
    } else if (is.numeric(modxVals)) { 
      ##TODO: Insert some checks that modxVals are reasonable
    } else if (is.character(modxVals)) {
      modxVals <- match.arg(tolower(modxVals),
                            c("quantile", "std.dev."))
      print(modxVals)
      modxVals <- switch(modxVals,
                         quantile = quantile(modxVar, na.rm = TRUE ),
                         "std.dev." = cutBySD(modxVar),
                         stop("unknown 'modxVals' algorithm"))
    }
  }
  lmx <- length(modxVals)                            
  
  predictors <- colnames(emf)[-1]
  predictors <- setdiff(predictors, c(modx, plotx))
  newdf <- data.frame(expand.grid(plotxSeq, modxVals))
  colnames(newdf) <- c(plotx, modx)
  if (length(predictors) > 0) {
    newdf <- cbind(newdf, centralValues(as.data.frame(emf[, predictors])))
    colnames(newdf) <- c(plotx, modx, predictors)
  }
  newdf$pred <- predict(model, newdata = newdf)
  dotargs <- list(...)
  if (!plotPoints){
    parms <- list(plotxVar, depVar, xlab = plotx, ylab = ylab, ylim = plotyRange,
         type = "n")
    parms <- modifyList(parms, dotargs)
    do.call("plot", parms)
  } else {
    if (is.factor(modxVar)) {
      parms <- list(plotxVar, depVar, xlab = plotx, ylab = ylab, ylim = plotyRange,
           col = modxVar)
      parms <- modifyList(parms, dotargs)
      do.call("plot", parms)
    }
    else {
      parms <- list(plotxVar, depVar, xlab = plotx, ylab = ylab, ylim = plotyRange)
      parms <- modifyList(parms, dotargs)
      do.call("plot", parms)
    }
  }
  for (i in 1:lmx) {
    pdat <- newdf[newdf[, modx] %in% modxVals[i], ]
    lines(pdat[, plotx], pdat$pred, lty = i, col = i, lwd = 2)
  }
  if (is.null(names(modxVals))) {
    legnd <- paste(modxVals, sep = "")
  }
  else {
    legnd <- paste(names(modxVals), sep = "")
  }
  legend("topleft", legend = legnd, lty = 1:lmx, col = 1:lmx, 
         bg = "white", title= paste("moderator:", modx))

  invisible(list(call=cl, newdata=newdf, modxVals = modxVals))
}
