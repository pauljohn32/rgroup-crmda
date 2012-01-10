##' Draw a 3-D regression plot for two predictors from any linear or nonlinear lm or glm object
##'
##' This allows user to fit a regression model with many variables and
##' then plot 2 of its predictors and the output plane for those
##' predictors with other variables set at mean or mode (numeric or
##' factor).  This is a front-end (wrapper) for R's persp function.
##' Persp does all of the hard work, this function reorganizes the
##' information for the user in a more readily understood way.  It
##' intended as a convenience for students (or others) who do not
##' want to fight their way through the details needed to use persp to
##' plot a regression plane.  The fitted model can have any number of
##' input variables, this will display only two of them. And, at least
##' for the moment, I insist these predictors must be numeric
##' variables. They can be transformed in any of the usual ways, such
##' as poly, log, and so forth.
##' 
##' Besides a fitted model object, plotPlane requires two additional
##' arguments, plotx1 and plotx2. These are the names of the plotting
##' variables. Please note, that if the term in the regression is
##' something like poly(fish,2) or log(fish), then the argument to
##' plotx1 should be the quoted name of the variable "fish".
##' plotPlane will handle the work of re-organizing the information so
##' that R's predict functions can generate the desired information.
##' This might be thought of as a 3D version of "termplot", with a
##' significant exception. The calculation of predicted values depends
##' on predictors besides plotx1 and plotx2 in a different ways. The
##' sample averages are used for numeric variables, but for factors
##' the modal value is used.
##'
##' For details, please consult the source code, which is being
##' cleaned up.
##' @param model an lm or glm fitted model object
##' @param plotx1 name of one variable to be used on the x1 axis
##' @param plotx2 name of one variable to be used on the x2 axis
##' @param drawArrows draw red arrows from prediction plane toward observed values TRUE or FALSE
##' @param plotPoints Should the plot include scatter of observed scores?
##' @param npp number of points at which to calculate prediction 
##' @param x1lab optional label 
##' @param x2lab optional label 
##' @param ylab optional label 
##' @param envir environment from whence to grab data 
##' @param ... additional parameters that will go to persp
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @rdname plotPlane
##' @export plotPlane
##' @seealso \code{\link[graphics]{persp}}, \code{\link[scatterplot3d]{scatterplot3d}}, \code{\link[HH]{regr2.plot}}
##' @example inst/examples/plotPlane-ex.R
plotPlane <- function(model = NULL,  plotx1 = NULL, plotx2 = NULL, drawArrows = FALSE, plotPoints = TRUE, npp = 20, x1lab, x2lab, ylab, envir = environment(formula(model)),  ...){
  UseMethod("plotPlane")
}



##' @return  The main point is the plot that is drawn, but for record keeping the return object is a list including 1) res: the transformation matrix that was created by persp (this allows the user to add additional details to the plot, 2) the call that was issued). 
##' 
##' @rdname plotPlane
##' @method plotPlane default
##' @S3method plotPlane default
plotPlane.default <- function (model = NULL, plotx1 = NULL, plotx2 = NULL, drawArrows = F, plotPoints = TRUE, npp = 20, x1lab, x2lab, ylab, envir = environment(formula(model)),  ...){
  if (is.null(model)) 
    stop("plotSlopes requires a fitted regression model.")
  if (is.null(plotx1) | is.null(plotx2)) 
    stop("plotSlopes requires the name of the variable to be drawn on the x axis")
  if (plotx1 == plotx2) stop("the two plotting variables should not be the same")
 
   carrier.name <- function(term) {
     if (length(term) > 1L) 
       carrier.name(term[[2L]])
     else as.character(term)
   }

  cl <- match.call()
  ## mm <- model.matrix(model) ##intercept first, no y
  mf <- model.frame(model) ##y first, no intercept
  tt <- terms(model)
  
  ## The dependent variable
  y <- model.response(mf)
  ## Create "varnames", the names of variables that are used
  ## somewhere in the model formula. It extracts "fish" from poly(fish,2), e.g.
  cn <- parse(text = colnames(mf))      
  varnames <- unlist(lapply(cn, carrier.name))

  ## Need a dataframe that has all elements from "varnames" in it.
  emf <- get_all_vars(tt, data=expand.model.frame(model, varnames))
  
  if (plotx1 %in% varnames )
    x1 <- emf[, plotx1]
  if (!is.numeric(x1)) 
    stop(paste("plotSlopes: The variable", plotx1, "should be a numeric variable"))
  x2 <- emf[, plotx2]
  if (!is.numeric(x2)) 
    stop(paste("plotSlopes: The variable", plotx2, "should be a numeric variable"))
  
  if (missing(ylab)) ylab <- colnames(model$model)[1]
  if (missing(x1lab)) x1lab <- plotx1
  if (missing(x2lab)) x2lab <- plotx2
  
  x1range <- magRange(x1, 1.25)
  x2range <- magRange(x2, 1.25)

  ##TODO must double check effect of function predictors
  otherPredictors <- varnames[-1] #all but y
  otherPredictors <- setdiff(otherPredictors, c(plotx2, plotx1)) #remove x1 x2
  if (length(otherPredictors) > 0) {
    otherPredictorValues <- centralValues(as.data.frame(model$model[, otherPredictors]))
  }
  
  myPredict <- function(a,b){
    ndf <- data.frame(a, b) #ndf = new data frame
    colnames(ndf) <- c(plotx1, plotx2)
    if (length(otherPredictors) > 0) {
      ndf <- cbind(ndf, otherPredictorValues)
      colnames(ndf) <- c(plotx1, plotx2, otherPredictors)
    }
    if ("glm" %in% class(model)) {
      predict(model, newdata = ndf, type = "response")
    }else{
      predict(model, newdata = ndf)
    }
  }
  
  x1seq <- plotSeq(x1range, length = npp)
  x2seq <- plotSeq(x2range, length = npp)
  zplane <- outer(x1seq, x2seq, function(a, b) { myPredict(a,b) } )
  
  yrange <- magRange(c(zplane,y), 1.15)
  
  res <- perspEmpty(x1 = x1range, x2 = x2range, y = yrange, 
                    x1lab = x1lab, x2lab = x2lab, ylab = ylab, ...)

  if (plotPoints){
    mypoints2 <- trans3d(x1, x2, y, pmat = res)
    points(mypoints2, pch = 1, col = "blue")
  }


  for (i in 1:length(x1seq)) {
    lines(trans3d(x1seq[i], x2seq, zplane[i, ], pmat = res), 
          lwd = 0.3)
  }
  for (j in 1:length(x2seq)) {
    lines(trans3d(x1seq, x2seq[j], zplane[, j], pmat = res), 
          lwd = 0.3)
  }

  ##for arrows. NEEDS reworking to be more general
  if ("glm" %in% class(model)) {
    fits <-  predict(model, type = "response")
  }else{
    fits <-  fitted(model)
  }
    mypoints4 <- trans3d(x1, x2, fits, pmat = res)
    newy <- ifelse(fits < y, fits + 0.8 * (y - fits), 
        fits + 0.8 * (y - fits))
    mypoints2s <- trans3d(x1, x2, newy, pmat = res)
    if (drawArrows) 
        arrows(mypoints4$x, mypoints4$y, mypoints2s$x, mypoints2s$y, 
            col = "red", lty = 4, lwd = 0.3, length = 0.1)
    invisible(list(res=res, call=cl))
}
