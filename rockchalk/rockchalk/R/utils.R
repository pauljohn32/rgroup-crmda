#' Create sequences for plotting
#'
#' \code{plotSeq} is a convenience for the creation of sequence
#' that can be used for plotting example values and calculating
#' predicted values. By default, the length of the plotting
#' sequence will be equal to the length of the original sequence.
#' In that case, the only effect is to create an evenly-spaced
#' set of values. If \code{length.out} is specified, the user
#' determines the number of elements in plotSeq.
#'
#' The primary intended usage is for the creation of
#' plotting sequences of numeric variables.  It takes
#' a variable's range and the fills in evenly spaced steps.
#' If x is a factor variable, the levels will be returned.
#' Uses of this functionality are planned in the future.
#' @usage plotSeq(x, length.out = length(x))
#' @param x an R vector variable
#' @param length.out the number of elements in the desired plotting sequence.
#' @export plotSeq
#' @seealso \code{pretty}
#' @examples
#' #Create a quadratic regression
#'
#' stde <- 14
#' x <- rnorm(100, m=50, s=10)
#' y <- 0.2 - 02*x + 0.2*x^2 + stde*rnorm(100)
#' mod1 <- lm (y ~ poly(x, 2))
#'
#' plot(x, y, main="The Quadratic Regression")
#' seqx <- plotSeq(x, length.out=10)
#' seqy <- predict(mod1, newdata=data.frame(x=seqx))
#' lines(seqx, seqy, col="red")
#'
#' # Notice the bad result when a plotting sequence is
#' # not used.
#' plot(x, y, main="Bad Plot Result")
#' seqy <- predict(mod1)
#' lines(x, seqy, col="green")

plotSeq <- function (x, length.out = length(x))
{
  if (is.numeric(x)){
    xr <- range(x, na.rm=T)
    pseq <- seq(xr[1], xr[2], length.out = length.out)
    return(pseq)
  }else{
    if (is.factor(x)){
      pseq <- levels(x)
      return(pseq)
    }else{
      stop("plotSeq can only create plotting sequences for numeric or factor variables")
    }
  }
}



##' Select most frequently occurring values from numeric or categorical variables.
##'
##' The "n" most frequently occurring values are returned, sorted by
##' frequency of occurrence (in descending order).
##'
##' This is used by plotSlopes, plotCurves, and other "newdata" making
##' functions.
##' @param x A numeric or character variable
##' @param n The maximum number of values that may be returned.
##' @return A named vector.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
cutByTable <- function(x, n = 5) {
    table1 <- table(x)
    n <- min(n, length(names(table1)))
    table1sort <-  sort(table1, decreasing = T)
    qs <- table1sort[1:n]
    names(qs) <- names(table1sort[1:n])
    invisible(qs)
}
NULL


## TODO:2012-04-23 now this only supplies the median, median+/-25.
## It ignores n, should fix.

##' Calculates the "center" quantiles, always including the median.
##'
##' If the numeric variable has fewer than 6 unique observed values,
##' this will send the data to cutByTable.
##' @param x A numeric vector.
##' @param n Currently ignored, will be the number of quantile points
##' @return A vector
##' @author Paul E. Johnson <pauljohn@@ku.edu>
cutByQuantile <- function(x, n = 3){
    uniqueVals <- unique(x)
    if (length(uniqueVals) < 6) {
        qs <- cutByTable(x, 5)
        invisible(qs)
    } else {
        cutVector <-
        qs <- quantile(x, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
        invisible(qs)
    }
}


##' Returns center values of x, the mean, mean-std.dev, mean+std.dev
##'
##' If the numeric variable has fewer than 6 unique observed values,
##' this will send the data to cutByTable.
##' @return A named vector
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @param x A numeric variable
cutBySD <- function(x){
    uniqueVals <- unique(x)
    if (length(uniqueVals) < 6) {
        qs <- cutByTable(x, 5)
        invisible(qs)
    } else {
        mx <- round(mean(x, na.rm=T),2)
        sdx <- round(sd(x, na.rm=T),2)
        ##qs <- c(mx - 2*sdx, mx - sdx, mx, mx + sdx, mx + 2*sdx)
        ##suffix <- c("(m-2sd)","(m-sd)","(m)","(m+sd)","(m+2sd)")
        qs <- c(mx - sdx, mx, mx + sdx)
        suffix <- c("(m-sd)","(m)","(m+sd)")
        names(qs) <-  paste(qs, suffix)
        invisible(qs)
    }
}
