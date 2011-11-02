#' perspEmpty
#' Creates a persp plot without drawing anything in the interior.
#' Does equivalent \code{of plot( type="n")} for persp.
#' 
#' Regression demonstrations require a blank slate in which
#' points and planes can be drawn. This function creates that
#' blank persp canvas for those projects. It is not necessary
#' that x1, x2 and y be vectors of the same length, since this
#' function's only purpose is to plot an empty box with ranges
#' determined by the input variables. persp calls the 3 axes
#' x, y, and z, but here they are called x1, x2, and y.
#'
#' @usage perspEmpty(x1, x2, y, ... )
#' @param x1 data for the first horizontal axis, an R vector 
#' @param x2 data for the second horizontal axis, an R vector
#' @param y data for the vertical axis, an R vector
#' @param ... further arguments that are passed to persp
#' @name perspEmpty
#' @export perspEmpty
#' @examples
#' x1 <- 1:10
#' x2 <- 40:50
#' y <-  rnorm(10)
#' perspEmpty(x1, x2, y)
#' perspEmpty(x1, x2, y, ticktype="detailed", nticks=10)

perspEmpty <- function(x1, x2, y, ... ){
  x1range <- range(x1)
  x2range <- range(x2)
  yrange <- range(y)
 
  zZero <- outer( plotSeq(x1range, l=5), plotSeq(x2range, l=5), function( a,b) { a*b*0 + yrange[1] } )

  dotargs <- list(...)

  myDefaults <- list(x=plotSeq(x1range, l=5), y= plotSeq(x2range, l=5), z=zZero, zlim=yrange, lwd=1, xlab="x1",ylab="x2",zlab="y", theta=0, phi=15)

  myargs <- modifyList(myDefaults, dotargs)
  res <- do.call("persp", myargs)
}

