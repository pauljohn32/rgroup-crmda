#' plotSeq
#' Create an evenly spaced sequence over the range of a variable
#'
#' \code{plotSeq} is a convenience for the creation of sequence
#' that can be used for plotting example values and calculating
#' predicted values. By default, the length of the plotting
#' sequence will be equal to the length of the original sequence.
#' In that case, the only effect is to create an evenly-spaced
#' set of values. If \code{length.out} is specified, the user
#' determines the number of elements in plotSeq.
#'
#' @usage plotSeq(x, length.out = length(x))
#' @param x an R vector variable
#' @param length.out the number of elements in the desired plotting sequence.
#' @export plotSeq
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


plotSeq <- function(x, length.out=length(x)){
  xr <- range(x)
  pseq <- seq(xr[1], xr[2], length.out=length.out)
  pseq
}
