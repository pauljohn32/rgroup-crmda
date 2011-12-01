##' Miscellaneous regression functions
##' 
##'  
##' \tabular{ll}{ Package: \tab rockchalk\cr Type: \tab Package\cr Version: \tab
##' 1.0\cr Date: \tab 2011-08-29\cr License: \tab GPL >= 3\cr LazyLoad: \tab
##' yes\cr } ~~ An overview of how to use the package, including the most
##' important ~~ ~~ functions ~~
##' 
##' @name rockchalk-package
##' @aliases rockchalk-package rockchalk
##' @docType package
##' @author Paul E. Johnson \email{pauljohn@@ku.edu}
##' 
##' Maintainer: Paul Johnson \email{<pauljohn@@ku.edu>}
##' @references http://pj.freefaculty.org/R
##' @keywords regression hplot
NULL


##' Religious beliefs and crime rates
##'
##' The data national-level summary indicators of public opinion about
##' the existence of heaven and hell as well as the national rate of
##' violent crime.
##' @name religioncrime
##' @docType data
##' @usage data(religioncrime)
##' @author Paul E. Johnson \email{pauljohn@@ku.edu} and Anonymous
##' @format data.frame: 51 obs. of 3 variables
##' @keywords datasets
##' @source  Anonymous researcher who claims the data is real.
##' @keywords datasets
##' @examples
##' require(rockchalk)
##' data(religioncrime)
##' mod1 <- lm(crime ~ heaven, data=religioncrime)
##' mod2 <- lm(crime ~ hell, data=religioncrime)
##' mod3 <- lm(crime ~ heaven + hell, data=religioncrime)
##' with(religioncrime,
##' mcGraph1(heaven, hell, crime)
##' )
##' with(religioncrime,
##' mcGraph2(heaven, hell, crime)
##' )
##' mod1 <- with(religioncrime,
##' mcGraph3(heaven, hell, crime)
##' )
##' summary(mod1[[1]])
##' ##TODO: Draw more with perspective matrix mod1[[2]]
NULL
