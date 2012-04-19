
##' recode a factor by putting observations with 2 or more values into a single new value
##'
##' If a factor variable is currently coded with levels
##' c("Male","Female","Man", "M"), and the user needs to combine the
##' redundant levels for males, this is the function to use!
##'
##' If the factor is an ordinal factor, then levels may be combined
##' only if they are adjacent. That is to say, a factor with levels
##' c("Lo","Med","Hi","Extreme") allows us to combine responses "Lo"
##' and "Med", while it will NOT allow us to combine "Lo" with "Hi".
##'
##' A non-ordered factor can be reorganized to combine any values, no
##' matter what positions they occupy in the levels vector.
##'
##' @param fac An R factor variable, either ordered or not.
##' @param levs The levels to be combined. Users may specify either a
##' numerical vector of level values, such as c(1,2,3), to combine the
##' first three elements of level(fac), or they may specify level
##' names.  This can be done as a character vector of *correctly
##' spelled* factor values, such as c("Yes","Maybe","Always") or it
##' may be provided as a subset of the output from levels, such as
##' levels(fac)[1:3].
##' @param newLabel A character string that represents the label of
##' the new level to be created when \code{levs} values are combined.
##' @name combineLevels
##' @export combineLevels
##' @return A new factor variable, with unused levels removed.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @examples
##' x <- c("M","A","B","C","A","B","A","M")
##' x <- factor(x)
##' levels(x)
##' combineLevels(x, levs = c("M","A"), newLabel = "M_or_A")
##' xnew <- combineLevels(x, levs = c("M","A"), newLabel = "M_or_A")

##' ## verify
##' addmargins(table(xnew, x))
##' combineLevels(x, c(1,4), "M_or_A")
##' ## Now an ordinal factor
##' z <- c("M","A","B","C","A","B","A","M")
##' z <- ordered(z)
##' z
##' levels(z)
##' combineLevels(z, levs=c(1,2), "Good")
##' z2 <- combineLevels(z, levs=c("A","B"), "AorB")
##' attributes(z2)
##' ## Should fail:
##' z2 <- combineLevels(z, levs=c("A","C"), "Whoops!")
##'
combineLevels <- function(fac, levs, newLabel){
    ##internal fn to discern connected sequence
    adjacent <- function(x){
        xfull <- seq(min(x),max(x))
        identical(xfull, as.integer(x))
    }

    if(is.missing(newLabel)) stop("newLabel is required for the newly combined observations")

    facl <- levels(fac)
    if (is.character(levs)){
        if(!identical(sum(levs %in% facl), length(levs))){
            stop(cat("Error: requested levels: \"",levs,"\" are not in the legal list of factor levels:\"", facl, "\""))
        }
    } else {
        if( sum(!levs %in% 1:length(facl)) > 0) stop(cat("Error: requested levs don't exist in the factor"))
  }

    ##convert levs to numeric indices
   if(is.character(levs)) levs <- which( facl %in% levs )

    ##for NOT ORDINAL factors, easy. Put new level on end

    if (! "ordered" %in% class(fac)){
        levels(fac) <- c(levels(fac), newLabel)
        faclnew <- levels(fac)
        fac[ as.numeric(fac) %in% levs ] <- newLabel
    }else{
        if ("ordered" %in% class(fac)){  ## levels must be adjacent
            if (!adjacent(levs)) {
                stop("fac is ordered. The levels to be combined must be adjacent")
            }
            levels(fac) <- c(facl[1:min(levs)], newLabel, facl[(1+min(levs)):length(facl)])
            faclnew <- levels(fac)
            fac[ as.numeric(fac) %in% levs ] <- newLabel
        }
    }

    fac <- fac[ , drop=TRUE]
    cat("The original levels", facl, "\nhave been replaced by", levels(fac),"\n")
    fac
}

