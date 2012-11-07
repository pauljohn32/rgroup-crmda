## Paul E. Johnson CRMDA <pauljohn@ku.edu>
## Portable Parallel Seeds Project.
## 2012-10-24


##' Selects among available random streams.
##'
##' R's global environment variable .Random.seed is re-set so that
##' random numbers generated after this call will follow from a
##' designated random number state. This will fail unless the
##' \code{initPortableStreams} function has already been called. This
##' function simply selects the n'th random stream for use,
##' presupposing that those stream states have already been set in the
##' global environment.
##'
##' This should handle three chores.  1) Copy the existing state of
##' the random generator (.Random.seed) into
##' currentStates[[currentStream]]. That allows us to "go back" to
##' that generator when we want to.  2) Change currentStream variable
##' to n, then 3) Re-set R's .Random.seed variable to the value from
##' currentStates[[currentStream]], so that successively drawn random
##' numbers follow the proper generator.
##' @export useStream
##' @param n An integer that selects which random stream should be used for the following work.
##' @param origin True or False. Should the stream be set at its original starting position, so as to re-generate the stream starting from the beginning? If FALSE, random numbers are drawn from the stream's current state.
##' @param verbose Requests detailed output for diagnostics.
##' @return Nothing. This function is run to change environment variables.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
useStream <- function(n = NULL, origin = FALSE, verbose = FALSE){
  oldseed <-
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    else stop("in useStream, .Random.seed was NULL")
  ## get local copies of currentStream, currentStates
  curStream <- get("currentStream", envir = .GlobalEnv, inherits = FALSE)
  curStates <- get("currentStates", envir = .GlobalEnv, inherits = FALSE)

  if (n > length(curStates)) stop("requested stream does not exist")
  curStates[[curStream]] <- oldseed
  if (origin) {
    strtStates <- get("startStates", envir = .GlobalEnv, inherits = FALSE)
    assign(".Random.seed", strtStates[[n]], envir = .GlobalEnv)
  } else {
    assign(".Random.seed", curStates[[n]], envir = .GlobalEnv)
  }
  ## put currentStream and currentStates back to .GlobalEnv

  assign("currentStream", n, envir = .GlobalEnv)
  assign("currentStates", curStates, envir = .GlobalEnv)
  if (verbose){
    print("useStream useStream useStream useStream")
    print("CurrentStream CurrentStream CurrentStream")
    print( get("currentStream", envir = .GlobalEnv, inherits = FALSE) )
    print("Current .Random.seed")
    print(.Random.seed)
  }
}


##' Return integer representing currently selected random stream
##'
##' The environment variable currentStates is a list of states
##' of possible random generators. The variable currentStream
##' indicates which of those generators is currently being used
##' by R.
##'
##' @export getCurrentStream
##' @return Integer index value of currently selected stream
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @examples
##' mySeeds <- seedCreator(500, 5, file="mySeeds.rds", seed = 123123)
##' initPortableStreams(mySeeds, run = 17)
## runif(2)
##' getCurrentStream()
##' useStream(2)
##' runif(2)
getCurrentStream <- function(){
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    else stop("in useStream, .Random.seed was NULL")
    ## get local copies of currentStream, currentStates
    curStream <- get("currentStream", envir = .GlobalEnv, inherits = FALSE)
    curStream
}

##' Brings saved random streams back to life. Reads a portable
##' parallel seeds object (or file) and sets the seed collection in
##' the global environment.
##'
##' The portable seeds object is created by the function
##' \link{seedCreator}. It is a list of lists. The list includes one
##' set of initializing states for each separate run of a simulation.
##' Within each of these sets, there will be enough information to
##' initialize one or more streams of random numbers.  These of
##' "initializing states" are the internal states of CMRG random
##' generators (see L'Ecuyer, 1999; L'Ecuyer, et al, 2002).
##'
##' This function scans the project's portable parallel seeds (either
##' an in-memory object or a named file), selects the desired run, and
##' then it writes 3 variables into the R global environment. 1)
##' startStates is a collection of random generator states, one for
##' each random stream from which the user might wish to draw random
##' numbers. This is a fixed value which should not be altered during
##' the program. It can be used to reset the generators to their
##' initial positions. 2) currentStates is the collection of random
##' generator states that will be updated. When the program calls the
##' \link{useStream} function, the currentStates vector is updated.
##' 3) currentStream indicates which of the currentStates should be
##' used to draw the next random value.
##'
##' At the outset, startStates and
##' currentStates are identical and currentStream equals 1 (meaning
##' the first element of currentStates is taken as the state of the
##' random generator).
##' @export initPortableStreams
##' @title initPortableStreams
##' @param projSeeds Either an object of class portableSeeds (created
##' by \code{seedCreator}) or a text string giving the name of an R
##' saved file of the appropriate format (created by the seedCreator function).
##' @param run Integer indicating which element from the portable seed collection is to be selected
##' @param verbose True or False: print out the state of the current generator
##' @return nothing is returned. This function is used for the side effect of seetting three objects in the global environment, the startStates (list), currentStates (list), and currentStream (an integer).
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @seealso \code{seedCreator} to generate the input file for this function and \code{useStream} to change from one stream to another.
##' @example inst/examples/pps-ex.R

initPortableStreams <- function(projSeeds, run, verbose = FALSE){
    require(parallel)
    RNGkind("L'Ecuyer-CMRG")

    if (missing(projSeeds)) {
        stop("initPortableStreams requires a seed object in order to initialize the random streams")
    } else if (is.character(projSeeds)){
        projSeeds <- readRDS(projSeeds)
    }
    if (class(projSeeds) != "portableSeeds"){
        stop("Inappropriate project seed object supplied. The projSeeds object must be created by the seedCreator function, which would have set its class as portableSeeds")
    }


    if (missing(run)) stop("run must be specified. Which replication is to be re-initialized?")
   ## if (length(projSeeds) < run) stop(paste("The project seed object does not include enough elements to draw the one you are asking for. The seed object includes only ", length(projSeeds), " objects."))
  runSeeds <- projSeeds[[run]]
  assign("currentStream",  1L, envir = .GlobalEnv)
  assign("startStates", runSeeds, envir = .GlobalEnv)
  assign("currentStates", runSeeds, envir = .GlobalEnv)
  assign(".Random.seed", runSeeds[[1L]],  envir = .GlobalEnv)
  if (verbose){
    print(paste("initPortableStreams, Run = ", run))
    print(.Random.seed)
    print(paste("CurrentStream CurrentStream CurrentStream =", get("currentStream", envir = .GlobalEnv, inherits = FALSE)))
    print("All Current States")
    print(paste(get("currentStates", envir = .GlobalEnv, inherits = FALSE)))
  }
}
