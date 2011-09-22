
#rm(list=(ls(all=T)))

################################# Specify Functions #######################################

#require(snowFT)

go.baby.go.fun <- function(x,parms)
  {
    
sim.data <- function(parms)
{
  require(mvtnorm)
  
  nobs <- parms$nobs
  mfK <- parms$mfK
  
  phi <- matrix(c(1,.5,.5,1),2,2)

  lambda <- matrix(c(rep(.6,5),rep(0,5),
                     rep(0,5),rep(.6,5),
                     rep(.1,5),rep(.2,5),
                     rep(.3,5),rep(.15,5),
                     rep(.2,5),rep(.1,5),
                     rep(.05,5),rep(.1,5)),10,6)
  
  theta <- toeplitz(c(.3,0,0,mfK,0,0,mfK,0,0,mfK))

  eta <- rmvnorm(nobs,c(0,0),phi)

  male <- rbinom(nobs,1,.5)

  white <-  rbinom(nobs,1,.75)

  Covs <- rmvnorm(nobs,c(0,0),matrix(c(1,0,0,1),2,2))

  preds <- cbind(eta,male,white,Covs)

  colnames(preds) <- c("F1","F2","male","white","c1","c2")

  errors <- rmvnorm(nobs,rep(0,10),theta)

  dat <- preds %*% t(lambda) + errors

  new.dat <- cbind(dat,preds[,3:6])
  
  colnames(new.dat) <- c(paste("a", 1:5, sep=""),paste("b", 1:5, sep=""),"male","white","c1","c2")
  
  new.dat

}



impose.missing <- function(dat, parms)
{

  min.pm <- parms$min.pm
  max.pm <- parms$max.pm
  pm.step <- parms$pm.step
  
miss.list <- list()
pm.vec <- seq(min.pm,max.pm,pm.step)  

  
  makin.missin <- function(x,dat,parms)
  {
   
    z <- x
   
  len.scale <- parms$len.scale
  cut.var <- parms$cut.var
  cut.prob <- parms$cut.prob

  cutoff <- quantile(cut.var,cut.prob)

  nrow=dim(dat)[1]
    ncol=dim(dat)[2]
  
  #  Create logical array indicating locations of values that exceed the 
  #  conditional value and count how many are eligible for deletion
   
    eligible.la <- dat[,cut.var] < cutoff 
    count.eligible <- sum(eligible.la)

  # Calculate the ratio of observations that are elibible for deletion    
  
    ratio.eligible <- count.eligible / nrow
  
  # ratio of deletion is the percent of the data that meets the condition for 
  # deletion that must be removed to achieve the desired percent missing
  
    ratio.deletion <- (z*.01) / ratio.eligible  

  # Create a logical array indicating where we should create missing data
    
    eligible.mat <- matrix(rep(eligible.la, len.scale),
                         nrow=nrow, ncol=len.scale)
                         
    random.gen <- function(x)
      {
  	if (x){runif(1, 0, 1) < ratio.deletion}
  	else FALSE
      }
  
    random.la <- matrix(sapply(eligible.mat, FUN=random.gen), ncol=len.scale)
                      
  # Complete the array (with FALSE) to be the same size as the original data
    
    random.la.comp <- cbind(random.la, matrix(FALSE,nrow=nrow,ncol=ncol-len.scale))

  # Insert NA wherever there is a TRUE in the index array
  
    dat[random.la.comp] <- NA

  # Populate miss.list with the incomplete matrices from the above opperations:

    dat
  }
  
miss.list <- lapply(pm.vec, FUN=makin.missin, dat=dat, parms=parms)

miss.list

} #end impose.missing()


impute.stack <- function(dat,parms)
{
  require(Amelia)
  
  imps <- parms$imps
  len.scale <- parms$len.scale
  
  amelia.out <- amelia(dat,m=imps,empri=.1*dim(dat)[1])

  stacked.dat <- do.call("rbind",amelia.out[[1]])

  raw.cov <- lapply(amelia.out[[1]],cov)
  
  super.mat <- cov(stacked.dat[,1:len.scale])

  list(raw.cov=raw.cov, super.mat=super.mat)
  
} # end impute.stack() 



fit.missin.models <- function(dat,parms)
{
  require(lavaan)

  mod1 <- parms$mod1
  sm.cov <- dat$super.mat
  raw.covs <- dat$raw.cov
  nobs <- parms$nobs
  imps <- parms$imps
  
  sm.full.mod <- cfa(mod1, sample.cov=sm.cov, sample.nobs=nobs,std.lv=T)	

  sm.res.mod <- cfa(mod1, sample.cov=sm.cov, sample.nobs=nobs,std.lv=T,orthogonal=T)

 fit.raw.mods <- function(x,parms)
    {
      mod1 <- parms$mod1
      nobs <- parms$nobs
      
       full.mod <- cfa(mod1, sample.cov=x, sample.nobs=nobs, std.lv=T)

       res.mod <- cfa(mod1, sample.cov=x, sample.nobs=nobs, std.lv=T, orthogonal=T)

      list(raw.full.mod = full.mod, raw.res.mod = res.mod)
     }

  raw.miss.out <- lapply(raw.covs,FUN=fit.raw.mods,parms=parms)

  list(sm.miss.out=list(sm.full.mod=sm.full.mod, sm.res.mod=sm.res.mod), raw.miss.out=raw.miss.out)

}# End fit.miss.models() 


fit.control.model <- function(dat,parms)
  {
    require(lavaan)

    nobs <- parms$nobs
    len.scale <- parms$len.scale
    mod1 <- parms$mod1

    con.cov <- cov(dat[,1:len.scale])
        
  full.mod <- cfa(mod1, sample.cov=con.cov, sample.nobs=nobs,std.lv=T)	

  res.mod <- cfa(mod1, sample.cov=con.cov, sample.nobs=nobs,std.lv=T,orthogonal=T)

control.out <- list(con.full.mod=full.mod, con.res.mod=res.mod)

}# End fit.control.model()



##
## Combine fundamental functions above into a larger aggregation to run the stuff for 1 cell:
##



run.miss.sim <- function(x,parms)
{
  len.scale <- parms$len.scale
  nobs <- parms$nobs

        imp.dat <- impute.stack(dat=x,parms)

	your.missin.out <- fit.missin.models(dat=imp.dat,parms)

        pm <- sum(is.na(x))/(len.scale*nobs)
                  
        list(cell.pm=pm, cell.missing.out=your.missin.out)
        
}# end run.miss.sim()




##
##
## Aggregate the two above functions to run a single row of the results matrix:
##
##



row.task <-function(x, dat, parms)
  {

    nobs <- parms$nobs
    len.scale <- parms$len.scale

    NewNobs <- (nobs-x)
    
    cull.dat <- dat[1:NewNobs,]

    parms$nobs <- NewNobs
    
    con.out <- fit.control.model(dat=cull.dat,parms)

    incomp.list <- impose.missing(dat=cull.dat,parms)

    miss.out <- lapply(incomp.list,FUN=run.miss.sim,parms)

    list(row.control.out=con.out, row.missing.out=miss.out)

  }# end row.task()




# Runs row.task() over all samplesizes to get an entire replication:

rep.task <- function(x, parms)
  {

    sam.inc <- parms$sam.inc
    
    fresh.dat <- sim.data(parms)

    rep.out <- lapply(sam.inc, FUN=row.task, dat=fresh.dat, parms=parms)

   }# end rep.task()

rep.task(x,parms) #execute rep.task() as the primary task of go.baby.go.fun()

}# end go.baby.go.fun()



##################################### Begin Parallel Runs (snowFT) ###################################

myseeds <- rep(235711,6)

rp <- 1 # seq(1:1000)

cnt <- 4

parameters <- list()

parameters$sam.inc <- seq(0,400,10)
parameters$cut.var <- "c1"
parameters$cut.prob <- .70
parameters$len.scale <- 10
parameters$min.pm <- 2
parameters$max.pm <- 50
parameters$pm.step <- 2
parameters$imps <- 100
parameters$nobs <- 500
parameters$mfK <- .05
parameters$mod1 <- "ConA =~ NA*a1 + a2 + a3 + a4 + a5
                    ConB =~ NA*b1 + b2 + b3 + b4 + b5"


#result <- performParallel(count=cnt, x=rp, fun=go.baby.go.fun, seed=myseeds, cltype="MPI", gentype="RNGstream", ft_verbose=T, parms=parameters)

result <- go.baby.go.fun(1, parameters)

save(result,file="pp.result.RData")

