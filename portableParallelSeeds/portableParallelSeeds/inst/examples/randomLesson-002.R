##Paul E. Johnson
## pauljohn@ku.edu 2012-10-25
set.seed(12345)

## Now, change the generator to a different type,
## the CMRG (L'Ecuyer, 1999).


RNGkind("L'Ecuyer-CMRG")
set.seed(12345)
runif(1)
t1 <- .Random.seed
t1


runif(1)
t2 <- .Random.seed
runif(1)
t3 <- .Random.seed
rnorm(1)
t4 <- .Random.seed
cbind(t1, t2, t3, t4)



assign(".Random.seed", t1, envir=.GlobalEnv)
u1 <- .Random.seed
runif(1)
u2 <- .Random.seed
runif(1)
u3 <- .Random.seed
runif(1)
u4 <- .Random.seed
runif(1)
u5 <- .Random.seed
cbind(u1, u2, u3, u4, u5)





