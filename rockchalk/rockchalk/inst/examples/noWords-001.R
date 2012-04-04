
1 + 4
exp(7)
sqrt(81)
log(4)

x <- c(1,2,3,4,5,6)
x
exp(x)
sqrt(x)
log(x)
1/x

x[4]
x[4:6]
x[c(1,4,5)]
x[ x > 4 ]

attributes(x)
is.null(x)
is.vector(x)
is.numeric(x)
is.character(x)
is.data.frame(x)
is.logical(x)
is.matrix(x)
is.list(x)


y <- c(80, 90, 100, 110, 120, 130)
x + y
x - y
y * x
x * y
y^x
3*x + 4*y


z <- c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
is.logical(z)
which(z == TRUE)
which(z)
which(z != TRUE)
which(!z)

print("Exclamation mark (!) means NOT")
cat("Exclamation mark (!) means NOT \n")


zTrueIndex <- which(z == TRUE) 
x[zTrueIndex]
y[zTrueIndex]
x[zTrueIndex] + y[zTrueIndex]

xy <- data.frame(x, y, z)
xy
colnames(xy)
dim(xy)
fix(xy)

xy[1,]
xy[ ,2]
xy[1:3, ]
xy[c(1,4,6), ]
xy[xy$z, ]
xy[!xy$z, ]
xy[ , -3]
xy[-c(1,2), ]
xy[ , c("x","y")]
xy[ , c("z")]
xy[["z"]]


newx <- xy[ , "x"]
all.equal(newx, x)
identical(newx, x)


subset(xy, subset= x < 3)
xy[ x<3, ]
xy[ x<3 & z == TRUE, ]
xy[ x<3 & z, ]
xy[ x<3 & !z, ]



xs1 <- seq(5, 20, by=1)
xs2 <- seq(5, 20, length.out=16)
xs1 == xs2
identical(xs1, xs2)

xs3 <- 5:20
xs1 == xs3
identical(xs1, xs3) ##hmmm. huh?
is.vector(xs3)
is.vector(xs1)
dim(xs1)
dim(xs3)
all.equal(xs1, xs3) #ok!
identical(xs1, xs3) ##hmmm. puzzler
which( xs1 != xs3 )
which( xs1 == xs3 )

for( i in seq_along(xs3)){
  print(xs1[i]-xs3[i])
}
## hmmm. again
is.integer(xs1)
is.numeric(xs1)
is.numeric(xs3)
is.integer(xs3) ## aha!

identical(xs3, as.integer(xs1)) ##well. maybe

## appears bugish
## > identical(seq(1,10), 1:10)
## [1] TRUE
## > identical(seq(1L, 10L,  by=1), 1:10)
## [1] FALSE
## > identical(seq(1,10,  by=1), 1:10)
## [1] FALSE
## > identical(seq(1L, 10L,  by=1L), 1:10)
## [1] TRUE

x1 <- c(1,1,1,1,1,1,1,1,1,1)
x2 <- rep(1, times = 10)
x2
x2 <- rep(1, 10)
x1 == x2
identical(x1, x2)



rep(c(1, 5, 8), length.out=9)

rep(c(1, 5, 8), each=3)

rep(c(1, 5, 8), each=3, length.out=9)

rep(c(1, 5, 8), each=3, length.out=10)

rep(c(1, 5, 8), times=3)

rep(c(1, 5, 8), each=3, times=3)
