
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






##inner product
x %*% y
t(x) %*% y
y %*% x

outer(x,y)
outer(x,y, FUN="*")
outer(x,y, FUN="+")
outer(x,y, FUN="/")
outer(x,y, FUN=function(a, b) {b - 2*a})

xym <- as.matrix(xy)
xym
crossprod(xy,xy)
