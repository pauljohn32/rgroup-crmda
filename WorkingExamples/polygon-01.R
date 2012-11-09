### Paul Johnson Jun 7, 2010

### For demonstration of polygon and "coloring in"
### the area undre part of a curve. 


### Suppose we plot a curve f(x) and then want to "color in"
### a section under the curve from x=5 to x=13.

### For input, polygon needs 2 columns of points, and they have to
### trace out the coordinates of the outside of the highlighted area.


###   Here's the ASCII art of the century


    ##             wind around top
    ##                  |
    ##                  |
    ##                  |  ________ f(13)
    ##                  v /      |
    ##             ______/       |    <-- then down
    ##       f(5) /              |
    ##         __/|              |
    ##      ______|______________|________
    ##            x=5           x=13
    ##          /              \
    ##         /                \
    ##       Start Here       finish last leg



### Begin at a point, say (x=5, y=0).  From there, let it "go up" to
### (x=5, y=f(5)). Then wind to the right, tracing out the values of
### f(x) from x=5 to x=13.  The trace stops at (x=13, y=f(13)).  Then
### vertically go down to (x=13, y=0), and then "all along" x back to
### (x=5, y=0).




SAVEFIG <- FALSE

### To save into file, uncomment following and run again
### SAVEFIG <- TRUE



myfun <- function (x) {
    y <- 35 + 9.5*sin(x ) + 19 * sin( 0.4*x) 
    y
}


myx <- seq(1,50, by=0.2)

myy <- myfun(myx)

### put it into data frame

mydf <- data.frame(myx,myy)

### remove variables, keep tidy!

rm(myx, myy)


if (SAVEFIG == T) pdf(file="poly-01.pdf", onefile=F, paper="special",height=6, width=6,family="Times")


### Just take the default plot
plot(myy ~ myx, data=mydf, type="l")

### Goal. Pick 2 points, x=a and x=b, then plot the area
### under curve between a, b.

a <- 5
b <- 13

mysmalldf <- mydf[mydf$myx >= a & mydf$myx <= b ,]

### Look at that:
## > mysmalldf
##     myx      myy

## 21  5.0 43.166871
## 22  5.2 43.196707
## 23  5.4 43.455023
## 24  5.6 43.904970
## 25  5.8 44.498677
## 26  6.0 45.179353
## 27  6.2 45.883762
## 28  6.4 46.544971
## 29  6.6 47.095273
## 30  6.8 47.469146
## 31  7.0 47.606148
## 32  7.2 47.453612
## 33  7.4 46.969056
## 34  7.6 46.122179
## 35  7.8 44.896390
## 36  8.0 43.289795
## 37  8.2 41.315589
## 38  8.4 39.001863
## 39  8.6 36.390804
## 40  8.8 33.537335
## 41  9.0 30.507237
## 42  9.2 27.374837
## 43  9.4 24.220341
## 44  9.6 21.126919
## 45  9.8 18.177669
## 46 10.0 15.452552
## 47 10.2 13.025453
## 48 10.4 10.961454
## 49 10.6  9.314441
## 50 10.8  8.125141
## 51 11.0  7.419654
## 52 11.2  7.208551
## 53 11.4  7.486565
## 54 11.6  8.232888
## 55 11.8  9.412062
## 56 12.0 10.975430
## 57 12.2 12.863085
## 58 12.4 15.006247
## 59 12.6 17.329957
## 60 12.8 19.756004
## 61 13.0 22.205948




## It is almost all we need.  On front of stack of points, add
## (a, 0)

## That is the starting point of the polygon.

## From there, the sequence of points goes up to (14.0, 32.41),
## and then traces to right, stops at (21.0, 59.18).

## On bottom of stack of points, add

## (b, 0)
## (a, 0)


mysmalldf <- rbind( c(a, 0), mysmalldf)


mysmalldf <- rbind(mysmalldf, c(b,0), c(a,0))

polygon(mysmalldf$myx, mysmalldf$myy, density=c(60),col="gray60",border="red",lwd=1.5)


### Beautify to suit your taste!
dev.off()
