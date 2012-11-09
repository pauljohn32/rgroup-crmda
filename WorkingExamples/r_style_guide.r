## Title: R Style-Guide
## Author: Aaron Boulton <aboulton@ku.edu>
## Date posted: 02/07/12
## Depends: None
## Description: The purpose of this guide is to familiarize
## the reader with R coding standards/conventions. 
## ----------------------------------------------------------------------------------------------------

## Note: The above heading is the template for working examples
## created by the R-Workgroup at the Center for Research Methods
## and Data Analysis (CRMDA), located at the University of Kansas

## Following R programming style conventions can be difficult.
## It is always recommended to follow the R Core Development Team's style
## conventions. However, there is no published R style guide from the Core
## Development Team. The closest they have come to publishing sytle guidelines
## is found in a document on the CRAN server that is a guide to R internal
## structures (p. 51):

browseURL("http://cran.r-project.org/doc/manuals/R-ints.pdf")

## Basically, they provide a configuration file for ESS in Emacs to set the
## recommended indentation of 4 for R code and 2 for help documentation
## (in Rd format). So, we know what indentation they prefer. The rest is
## up to us to decipher. There are three "unofficial" R-style guides that are
## often cited on message boards. These are:

## The Google R style guide

browseURL("http://google-styleguide.googlecode.com/svn/trunk
	/google-r-style.html")

## The "Hadley" style guide

browseURL("http://had.co.nz/stat405/resources/r-style-guide.html")

## The "Bengtsson" style guide

browseURL("https://docs.google.com/View?id=dddzqd53_2646dcw759cb")

## As you can imagine, having three "unofficial" style guides in common use that
## are not endorsed by the R Core Development Team can create problems. If you
## don't believe me, read this thread from the r-help list (it's actually very
## informative, but you can see there are several differing opinions for some
## issues):

browseURL("http://r.789695.n4.nabble.com/Google-s-R-Style-Guide-td901694.html")

## RULES
 
## The following conventions I describe overlap between (1) Code written
## by R-Core developers (2) The three unofficial style guides. Thus, these
## can be safely considered as rules.

## FILE NAMES
## Always use the .r extension for r files. Also, the file name chosen should be meaningful

## GOOD

r_style_guide.R

## BAD

rsg.txt

## ASSIGNMENT
## Always use the assignment operator "<-" instead of "=" when making an object assignment:

## GOOD

x <- 5

## BAD

x = 5

## SPACING
## There should always be a space between binary operators (=, +, -, <-, etc.):

## GOOD

x <- (2 + 7) / (3 - 7)

## BAD

x=(2+7)/(3-7)

## COMMAS
## There should always be a space after a comma, never before. This is often violated in function
## calls:

## GOOD

m <- matrix(c(1:10), nrow = 2, ncol = 5)

## BAD

m=matrix(c(1:10),nrow=2,ncol=5)

## PARENTHESES
## There should always be a space before the left parenthesis except when making a function call.
## Also, there should be no space after the left parenthesis or before the right parenthesis unless
## there is a trailing comma in the function call:

## GOOD

x <- mean(c(1:10))
if (x == 5.5) {
	message("No space after function call")
}

# BAD

x<-mean ( c( 1:10 ) )
if( x == 5.5 ) {message( "No space after function calls" )}

## CURLY BRACES
## The opening curly brace should never be on its own line.

## GOOD

y <- rep(1, 10, NA)
for (i in 1:10) {
	y[i] <- i ^ 2
}

## BAD
y <- rep(1, 10, NA)
for (i in 1:10)
{
	y[i] <- i ^ 2
}

## ELSE STATEMENTS
## Within an 'if' statement, any 'else' statements must be on the same line as
## the closing bracket of the if statement. Although giving the closing bracket
## may seem better and more like other languages, this can cause an error in R
## under some circumstances

## GOOD
y <- rep(1, 10, NA)
for (i in 1:10) {
	if (i < 5) {
		y[i] <- i ^ 2
	} else {
		y[i] <- i ^ 3
	}
}

## BAD
y <- rep(1, 10, NA)
for (i in 1:10) {
	if (i < 5) {
		y[i] <- i ^ 2
	}
	else {
		y[i] <- i ^ 3
	}
}

## STILL UP FOR DEBATE

## INDENTATION
## RECOMMENDATION: 2 or 4 characters
## As indicated previously, the R Core Developers recommend an indentation of 4 characters for code
## and 2 for the .Rd format (help files). The Google and Hadley recommendations suggest 2 characters
## for indentation to "reduce the risk of line wrapping". Bengtsson rides the fence somewhat but
## settles on 2. Personally, I like 2 characters. What appears to be agreed upon is that you should
## use no less than 2 and no more than four lines. Also, 3 appears uncommon enough that one should
## either go with 2 or 4.

## GOOD

x <- rep(1, 10, NA)
for (i in 1:10) {
	x[i] <- i + 1
	if (x[i] == 6) {
	  x[i] <- 0
	}
}

## ALSO GOOD

x <- rep(1, 10, NA)
for (i in 1:10) {
    x[i] <- i + 1
	  if (x[i] == 6) {
	      x[i] <- 0
	  }
}

## BAD

x <- rep(1, 10, NA)
for (i in 1:10) {
x[i] <- i + 1
if (x[i] == 6) {
x[i] <- 0
}
}

x <- rep(1, 10, NA)
for (i in 1:10) {
			x[i] <- i + 1
						if (x[i] == 6) {
									x[i] <- 0
						}
}

## SEMICOLONS
## RECOMMENDATION: Don't use
## Semicolons can be used to end a line thus allowing several commands to be executed on a single
## line. The Google and Hadley guidelines discourage their use, and it appears the R Core Developers
## do not use semicolons. Bengtsson, on the other hand, recommends liberal use of the semicolon so
## that line ambiguity does not exist. Personally, I think it makes the code ugly, encourages
## more than one command per line, can easily screw up loops and conditions, and really, if you are
## running into line ambiguity then your code needs more work then just semicolons. Bengtsson even
## acknowledges he's going out on a limb here, and thus I would recommend limiting or elinamting
## their use:

## GOOD

x <- 60
y <- 70
z <- x + 2 * y

## BAD

x <- 60;
y <- 70;
z <- x + 2*y;

x <- 60; y <- 70; z <- x + 2*y;

## Aaron to-do:
## Naming conventions for variables/functions/etc.
## Add to parentheses section re: if, for, etc.