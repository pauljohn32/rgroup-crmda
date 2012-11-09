## Paul Johnson
## 2011-10-07

In 2010, I was invited to make a presentation about R to the Sociology
department. Their faces were as blank as you could possibly imagine,
from which I concluded that I made a weak performance.

Nevertheless, I've looked at this again and I still think it is
pretty good, so perhaps other people who need to make a presentation
about R can use this as a starting point.

Just a format warning.

In the best of all possible worlds, the LaTeX document would contain
all the R code and so the two pieces--the stat work and the document--
could be processed at the same time.  In this case, I had some graphs
and screen shots that I wanted to  include, although they were not
generated inside the document. The "figs" directory contains those
included images.

There were some illustrations that I wrote up as separate files 
(look under auxiliary in this folder) and I left them separate,
but just included the results here.  

This was the first document in which I tried to Sweave a
Beamer presentation. I decided to  
use an R "branch" to isolate the R code in the document, and then
save the results and insert them into the document so that I could
turn off the R branch and just produce the document without using
R and Sweave at all. I think that's a great feature, because
people who get this LaTeX document should be able to work with it
in LyX even if they don't have a working Sweave setup.

Like all first tries, this is not the best setup, and it may be
that I come back and work on it to make it more consistent with
the approach I've been using while I make course lectures.  Those
are the "lecture" files I upload under 

http://pj.freefaculty.org/guides/stat/Regression

PJ
